open Lwt.Syntax

type prop =
  { name : string
  ; resolver : unit -> Yojson.Safe.t Lwt.t
  }

type deferred =
  { prop : prop
  ; group : string
  }

type version = string option

module type CONFIG = sig
  val render : head:string -> app:string -> string
  val version : unit -> version
end

module type INERTIA = sig
  val render
    :  component:string
    -> ?props:prop list
    -> ?deferred:deferred list
    -> Dream.request
    -> Dream.response Lwt.t

  val prop : string -> (unit -> Yojson.Safe.t Lwt.t) -> prop
  val deferred : string -> ?group:string -> (unit -> Yojson.Safe.t Lwt.t) -> deferred
end

module Page_object = struct
  type t =
    { component : string
    ; props : prop list
    ; deferred : deferred list
    ; url : string
    ; version : version
    }

  type requested_keys =
    | All
    | Partial of string list

  let is_version_stale t request =
    match Dream.header request "X-Inertia-Version", t.version with
    | Some rv, Some pv -> rv <> pv
    | _, _ -> false
  ;;

  let props_to_json t keys =
    let* props =
      match keys with
      | All ->
        Lwt_list.map_p
          (fun { name; resolver } ->
            let* v = resolver () in
            Lwt.return (name, v))
          t.props
      | Partial keys ->
        let deferred_prop = List.map (fun d -> d.prop) t.deferred in
        let all_props = List.append t.props deferred_prop in
        Lwt_list.filter_map_p
          (fun { name; resolver } ->
            if List.exists (fun l -> l = name) keys
            then
              let* v = resolver () in
              Lwt.return_some (name, v)
            else Lwt.return_none)
          all_props
    in
    Lwt.return (`Assoc props)
  ;;

  let deferred_props_by_group t =
    let groups = Hashtbl.create @@ List.length t.deferred in
    List.iter
      (fun { prop = { name; _ }; group } ->
        match Hashtbl.find_opt groups group with
        | None -> Hashtbl.add groups group [ name ]
        | Some g -> Hashtbl.replace groups group (name :: g))
      t.deferred;
    let g =
      Hashtbl.fold
        (fun name props acc ->
          let p = List.map (fun p -> `String p) props in
          (name, `List p) :: acc)
        groups
        []
    in
    `Assoc g
  ;;

  let to_json t keys =
    let v = t.version |> Option.map (fun v -> `String v) |> Option.value ~default:`Null in
    let* props = props_to_json t keys in
    let json =
      [ "component", `String t.component
      ; "props", props
      ; "url", `String t.url
      ; "version", v
      ]
    in
    let json =
      match keys with
      | All ->
        let deferred_props = deferred_props_by_group t in
        ("deferredProps", deferred_props) :: json
      | Partial _ -> json
    in
    Lwt.return (`Assoc json)
  ;;

  let to_string t keys =
    let* y = to_json t keys in
    Lwt.return (Yojson.Safe.to_string y)
  ;;
end

module Make (Config : CONFIG) : INERTIA = struct
  type partial_reload_data =
    { requested_keys : string list
    ; component : string
    }

  type request_kind =
    | Initial_load
    | Inertia_request
    | Intertia_partial_request of partial_reload_data

  let get_data_keys data_keys =
    String.split_on_char ',' data_keys
    |> List.filter_map (fun s ->
      match s |> String.trim with
      | "" -> None
      | _ as r -> Some r)
  ;;

  let request_kind request =
    let h = Dream.header request in
    match h "X-Inertia", h "X-Inertia-Partial-Data", h "X-Inertia-Partial-Component" with
    | Some "true", Some keys, Some component ->
      let requested_keys = get_data_keys keys in
      Intertia_partial_request { requested_keys; component }
    | Some "true", _, _ -> Inertia_request
    | _, _, _ -> Initial_load
  ;;

  let respond_with_conflict url =
    let headers = [ "X-Inertia-Location", url ] in
    Dream.respond ~status:`Conflict ~headers ""
  ;;

  let respond_with_json po keys =
    let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
    let* json = Page_object.to_string po keys in
    Dream.json ~headers json
  ;;

  let respond_with_html po =
    let* resp = Page_object.to_string po All in
    let app = Fmt.str {html|<div id="app" data-page='%s'></div> |html} resp in
    let head = "<!-- inertia head -->" in
    Dream.respond @@ Config.render ~app ~head
  ;;

  let respond po request =
    match request_kind request with
    | Initial_load -> respond_with_html po
    | Inertia_request -> respond_with_json po All
    | Intertia_partial_request { component; requested_keys } ->
      if component <> po.component
      then respond_with_html po
      else respond_with_json po (Partial requested_keys)
  ;;

  let render ~component ?(props = []) ?(deferred = []) request =
    let po =
      Page_object.
        { component
        ; props
        ; deferred
        ; url = Dream.target request
        ; version = Config.version ()
        }
    in
    match Page_object.is_version_stale po request, Dream.method_ request with
    | true, `GET -> respond_with_conflict po.url
    | _, _ -> respond po request
  ;;

  let prop name resolver = { name; resolver }
  let deferred name ?(group = "default") resolver = { prop = { name; resolver }; group }
end
