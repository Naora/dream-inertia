open Lwt.Syntax

type prop =
  { name : string
  ; resolver : unit -> Yojson.Safe.t Lwt.t
  ; is_mergeable : bool
  }

type deferred =
  { prop : prop
  ; group : string
  }

type version = string option

module type CONFIG = sig
  val render : head:string -> app:string -> string
  val version : unit -> version
  val shared : Dream.request -> prop list option
end

module type INERTIA = sig
  val render
    :  component:string
    -> ?props:prop list
    -> ?deferred:deferred list
    -> Dream.request
    -> Dream.response Lwt.t

  val location : Dream.request -> string -> Dream.response Lwt.t
end

module Page_object = struct
  type t =
    { component : string
    ; shared : prop list option
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

  let all_props t =
    Lwt_list.map_p
      (fun { name; resolver; _ } ->
        let* v = resolver () in
        Lwt.return (name, v))
      t.props
  ;;

  let find_props_by_key t keys =
    let deferred_prop = List.map (fun d -> d.prop) t.deferred in
    let all_props = List.append t.props deferred_prop in
    Lwt_list.filter_map_p
      (fun { name; resolver; _ } ->
        if List.exists (fun l -> l = name) keys
        then
          let* v = resolver () in
          Lwt.return_some (name, v)
        else Lwt.return_none)
      all_props
  ;;

  let merge_shared_props t =
    match t.shared with
    | Some s ->
      let props =
        List.sort_uniq (fun a b -> String.compare a.name b.name) (t.props @ s)
      in
      { t with props }
    | None -> t
  ;;

  let props_to_json t keys =
    let t = merge_shared_props t in
    let* props =
      match keys with
      | All -> all_props t
      | Partial keys -> find_props_by_key t keys
    in
    Lwt.return @@ `Assoc props
  ;;

  let get_deferred_props_by_group t =
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

  let get_mergeable_props t =
    let p =
      List.filter_map
        (fun { name; is_mergeable; _ } ->
          if is_mergeable then Some (`String name) else None)
        t.props
    in
    let d =
      List.filter_map
        (fun { prop = { name; is_mergeable; _ }; _ } ->
          if is_mergeable then Some (`String name) else None)
        t.deferred
    in
    `List (p @ d)
  ;;

  let version_to_json t =
    t.version |> Option.map (fun v -> `String v) |> Option.value ~default:`Null
  ;;

  (* TODO: essayer d'en faire un pipeline ici sera certainement plus simple a lire *)
  let to_json t keys =
    let* props = props_to_json t keys in
    let json =
      [ "component", `String t.component
      ; "props", props
      ; "url", `String t.url
      ; "version", version_to_json t
      ; "mergeProps", get_mergeable_props t
      ]
    in
    let json =
      match keys with
      | All ->
        let deferred_props = get_deferred_props_by_group t in
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
    | Inertia_partial_request of partial_reload_data

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
      Inertia_partial_request { requested_keys; component }
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
    let app =
      Dream.html_escape @@ Fmt.str {html|<div id="app" data-page='%s'></div> |html} resp
    in
    let head = "<!-- inertia head -->" in
    Dream.html @@ Config.render ~app ~head
  ;;

  let respond po request =
    match request_kind request with
    | Initial_load -> respond_with_html po
    | Inertia_request -> respond_with_json po All
    | Inertia_partial_request { component; requested_keys } ->
      if component <> po.component
      then respond_with_json po All
      else respond_with_json po (Partial requested_keys)
  ;;

  let render ~component ?(props = []) ?(deferred = []) request =
    let po =
      Page_object.
        { component
        ; shared = Config.shared request
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

  let location request target =
    match request_kind request with
    | Initial_load -> Dream.redirect request target
    | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
  ;;
end

let prop name ?(merge = false) resolver = { name; is_mergeable = merge; resolver }

let defer name ?(merge = false) ?(group = "default") resolver =
  let prop = prop name ~merge resolver in
  { prop; group }
;;
