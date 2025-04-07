type props = Yojson.Safe.t
type version = string option

module type CONFIG = sig
  val render : head:string -> app:string -> string
  val version : unit -> version
end

module type INERTIA = sig
  val render : component:string -> props:props -> Dream.request -> Dream.response Lwt.t
end

module Page_object = struct
  type t =
    { component : string
    ; props : props
    ; url : string
    ; version : version
    }

  let is_version_stale t request =
    match Dream.header request "X-Inertia-Version", t.version with
    | Some rv, Some pv -> rv <> pv
    | _, _ -> false
  ;;

  let partial_reload t ~component ~requested_keys =
    match component with
    | ca when ca = t.component ->
      let props =
        match t.props with
        | `Assoc current_props ->
          let filtered_props =
            List.filter (fun (k, _) -> List.mem k requested_keys) current_props
          in
          `Assoc filtered_props
        | _ -> t.props
      in
      { t with props }
    | _ -> t
  ;;

  let to_json { component; props; url; version } =
    let v = version |> Option.map (fun v -> `String v) |> Option.value ~default:`Null in
    `Assoc
      [ "component", `String component; "props", props; "url", `String url; "version", v ]
  ;;

  let to_string t =
    let y = to_json t in
    Yojson.Safe.to_string y
  ;;
end

module Make (Config : CONFIG) : INERTIA = struct
  type partial_reload_data =
    { requested_keys : string list
    ; component : string
    }

  and request_kind =
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

  let respond_conflict url =
    let headers = [ "X-Inertia-Location", url ] in
    Dream.respond ~status:`Conflict ~headers ""
  ;;

  let respond po request =
    let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
    match request_kind request with
    | Initial_load ->
      let resp = Page_object.to_string po in
      let app = Fmt.str {html|<div id="app" data-page='%s'></div> |html} resp in
      let head = "<!-- inertia head -->" in
      Dream.respond @@ Config.render ~app ~head
    | Inertia_request -> Dream.json ~headers @@ Page_object.to_string po
    | Intertia_partial_request { component; requested_keys } ->
      let po = Page_object.partial_reload po ~component ~requested_keys in
      Dream.json ~headers @@ Page_object.to_string po
  ;;

  let render ~component ~props request =
    let po =
      Page_object.
        { component; props; url = Dream.target request; version = Config.version () }
    in
    match Page_object.is_version_stale po request, Dream.method_ request with
    | true, `GET -> respond_conflict po.url
    | _, _ -> respond po request
  ;;
end
