open Lwt.Syntax

type t =
  { component : string
  ; props : Prop.t list
  ; url : string
  ; version : string option
  ; clear_history : bool
  ; encrypt_history : bool
  }

type requested_keys =
  | All
  | Partial of string list

let json_of_props ~keys t =
  let* props =
    match keys with
    | All -> Prop.resolve_props t.props
    | Partial keys -> Prop.resolve_props_by_keys ~keys t.props
  in
  Lwt.return @@ `Assoc props
;;

let json_of_deferred_props_by_group t =
  let groups = Prop.deferred_props_by_group t.props in
  let g = List.map (fun (g, ns) -> g, `List (List.map (fun n -> `String n) ns)) groups in
  `Assoc g
;;

let json_of_mergeable_props t =
  let m, d = Prop.mergeable_props t.props in
  `List m, `List d
;;

let json_of_version t =
  t.version |> Option.map (fun v -> `String v) |> Option.value ~default:`Null
;;

let to_json ~keys t =
  let* props = json_of_props ~keys t in
  let merge_props, deep_merge_props = json_of_mergeable_props t in
  let deferred_props =
    match keys with
    | All -> json_of_deferred_props_by_group t
    | Partial _ -> `Assoc []
  in
  let json =
    [ "component", `String t.component
    ; "props", props
    ; "url", `String t.url
    ; "version", json_of_version t
    ; "mergeProps", merge_props
    ; "deepMergeProps", deep_merge_props
    ; "deferredProps", deferred_props
    ; "clearHistory", `Bool t.clear_history
    ; "encryptHistory", `Bool t.encrypt_history
    ]
  in
  Lwt.return (`Assoc json)
;;

let to_string ~keys t =
  let* y = to_json ~keys t in
  Lwt.return (Yojson.Safe.to_string y)
;;

let respond_with_conflict url =
  let headers = [ "X-Inertia-Location", url ] in
  Dream.respond ~status:`Conflict ~headers ""
;;

let respond_with_json ~keys t =
  let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
  let* json = to_string ~keys t in
  Dream.json ~headers json
;;

let respond_with_html ~context t =
  let* app = to_string ~keys:All t in
  context |> Context.template |> Template.render { app } |> Dream.html
;;

let respond ~context t =
  match Context.request_kind context with
  | Initial_load -> respond_with_html ~context t
  | Inertia_request -> respond_with_json ~keys:All t
  | Inertia_partial_request { component; requested_keys } ->
    if component <> t.component
    then respond_with_json ~keys:All t
    else respond_with_json ~keys:(Partial requested_keys) t
;;

let location request target =
  match Context.of_request request |> Context.request_kind with
  | Initial_load -> Dream.redirect request target
  | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
;;

let is_version_stale request version =
  match Dream.header request "X-Inertia-Version", version with
  | Some rv, Some pv -> rv <> pv
  | _, _ -> false
;;

let render ~component ?(props = []) ?(clear_history = false) request =
  let context = Context.of_request request in
  let version = Context.version context in
  let props =
    Context.shared_props context
    |> Option.map (fun s -> Prop.merge_props ~from:s ~into:props)
    |> Option.value ~default:props
  in
  let url = Dream.target request in
  match is_version_stale request version, Dream.method_ request with
  | true, `GET -> respond_with_conflict url
  | _, _ ->
    respond
      ~context
      { component
      ; props
      ; url
      ; version
      ; clear_history
      ; encrypt_history = context.encrypt_history
      }
;;
