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

let all_props t = Lwt_list.filter_map_p Prop.resolve_prop t.props
let find_props_by_key keys t = Lwt_list.filter_map_p (Prop.resolve_partial keys) t.props

let json_of_props keys t =
  let* props =
    match keys with
    | All -> all_props t
    | Partial keys -> find_props_by_key keys t
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

let to_json keys t =
  let* props = json_of_props keys t in
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

let to_string keys t =
  let* y = to_json keys t in
  Lwt.return (Yojson.Safe.to_string y)
;;

let respond_with_conflict url =
  let headers = [ "X-Inertia-Location", url ] in
  Dream.respond ~status:`Conflict ~headers ""
;;

let respond_with_json keys t =
  let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
  let* json = to_string keys t in
  Dream.json ~headers json
;;

let respond_with_html render t =
  let* app = to_string All t in
  let head = "<!-- inertia head -->" in
  Dream.html @@ render ~head ~app
;;

let respond ~render request t =
  match Context.request_kind request with
  | Initial_load -> respond_with_html render t
  | Inertia_request -> respond_with_json All t
  | Inertia_partial_request { component; requested_keys } ->
    if component <> t.component
    then respond_with_json All t
    else respond_with_json (Partial requested_keys) t
;;
