open Lwt.Syntax

type t =
  { component : string
  ; props : Prop.t list
  ; url : string
  ; version : version
  ; clear_history : bool
  ; encrypt_history : bool
  }

and version = string option

type requested_keys =
  | All
  | Partial of string list

let is_version_stale t request =
  match Dream.header request "X-Inertia-Version", t.version with
  | Some rv, Some pv -> rv <> pv
  | _, _ -> false
;;

let all_props t = Lwt_list.filter_map_p Prop.resolve_prop t.props
let find_props_by_key t keys = Lwt_list.filter_map_p (Prop.resolve_partial keys) t.props

(* let merge_shared_props t =
  match t.shared with
  | Some s ->
    let props = Prop.merge_props ~from:s ~into:t.props in
    { t with props }
  | None -> t
;; *)

let json_of_props t keys =
  (*   let t = merge_shared_props t in *)
  let* props =
    match keys with
    | All -> all_props t
    | Partial keys -> find_props_by_key t keys
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

let to_json t keys =
  let* props = json_of_props t keys in
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

let to_string t keys =
  let* y = to_json t keys in
  Lwt.return (Yojson.Safe.to_string y)
;;
