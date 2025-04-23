open Lwt.Syntax

type t =
  { name : string
  ; resolver : resolver
  ; merging_mode : merge_kind
  ; loading_mode : loading_kind
  }

and resolver = unit -> Yojson.Safe.t Lwt.t

and merge_kind =
  | No_merge
  | Merge
  | Deep_merge

and loading_kind =
  | Default
  | Defer of string
  | Always
  | Optional

let string_of_merging_kind = function
  | No_merge -> "no merge"
  | Merge -> "merge"
  | Deep_merge -> "deep merge"
;;

let string_of_loading_kind = function
  | Default -> "default"
  | Defer group -> "defer (" ^ group ^ ")"
  | Always -> "always"
  | Optional -> "optional"
;;

let pp ppf prop =
  Fmt.pf
    ppf
    "{name = %s; merging_mode = %s; loading_mode = %s}"
    prop.name
    (string_of_merging_kind prop.merging_mode)
    (string_of_loading_kind prop.loading_mode)
;;

let resolve_prop { name; resolver; loading_mode; _ } =
  match loading_mode with
  | Optional | Defer _ -> Lwt.return_none
  | Always | Default ->
    let* v = resolver () in
    Lwt.return_some (name, v)
;;

let resolve_partial keys { name; resolver; loading_mode; _ } =
  if List.exists (fun l -> l = name || loading_mode = Always) keys
  then
    let* v = resolver () in
    Lwt.return_some (name, v)
  else Lwt.return_none
;;

let rec merge_props ~from ~into =
  match from with
  | [] -> into
  | hd :: ta ->
    if List.exists (fun p -> hd.name = p.name) into
    then merge_props ~into ~from:ta
    else merge_props ~into:(hd :: into) ~from:ta
;;

let mergeable_props props =
  List.fold_left
    (fun (merge, deep_merge) { name; merging_mode; _ } ->
      match merging_mode with
      | No_merge -> merge, deep_merge
      | Merge -> `String name :: merge, deep_merge
      | Deep_merge -> merge, `String name :: deep_merge)
    ([], [])
    props
;;

let deferred_props_by_group props =
  let rec aux acc props =
    match props with
    | [] -> acc
    | { name; loading_mode; _ } :: t ->
      (match loading_mode with
       | Always | Optional | Default -> aux acc t
       | Defer group ->
         let new_acc =
           match List.assoc_opt group acc with
           | None -> (group, [ name ]) :: acc
           | Some p -> (group, name :: p) :: List.remove_assoc group acc
         in
         aux new_acc t)
  in
  aux [] props
;;
