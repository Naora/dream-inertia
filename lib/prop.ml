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

let create ?(merge = No_merge) ?(load = Default) name resolver =
  { name; resolver; merging_mode = merge; loading_mode = load }
;;

let pp_merge_kind ppf = function
  | No_merge -> Fmt.pf ppf "no merge"
  | Merge -> Fmt.pf ppf "merge"
  | Deep_merge -> Fmt.pf ppf "deep merge"
;;

let pp_loading_kind ppf = function
  | Default -> Fmt.pf ppf "default"
  | Defer group -> Fmt.pf ppf "defer (%s)" group
  | Always -> Fmt.pf ppf "always"
  | Optional -> Fmt.pf ppf "optional"
;;

let pp ppf prop =
  Fmt.pf
    ppf
    "{name = %s; merging_mode = %a; loading_mode = %a}"
    prop.name
    pp_merge_kind
    prop.merging_mode
    pp_loading_kind
    prop.loading_mode
;;

let resolve_props props =
  Lwt_list.filter_map_p
    (fun t ->
      match t.loading_mode with
      | Optional | Defer _ -> Lwt.return_none
      | Always | Default ->
        let* v = t.resolver () in
        Lwt.return_some (t.name, v))
    props
;;

let resolve_props_by_keys ~keys props =
  Lwt_list.filter_map_p
    (fun t ->
      if List.exists (fun l -> l = t.name || t.loading_mode = Always) keys
      then
        let* v = t.resolver () in
        Lwt.return_some (t.name, v)
      else Lwt.return_none)
    props
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
