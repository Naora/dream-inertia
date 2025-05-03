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

let name prop = prop.name
let loading_mode prop = prop.loading_mode
