type t =
  { inertia_mode : request_kind
  ; encrypt_history : bool
  ; shared : Prop.t list option
  }

and partial_reload_data =
  { requested_keys : string list
  ; component : string
  }

and request_kind =
  | Initial_load
  | Inertia_request
  | Inertia_partial_request of partial_reload_data

let pp_partial_reload_data ppf p =
  Fmt.pf
    ppf
    "{component = %s; requested_keys = %a}"
    p.component
    (Fmt.list Fmt.string)
    p.requested_keys
;;

let string_of_request_kind = function
  | Initial_load -> "initial load"
  | Inertia_request -> "inertia request"
  | Inertia_partial_request p ->
    Fmt.str "inertia partial request (%a)" pp_partial_reload_data p
;;

let pp ppf c =
  Fmt.pf
    ppf
    "{inertia_mode = %s; encrypt_history = %b; shared = %a}"
    (string_of_request_kind c.inertia_mode)
    c.encrypt_history
    (Fmt.option (Fmt.list Prop.pp))
    c.shared
;;

let field = Dream.new_field ~name:"context" ~show_value:(fun c -> Fmt.str "%a" pp c) ()

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

let create request props =
  let inertia_mode = request_kind request in
  { encrypt_history = false; inertia_mode; shared = props }
;;

let of_request request = Dream.field request field |> Option.get

let request_kind request =
  let context = of_request request in
  context.inertia_mode
;;

let shared_props request =
  let context = of_request request in
  context.shared
;;

let set_shared_props request props =
  let context = of_request request in
  Dream.set_field request field { context with shared = Some props }
;;

let set_encrypt_history request encrypt_history =
  let context = of_request request in
  Dream.set_field request field { context with encrypt_history }
;;
