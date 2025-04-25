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

let pp_request_kind ppf = function
  | Initial_load -> Fmt.pf ppf "initial load"
  | Inertia_request -> Fmt.pf ppf "inertia request"
  | Inertia_partial_request p ->
    Fmt.pf ppf "inertia partial request (%a)" pp_partial_reload_data p
;;

let pp ppf t =
  Fmt.pf
    ppf
    "{inertia_mode = %a; encrypt_history = %b; shared = %a}"
    pp_request_kind
    t.inertia_mode
    t.encrypt_history
    (Fmt.option (Fmt.list Prop.pp))
    t.shared
;;

let field = Dream.new_field ~name:"context" ~show_value:(fun c -> Fmt.str "%a" pp c) ()

let create request props =
  let get_data_keys data_keys =
    String.split_on_char ',' data_keys
    |> List.filter_map (fun s ->
      match s |> String.trim with
      | "" -> None
      | _ as r -> Some r)
  in
  let h = Dream.header request in
  let inertia_mode =
    match h "X-Inertia", h "X-Inertia-Partial-Data", h "X-Inertia-Partial-Component" with
    | Some "true", Some keys, Some component ->
      let requested_keys = get_data_keys keys in
      Inertia_partial_request { requested_keys; component }
    | Some "true", _, _ -> Inertia_request
    | _, _, _ -> Initial_load
  in
  Dream.set_field request field { encrypt_history = false; inertia_mode; shared = props }
;;

let get_context request = Dream.field request field |> Option.get

let request_kind request =
  let context = get_context request in
  context.inertia_mode
;;

let shared_props request =
  let context = get_context request in
  context.shared
;;

let set_shared_props request props =
  let context = get_context request in
  Dream.set_field request field { context with shared = Some props }
;;

let encrypt_history request =
  let context = get_context request in
  context.encrypt_history
;;

let set_encrypt_history request encrypt_history =
  let context = get_context request in
  Dream.set_field request field { context with encrypt_history }
;;
