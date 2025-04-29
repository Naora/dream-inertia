type t =
  { inertia_mode : request_kind
  ; encrypt_history : bool
  ; shared : Prop.t list option
  ; version : string option
  ; render_template : page_data -> string
  }

and partial_reload_data =
  { requested_keys : string list
  ; component : string
  }

and request_kind =
  | Initial_load
  | Inertia_request
  | Inertia_partial_request of partial_reload_data

and page_data = { app : string }

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

let create ~encrypt_history ~request ~props ~version ~renderer =
  let get_data_keys data_keys =
    String.split_on_char ',' data_keys
    |> List.filter_map (fun s ->
      match String.trim s with
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
  Dream.set_field
    request
    field
    { encrypt_history; inertia_mode; shared = props; version; render_template = renderer }
;;

(* TODO: raise proper error *)
let of_request request = Dream.field request field |> Option.get
let update request t = Dream.set_field request field t
let request_kind t = t.inertia_mode
let shared_props t = t.shared
let set_shared_props props t = { t with shared = Some props }
let encrypt_history t = t.encrypt_history
let set_encrypt_history encrypt_history t = { t with encrypt_history }
let render page_data t = t.render_template page_data
let app page_data = page_data.app
