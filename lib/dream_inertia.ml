open Lwt.Syntax

module type CONFIG = sig
  val render : head:string -> app:string -> string
  val version : unit -> Page_object.version
end

module type INERTIA = sig
  val render
    :  component:string
    -> ?props:Prop.t list
    -> ?clear_history:bool
    -> Dream.request
    -> Dream.response Lwt.t

  val location : Dream.request -> string -> Dream.response Lwt.t

  val prop
    :  ?merge:Prop.merge_kind
    -> ?load:Prop.loading_kind
    -> string
    -> Prop.resolver
    -> Prop.t

  val defer : ?group:string -> ?merge:Prop.merge_kind -> string -> Prop.resolver -> Prop.t
  val encrypt_history : Dream.middleware
  val shared_props : Prop.t list -> Dream.middleware
  val inertia : Dream.middleware
end

module Make (Config : CONFIG) : INERTIA = struct
  type partial_reload_data =
    { requested_keys : string list
    ; component : string
    }

  (*TODO: bouger ca dans le middleware. Permet de reduire le nombre d'evaluation de request_kind *)
  type request_kind =
    | Initial_load
    | Inertia_request
    | Inertia_partial_request of partial_reload_data

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

  let respond_with_conflict url =
    let headers = [ "X-Inertia-Location", url ] in
    Dream.respond ~status:`Conflict ~headers ""
  ;;

  let respond_with_json po keys =
    let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
    let* json = Page_object.to_string po keys in
    Dream.json ~headers json
  ;;

  let respond_with_html po =
    let* app = Page_object.to_string po All in
    let head = "<!-- inertia head -->" in
    Dream.html @@ Config.render ~app ~head
  ;;

  let respond po request =
    match request_kind request with
    | Initial_load -> respond_with_html po
    | Inertia_request -> respond_with_json po All
    | Inertia_partial_request { component; requested_keys } ->
      if component <> po.component
      then respond_with_json po All
      else respond_with_json po (Partial requested_keys)
  ;;

  let location request target =
    match request_kind request with
    | Initial_load -> Dream.redirect request target
    | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
  ;;

  let prop ?(merge = Prop.No_merge) ?(load = Prop.Default) name resolver =
    Prop.{ name; merging_mode = merge; resolver; loading_mode = load }
  ;;

  let defer ?(group = "default") = prop ~load:(Defer group)

  let shared_props_field =
    Dream.new_field
      ~name:"shared_props"
      ~show_value:(fun p -> Fmt.str "%a" (Fmt.list Prop.pp) p)
      ()
  ;;

  let shared_props props inner request =
    Dream.set_field request shared_props_field props;
    inner request
  ;;

  let encrypt_history_field =
    Dream.new_field ~name:"encrypt_history" ~show_value:(fun f -> string_of_bool f) ()
  ;;

  let encrypt_history inner request =
    Dream.set_field request encrypt_history_field true;
    inner request
  ;;

  let render ~component ?(props = []) ?(clear_history = false) request =
    let encrypt_history =
      Dream.field request encrypt_history_field |> Option.value ~default:false
    in
    let props =
      Dream.field request shared_props_field
      |> Option.map (fun s -> Prop.merge_props ~from:s ~into:props)
      |> Option.value ~default:props
    in
    let po =
      Page_object.
        { component
        ; props
        ; url = Dream.target request
        ; version = Config.version ()
        ; clear_history
        ; encrypt_history
        }
    in
    match Page_object.is_version_stale po request, Dream.method_ request with
    | true, `GET -> respond_with_conflict po.url
    | _, _ -> respond po request
  ;;

  let inertia inner request =
    let xsrf = Dream.header request "X-XSRF-TOKEN" in
    let* response =
      match xsrf with
      | Some token ->
        let* valid = Dream.verify_csrf_token request token in
        (match valid with
         | `Expired _ | `Wrong_session | `Invalid -> Dream.respond ~code:419 ""
         | `Ok -> inner request)
      | None -> inner request
    in
    let new_token = Dream.csrf_token request in
    Dream.set_cookie response request "XSRF-TOKEN" new_token;
    Lwt.return response
  ;;
end
