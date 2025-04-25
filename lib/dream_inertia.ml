open Lwt.Syntax

module type CONFIG = sig
  val render : head:string -> app:string -> string
  val version : unit -> string option
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
  val inertia : ?props:Prop.t list -> Dream.middleware
end

module Make (Config : CONFIG) : INERTIA = struct
  let respond_with_conflict url =
    let headers = [ "X-Inertia-Location", url ] in
    Dream.respond ~status:`Conflict ~headers ""
  ;;

  let respond_with_json po keys =
    let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
    let* json = Page_object.to_string keys po in
    Dream.json ~headers json
  ;;

  let respond_with_html po =
    let* app = Page_object.to_string All po in
    let head = "<!-- inertia head -->" in
    Dream.html @@ Config.render ~app ~head
  ;;

  let respond po request =
    match Context.request_kind request with
    | Initial_load -> respond_with_html po
    | Inertia_request -> respond_with_json po All
    | Inertia_partial_request { component; requested_keys } ->
      if component <> po.component
      then respond_with_json po All
      else respond_with_json po (Partial requested_keys)
  ;;

  let location request target =
    match Context.request_kind request with
    | Initial_load -> Dream.redirect request target
    | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
  ;;

  let prop ?(merge = Prop.No_merge) ?(load = Prop.Default) name resolver =
    Prop.{ name; merging_mode = merge; resolver; loading_mode = load }
  ;;

  let defer ?(group = "default") = prop ~load:(Defer group)

  let shared_props props inner request =
    Context.set_shared_props request props;
    inner request
  ;;

  let encrypt_history inner request =
    Context.set_encrypt_history request true;
    inner request
  ;;

  let render ~component ?(props = []) ?(clear_history = false) request =
    let props =
      Context.shared_props request
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
        ; encrypt_history = Context.encrypt_history request
        }
    in
    match Page_object.is_version_stale request po, Dream.method_ request with
    | true, `GET -> respond_with_conflict po.url
    | _, _ -> respond po request
  ;;

  let inertia ?props inner request =
    Context.create request props;
    let xsrf = Dream.header request "X-XSRF-TOKEN" in
    let* response =
      match xsrf with
      | Some token ->
        let* valid = Dream.verify_csrf_token request token in
        (match valid with
         | `Expired _ | `Wrong_session | `Invalid ->
           respond_with_conflict @@ Dream.target request
         | `Ok -> inner request)
      | None -> inner request
    in
    let new_token = Dream.csrf_token request in
    Dream.set_cookie response request "XSRF-TOKEN" new_token;
    Lwt.return response
  ;;
end
