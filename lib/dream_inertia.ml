open Lwt.Syntax

module type CONFIG = sig
  (* add a page type with head and app *)
  val render : head:string -> app:string -> string
  val version : unit -> string option
end

module type INERTIA = sig
  val prop
    :  ?merge:Prop.merge_kind
    -> ?load:Prop.loading_kind
    -> string
    -> Prop.resolver
    -> Prop.t

  val defer : ?group:string -> ?merge:Prop.merge_kind -> string -> Prop.resolver -> Prop.t

  val render
    :  component:string
    -> ?props:Prop.t list
    -> ?clear_history:bool
    -> Dream.request
    -> Dream.response Lwt.t

  val location : Dream.request -> string -> Dream.response Lwt.t
  val encrypt_history : Dream.middleware
  val shared_props : Prop.t list -> Dream.middleware
  val inertia : ?props:Prop.t list -> Dream.middleware
end

module Make (Config : CONFIG) : INERTIA = struct
  let prop = Prop.create
  let defer ?(group = "default") = prop ~load:(Defer group)

  let location request target =
    match Context.request_kind request with
    | Initial_load -> Dream.redirect request target
    | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
  ;;

  let is_version_stale request version =
    match Dream.header request "X-Inertia-Version", version with
    | Some rv, Some pv -> rv <> pv
    | _, _ -> false
  ;;

  let render ~component ?(props = []) ?(clear_history = false) request =
    let props =
      Context.shared_props request
      |> Option.map (fun s -> Prop.merge_props ~from:s ~into:props)
      |> Option.value ~default:props
    in
    let version = Config.version () in
    let url = Dream.target request in
    match is_version_stale request version, Dream.method_ request with
    | true, `GET -> Response.respond_with_conflict url
    | _, _ ->
      Response.respond
        ~render:Config.render
        request
        Response.
          { component
          ; props
          ; url
          ; version
          ; clear_history
          ; encrypt_history = Context.encrypt_history request
          }
  ;;

  let shared_props props inner request =
    Context.set_shared_props request props;
    inner request
  ;;

  let encrypt_history inner request =
    Context.set_encrypt_history request true;
    inner request
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
           Response.respond_with_conflict @@ Dream.target request
         | `Ok -> inner request)
      | None -> inner request
    in
    let new_token = Dream.csrf_token request in
    Dream.set_cookie response request "XSRF-TOKEN" new_token;
    Lwt.return response
  ;;
end
