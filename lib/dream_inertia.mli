(** This module provides a set of functions and types for handling {{:https://inertiajs.com} Inertia.js}
    requests in a {{:https://aantron.github.io/dream/} Dream} web application. It includes functions for rendering
    Inertia pages, creating properties, and handling Inertia responses. *)

(** {1 Middlewares} *)

(** [inertia ~xsrf ~encrypt_history ~props ~version ~renderer] is a
    Dream middleware that handles Inertia requests. The middleware will automatically handle Inertia requests and
    render the appropriate response based on the request.

    [xsrf] is a boolean that indicates whether to enable CSRF protection for Inertia requests.
    See {{:https://aantron.github.io/dream/#sessions} Dream.session}. Defaults to false.

    [encrypt_history] encrypts the Inertia history.
    See {{:https://inertiajs.com/csrf-protection} csrf-protection}. Defaults to false.

    [version] enables Inertia to reload the application on the client side. See {{:https://inertiajs.com/asset-versioning} asset versioning}. Defaults to None

    Basic usage :
    {[
      (* This render function is for demo purpose use a proper template engine *)
      let render page_data =
        Fmt.str
          "<head></head><body><div id="app" data-page='%a'></div></body>"
          Template.page_data
          page_data
      ;;

      let home_handler request =
        let open Dream_inertia in
        render
          request
          ~component:"Home"
          ~props:
            [ prop "event" (fun () ->
                { id = "123456"
                ; title = "event"
                ; start_date = "26/03/24"
                ; description = "the day where all began"
                }
                |> yojson_of_event
                |> Lwt.return)
            ; defer "deferred_events" (fun () ->
                let open Lwt.Syntax in
                let* _ = Lwt_unix.sleep 3. in
                { id = "123456"
                ; title = "defrred_event"
                ; start_date = "26/03/24"
                ; description = "the day where all began"
                }
                |> yojson_of_event
                |> Lwt.return)
            ]
      ;;

      let () =
        Dream.run
        @@ Dream_inertia.inertia ~renderer:render
        @@ Dream.router [ Dream.get "/" home_handler ]
      ;;
    ]} *)
val inertia
  :  ?xsrf:bool
  -> ?encrypt_history:bool
  -> ?props:Prop.t list
  -> ?version:string
  -> renderer:Template.renderer
  -> Dream.middleware

(** [shared_props props] is a Dream middleware that overrides the shared properties
    to the Inertia response. The properties are shared between all Inertia
    requests. *)
val shared_props : Prop.t list -> Dream.middleware

(** [encrypt_history ~encrypt] is a Dream middleware that encrypts
    the Inertia history. *)
val encrypt_history : ?encrypt:bool -> Dream.middleware

(** {1 Property helpers} *)

(** [prop ?merge ?load page_component resolver] creates a property with optional merge and loading modes. The default
    loading mode is [Default]. The default merge mode is [No_merge]. This is a alias for {!Prop.make}. *)
val prop
  :  ?merge:Prop.merge_kind
  -> ?load:Prop.loading_kind
  -> string
  -> Prop.resolver
  -> Prop.t

(** [defer ?group ?merge page_component resolver] creates a deferred property with optional merge mode. The group is used to
    group the deferred properties together. The default group is "default". This is a alias for {!Prop.make}. *)
val defer : ?group:string -> ?merge:Prop.merge_kind -> string -> Prop.resolver -> Prop.t

(** {1 Render helpers} *)

(** [page_app] is a function that takes a page data and returns the page app
    as a string. The page app is used to render the Inertia page.
    Example for dream eml template :
    {[
      let render app =
        <!DOCTYPE html>
        <html>
          <head>
          </head>
          <body>
            <div id="app" data-page='<%s! app %>'></div>
          </body>
        </html>

      let render page_data = Index.render @@ Dream_inertia.page_app page_data
    ]}*)
val page_app : Template.page_data -> string

(** {1 Response} *)

(** [location request url] is a function that takes a Dream request and a URL
    and returns a Dream response with the location set to the given URL.
    This is used to redirect the Inertia request to the given URL.
    See {{:https://inertiajs.com/redirects#external-redirects} external redirects} *)
val location : Dream.request -> string -> Dream.response Lwt.t

(** [render component ?props ?clear_history request] is a function that takes a
    component name, optional properties, and a Dream request and returns a
    Dream response. The component name is used to identify the page to render.
    The optional properties are used to pass data to the page. The clear_history flag is used to clear the client
    history See {!encrypt_history}. *)
val render
  :  component:string
  -> ?props:Prop.t list
  -> ?clear_history:bool
  -> Dream.request
  -> Dream.response Lwt.t

(** {1 Dream_inertia modules} *)

module Prop = Prop
module Template = Template
