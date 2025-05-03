(** This module provides a set of functions and types for handling Inertia.js
    requests in a Dream web application. It includes functions for rendering
    Inertia pages, creating properties, and handling Inertia responses. *)

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
    group the deferred properties together. The default group is "default". *)
val defer : ?group:string -> ?merge:Prop.merge_kind -> string -> Prop.resolver -> Prop.t

(** {1 Render helpers} *)

(** [page_app] is a function that takes a page data and returns the page app
    as a string. The page app is used to render the Inertia page. *)
val page_app : Template.page_data -> string

(** {1 Response} *)

(** [location request url] is a function that takes a Dream request and a URL
    and returns a Dream response with the location set to the given URL.
    This is used to redirect the Inertia request to
    the given URL. *)
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

(** {1 Middlewares} *)

(** [shared_props props] is a Dream middleware that adds the given properties
    to the Inertia response. The properties are shared between all Inertia
    requests. This middleware should be used in conjunction with the
    {!Dream_inertia.inertia} middleware. *)
val shared_props : Prop.t list -> Dream.middleware

(** [encrypt_history ~encrypt] is a Dream middleware that encrypts
    the Inertia history. If [encrypt] is true, the history will be encrypted.
    Otherwise, it will not be. This middleware should be used in conjunction with
    the {!Dream_inertia.inertia} middleware. *)
val encrypt_history : ?encrypt:bool -> Dream.middleware

(** [inertia ~xsrf ~encrypt_history ~props ~version ~renderer] is a
    Dream middleware that handles Inertia requests. The middleware will automatically handle Inertia requests and
    render the appropriate response based on the request kind. *)
val inertia
  :  ?xsrf:bool
  -> ?encrypt_history:bool
  -> ?props:Prop.t list
  -> ?version:string
  -> renderer:Template.renderer
  -> Dream.middleware

(** {1 Internal modules} *)

module Prop = Prop
module Template = Template
