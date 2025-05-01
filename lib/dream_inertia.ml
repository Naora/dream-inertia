module Prop = Prop
module Template = Template

(* Property helpers *)
let prop = Prop.create
let defer ?(group = "default") = Prop.create ~load:(Defer group)

(* Render helpers *)
let page_app = Template.page_app

(* Response *)
let location = Response.location
let render = Response.render

(* Middlewares *)
let shared_props = Middlewares.shared_props
let encrypt_history = Middlewares.encrypt_history
let inertia = Middlewares.inertia
