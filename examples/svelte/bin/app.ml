module Inertia = Dream_inertia.Make (struct
    let render ~head ~app = Index.render head app
    let version () = Some "3"
  end)
