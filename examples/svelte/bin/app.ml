module Inertia = Dream_inertia.Make (struct
    let render ~head ~app = Index.render app head
    let version () = Some "3"
  end)
