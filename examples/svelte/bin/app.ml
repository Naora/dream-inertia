module Inertia = Dream_inertia.Make (struct
    let render ~app ~head = Index.render app head
    let version () = Some "3"
  end)
