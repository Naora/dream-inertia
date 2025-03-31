module View : Dream_inertia.View = struct
  let render ~app ~head = Index.render app head
end

module Inertia = Dream_inertia.Make (View)
