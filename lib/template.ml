type t = { render : renderer }
and renderer = page_data -> string
and page_data = { app : string }

let create render = { render }
let page_app p = p.app
let render page_data t = t.render page_data
