type props = Yojson.Safe.t

type t = { root_view : root_view }
and root_view = string -> string

let init root_view = { root_view }
let is_inertia_request request = Dream.header request "X-Inertia" = Some "true"

let handle_partial_reload request component props =
  match
    ( Dream.header request "X-Inertia-Partial-Data"
    , Dream.header request "X-Inertia-Partial-Component" )
  with
  | Some data_keys_str, Some partial_component
    when partial_component = component ->
    (try
       let requested_keys =
         String.split_on_char ',' data_keys_str
         |> List.map String.trim
         |> List.filter (( <> ) "")
       in
       match props with
       | `Assoc current_props ->
         let filtered_props =
           List.filter (fun (k, _) -> List.mem k requested_keys) current_props
         in
         `Assoc filtered_props
       | _ -> props
     with
     | _ ->
       Dream.error (fun log ->
         log ~request "Failed to parse X-Inertia-Partial-Data: %s" data_keys_str);
       props)
  | _ -> props
;;

let render t ~component ~props request =
  let final_props =
    if is_inertia_request request
    then handle_partial_reload request component props
    else props
  in
  let page_object =
    Yojson.Safe.to_string
      (`Assoc
        [ "component", `String component
        ; "props", final_props
        ; "url", `String (Dream.target request)
        ; "version", `String "1"
        ])
  in
  Dream.respond @@ t.root_view page_object
;;

let inertia_head _t = "<!-- inertia head -->"
let inertia t = Fmt.str {html|<div id="app" data-page='%s'></div> |html} t
