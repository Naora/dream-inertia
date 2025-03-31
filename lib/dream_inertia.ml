type props = Yojson.Safe.t

module type View = sig
  val render : app:string -> head:string -> string
end

module type Inertia = sig
  type t

  val render
    :  component:string
    -> props:props
    -> Dream.request
    -> Dream.response Lwt.t
end

module Make (V : View) : Inertia = struct
  type t

  let is_inertia_request request =
    Dream.header request "X-Inertia" = Some "true"
  ;;

  (* let handle_partial_reload request component props =
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
     log
     ~request
     "Failed to parse X-Inertia-Partial-Data: %s"
     data_keys_str);
     props)
     | _ -> props
     ;; *)

  let head _t = "<!-- inertia head -->"

  let app props =
    Yojson.Safe.to_string props
    |> Fmt.str {html|<div id="app" data-page='%s'></div> |html}
  ;;

  let render ~component ~props request =
    let page_object =
      `Assoc
        [ "component", `String component
        ; "props", props
        ; "url", `String (Dream.target request)
        ; "version", `String "1"
        ]
    in
    if is_inertia_request request
    then
      Dream.json ~headers:[ "Vary", "Inertia"; "X-Inertia", "true" ]
      @@ Yojson.Safe.to_string page_object
    else
      Dream.respond @@ V.render ~app:(app page_object) ~head:(head page_object)
  ;;
end
