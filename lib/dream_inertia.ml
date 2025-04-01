type props = Yojson.Safe.t

module type View = sig
  val render : app:string -> head:string -> string
end

module type Inertia = sig
  val render
    :  component:string
    -> props:props
    -> Dream.request
    -> Dream.response Lwt.t
end

module Make (V : View) : Inertia = struct
  type partial =
    { data_keys : string list
    ; component : string
    }

  and kind =
    | Initial
    | Inertia
    | Partial of partial

  let get_data_keys data_keys =
    String.split_on_char ',' data_keys
    |> List.filter_map (fun s ->
      match s |> String.trim with
      | "" -> None
      | _ as r -> Some r)
  ;;

  let request_kind request =
    let i = Dream.header request "X-Inertia" in
    let d = Dream.header request "X-Inertia-Partial-Data" in
    let c = Dream.header request "X-Inertia-Partial-Component" in
    match i, d, c with
    | Some "true", Some dk, Some component ->
      let data_keys = get_data_keys dk in
      Partial { data_keys; component }
    | Some "true", _, _ -> Inertia
    | _, _, _ -> Initial
  ;;

  let handle_partial_reload partial component props =
    match partial with
    | p when p.component = component ->
      (match props with
       | `Assoc current_props ->
         let filtered_props =
           List.filter (fun (k, _) -> List.mem k p.data_keys) current_props
         in
         `Assoc filtered_props
       | _ -> props)
    | _ -> props
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
    let headers = [ "Vary", "Inertia"; "X-Inertia", "true" ] in
    match request_kind request with
    | Initial ->
      let app =
        Yojson.Safe.to_string page_object
        |> Fmt.str {html|<div id="app" data-page='%s'></div> |html}
      in
      let head = "<!-- inertia head -->" in
      Dream.respond @@ V.render ~app ~head
    | Inertia -> Dream.json ~headers @@ Yojson.Safe.to_string page_object
    | Partial p ->
      let _ = handle_partial_reload p component props in
      Dream.json ~headers @@ Yojson.Safe.to_string page_object
  ;;
end
