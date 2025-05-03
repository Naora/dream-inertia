open Lwt.Syntax

type requested_keys =
  | All
  | Partial of string list

module Page_object = struct
  type t =
    { component : string
    ; props : Prop.t list
    ; url : string
    ; version : string option
    ; clear_history : bool
    ; encrypt_history : bool
    ; merge_props : string list
    ; deep_merge_props : string list
    ; deferred_props : deferred_group list
    }

  and deferred_group = string * string list

  let filter_props ~keys props =
    match keys with
    | All ->
      List.filter
        (fun p ->
          match Prop.loading_mode p with
          | Optional | Defer _ -> false
          | Always | Default -> true)
        props
    | Partial keys ->
      List.filter (fun p -> List.mem (Prop.name p) keys || p.loading_mode = Always) props
  ;;

  let mergeable_props props =
    List.fold_left
      (fun (merge, deep_merge) p ->
        match Prop.merging_mode p with
        | No_merge -> merge, deep_merge
        | Merge -> Prop.name p :: merge, deep_merge
        | Deep_merge -> merge, Prop.name p :: deep_merge)
      ([], [])
      props
  ;;

  let deferred_props_by_group props =
    let rec aux acc props =
      match props with
      | [] -> acc
      | p :: t ->
        (match Prop.loading_mode p with
         | Always | Optional | Default -> aux acc t
         | Defer group ->
           let new_acc =
             match List.assoc_opt group acc with
             | None -> (group, [ Prop.name p ]) :: acc
             | Some l -> (group, Prop.name p :: l) :: List.remove_assoc group acc
           in
           aux new_acc t)
    in
    aux [] props
  ;;

  let create ~component ~props ~url ~version ~clear_history ~encrypt_history ~keys =
    let merge_props, deep_merge_props = mergeable_props props in
    let deferred_props =
      match keys with
      | All -> deferred_props_by_group props
      | Partial _ -> []
    in
    let props = filter_props ~keys props in
    { component
    ; props
    ; url
    ; version
    ; clear_history
    ; encrypt_history
    ; merge_props
    ; deep_merge_props
    ; deferred_props
    }
  ;;

  let resolve_props props =
    Lwt_list.map_p
      (fun t ->
        let* v = Prop.resolve t in
        Lwt.return (Prop.name t, v))
      props
  ;;

  let to_json t =
    let* props = resolve_props t.props in
    let merge_props = List.map (fun n -> `String n) t.merge_props in
    let deep_merge_props = List.map (fun n -> `String n) t.deep_merge_props in
    let deferred_props =
      List.map
        (fun (g, ns) -> g, `List (List.map (fun n -> `String n) ns))
        t.deferred_props
    in
    let version =
      match t.version with
      | Some v -> `String v
      | None -> `Null
    in
    let json =
      [ "component", `String t.component
      ; "props", `Assoc props
      ; "url", `String t.url
      ; "version", version
      ; "mergeProps", `List merge_props
      ; "deepMergeProps", `List deep_merge_props
      ; "deferredProps", `Assoc deferred_props
      ; "clearHistory", `Bool t.clear_history
      ; "encryptHistory", `Bool t.encrypt_history
      ]
    in
    Lwt.return (`Assoc json)
  ;;

  let to_string t =
    let* json = to_json t in
    let json_str = Yojson.Safe.to_string json in
    Lwt.return json_str
  ;;
end

let respond_with_conflict url =
  let headers = [ "X-Inertia-Location", url ] in
  Dream.respond ~status:`Conflict ~headers ""
;;

let respond_with_json page_object =
  let headers = [ "Vary", "X-Inertia"; "X-Inertia", "true" ] in
  let* json = Page_object.to_string page_object in
  Dream.json ~headers json
;;

let respond_with_html ~context page_object =
  let* app = Page_object.to_string page_object in
  context |> Context.template |> Template.render { app } |> Dream.html
;;

let location request target =
  match Context.of_request request |> Context.request_kind with
  | Initial_load -> Dream.redirect request target
  | _ -> Dream.respond ~status:`Conflict ~headers:[ "X-Inertia-Location", target ] ""
;;

let rec merge_props ~from ~into =
  match from with
  | [] -> into
  | hd :: ta ->
    if List.exists (fun p -> Prop.name hd = Prop.name p) into
    then merge_props ~into ~from:ta
    else merge_props ~into:(hd :: into) ~from:ta
;;

let is_version_stale request version =
  match Dream.header request "X-Inertia-Version", version with
  | Some rv, Some pv -> rv <> pv
  | _, _ -> false
;;

let render ~component ?(props = []) ?(clear_history = false) request =
  let context = Context.of_request request in
  let version = Context.version context in
  let encrypt_history = Context.encrypt_history context in
  let props =
    Context.shared_props context
    |> Option.map (fun s -> merge_props ~from:s ~into:props)
    |> Option.value ~default:props
  in
  let url = Dream.target request in
  match is_version_stale request version, Dream.method_ request with
  | true, `GET -> respond_with_conflict url
  | _, _ ->
    let partial_page_object =
      Page_object.create ~component ~props ~url ~version ~clear_history ~encrypt_history
    in
    (match Context.request_kind context with
     | Initial_load -> respond_with_html ~context @@ partial_page_object ~keys:All
     | Inertia_request -> respond_with_json @@ partial_page_object ~keys:All
     | Inertia_partial_request { component = c; requested_keys } ->
       if c <> component
       then respond_with_json @@ partial_page_object ~keys:All
       else respond_with_json @@ partial_page_object ~keys:(Partial requested_keys))
;;
