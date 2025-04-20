module Server (V : sig
    val version : string option
  end) =
struct
  module Inertia = Dream_inertia.Make (struct
      let render ~head ~app =
        Fmt.str "<html><head>%s</head><body>%s</body></html>" head app
      ;;

      let version () = V.version

      let shared _ =
        Some [ Dream_inertia.prop "shared_test" (fun () -> Lwt.return (`String "hello")) ]
      ;;
    end)

  let describe_middleware handler request =
    let pp_headers =
      Fmt.list ~sep:(Fmt.any "\n ") (Fmt.pair ~sep:(Fmt.any ": ") Fmt.string Fmt.string)
    in
    let pp_req ppf req =
      let m = req |> Dream.method_ |> Dream.method_to_string in
      let t = Dream.target req in
      let h = Dream.all_headers req in
      Fmt.pf ppf "--- request ---\n%s: %s\n[%a]\n" m t pp_headers h
    in
    let pp_resp ppf (res, body) =
      let s = Dream.status res in
      let i = Dream.status_to_int s in
      let r = Dream.status_to_string s in
      let h = Dream.all_headers res in
      Fmt.pf ppf "--- response ---\n%d %s\n[%a]\n\n%s\n" i r pp_headers h body
    in
    let%lwt response = handler request in
    let%lwt body = Dream.body response in
    Fmt.pr "\n%a\n%a\n\n" pp_req request pp_resp (response, body);
    Lwt.return response
  ;;

  let handler request = Inertia.render request ~component:"Test"
  let handler_redirect request = Dream.redirect request "/"
  let handler_location request = Inertia.location request "https://www.google.com"

  let handler_mergeable request =
    let open Dream_inertia in
    Inertia.render
      request
      ~component:"Mergeable"
      ~props:
        [ prop "merge_this" ~merge:Merge (fun () -> Lwt.return (`String "merged"))
        ; prop "not_this" (fun () -> Lwt.return (`String "ow no"))
        ; prop "deep_merge_this" ~merge:Deep_merge (fun () ->
            Lwt.return (`String "deep thinking"))
        ; defer "merge_this_d" ~merge:Merge (fun () -> Lwt.return (`String "merged"))
        ; defer "not_this_d" (fun () -> Lwt.return (`String "ow no"))
        ; defer "deep_merge_this_d" ~merge:Deep_merge (fun () ->
            Lwt.return (`String "deep thinking"))
        ]
  ;;

  let handler_loading request =
    let open Dream_inertia in
    Inertia.render
      request
      ~component:"Loading"
      ~props:
        [ prop "default" (fun () -> Lwt.return (`String "default"))
        ; prop "always" ~load:Always (fun () -> Lwt.return (`String "always"))
        ; prop "optional" ~load:Optional (fun () -> Lwt.return (`String "optional"))
        ; defer "defer" (fun () -> Lwt.return (`String "defer"))
        ]
  ;;

  let handler_with_shared_data request =
    Inertia.render
      request
      ~component:"Test"
      ~props:
        [ Dream_inertia.prop "shared_test" (fun () ->
            Lwt.return (`String "overriden shared data"))
        ]
  ;;

  let handler_clear_history request =
    Inertia.render request ~component:"Test" ~clear_history:true
  ;;

  let encrypt encrypt_history =
    if encrypt_history then Dream_inertia.encrypt_history else Dream.no_middleware
  ;;

  let routes ?(encrypt_history = false) =
    describe_middleware
    @@ Dream.memory_sessions
    @@ Dream_inertia.inertia
    @@ encrypt encrypt_history
    @@ Dream.router
         [ Dream.get "/" handler
         ; Dream.post "/" handler_redirect
         ; Dream.put "/" handler_redirect
         ; Dream.patch "/" handler_redirect
         ; Dream.delete "/" handler_redirect
         ; Dream.get "/shared" handler_with_shared_data
         ; Dream.get "/location" handler_location
         ; Dream.get "/mergeable" handler_mergeable
         ; Dream.get "/loading" handler_loading
         ; Dream.get "/clear-history" handler_clear_history
         ]
  ;;
end

let inertia_header
  ?(accept = "text/html, application/xhtml+xml")
  ?version
  ?inertia
  ?(partial = [])
  ?(component = "Loading")
  ()
  =
  let string_header h v = Option.map (fun (v : string) -> h, v) v in
  let version = string_header "X-Inertia-Version" version in
  let inertia = Option.map (fun i -> "X-Inertia", string_of_bool i) inertia in
  let partial =
    if List.length partial = 0
    then []
    else
      [ Some ("X-Inertia-Partial-Component", component)
      ; Some ("X-Inertia-Partial-Data", String.concat ", " partial)
      ]
  in
  partial @ [ Some ("Accept", accept); inertia; version ] |> List.filter_map (fun x -> x)
;;

module NoVersionServer = Server (struct
    let version = None
  end)

module WithVersionServer = Server (struct
    let version = Some "0"
  end)

let test message routes request =
  Fmt.pr "test: %s" message;
  let _ = Dream.test routes request in
  ()
;;

let () =
  test
    "initial load"
    NoVersionServer.routes
    (Dream.request ~headers:(inertia_header ()) "");
  test
    "inertia load"
    NoVersionServer.routes
    (Dream.request ~headers:(inertia_header ~inertia:true ()) "");
  test
    "inertia with stale version. Server has version \"0\""
    WithVersionServer.routes
    (Dream.request ~headers:(inertia_header ~inertia:true ~version:"1" ()) "");
  [ `POST; `PATCH; `PUT; `DELETE ]
  |> List.iter (fun method_ ->
    test
      (Fmt.str
         "inertia with stale version and a %s method. Server has version \"0\""
         (Dream.method_to_string method_))
      WithVersionServer.routes
      (Dream.request ~method_ ~headers:(inertia_header ~inertia:true ~version:"1" ()) ""));
  test
    "with shared data collisions"
    NoVersionServer.routes
    (Dream.request ~target:"/shared" ~headers:(inertia_header ~inertia:true ()) "");
  test
    "location with inertia request"
    NoVersionServer.routes
    (Dream.request ~target:"/location" ~headers:(inertia_header ~inertia:true ()) "");
  test
    "location is initial load"
    NoVersionServer.routes
    (Dream.request ~target:"/location" "");
  test "mergeable props" NoVersionServer.routes (Dream.request ~target:"/mergeable" "");
  test
    "mergeable props"
    NoVersionServer.routes
    (Dream.request ~target:"/mergeable" ~headers:(inertia_header ~inertia:true ()) "");
  test
    "default load props"
    NoVersionServer.routes
    (Dream.request ~target:"/loading" ~headers:(inertia_header ~inertia:true ()) "");
  test
    "load partial optional"
    NoVersionServer.routes
    (Dream.request
       ~target:"/loading"
       ~headers:(inertia_header ~inertia:true () ~partial:[ "optional" ])
       "");
  test
    "load partial without optional"
    NoVersionServer.routes
    (Dream.request
       ~target:"/loading"
       ~headers:(inertia_header ~inertia:true ~partial:[ "default" ] ())
       "");
  test
    "load partial with wrong component"
    NoVersionServer.routes
    (Dream.request
       ~target:"/loading"
       ~headers:(inertia_header ~inertia:true ~partial:[ "default" ] ~component:"Test" ())
       "");
  test
    "encrypt history"
    (NoVersionServer.routes ~encrypt_history:true)
    (Dream.request ~target:"/" ~headers:(inertia_header ~inertia:true ()) "");
  test
    "clear history"
    (NoVersionServer.routes ~encrypt_history:true)
    (Dream.request ~target:"/clear-history" ~headers:(inertia_header ~inertia:true ()) "")
;;
