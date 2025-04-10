module Server (V : sig
    val version : string option
  end) =
struct
  module Inertia = Dream_inertia.Make (struct
      let render ~head ~app =
        Fmt.str "<html><head>%s</head><body>%s</body></html>" head app
      ;;

      let version () = V.version
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

  let handler request = Inertia.render ~component:"Test" ~props:[] ~deferred:[] request
  let redirect_handler request = Dream.redirect request "/"

  let routes =
    describe_middleware
    @@ Dream.router
         [ Dream.get "/" handler
         ; Dream.post "/" redirect_handler
         ; Dream.put "/" redirect_handler
         ; Dream.patch "/" redirect_handler
         ; Dream.delete "/" redirect_handler
         ]
  ;;
end

let inertia_header ?(accept = "text/html, application/xhtml+xml") ?version ?inertia () =
  let string_header h v = Option.map (fun (v : string) -> h, v) v in
  let version = string_header "X-Inertia-Version" version in
  let inertia = Option.map (fun i -> "X-Inertia", string_of_bool i) inertia in
  [ Some ("Accept", accept); inertia; version ] |> List.filter_map (fun x -> x)
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
    "test initial load"
    NoVersionServer.routes
    (Dream.request ~headers:(inertia_header ()) "");
  test
    "test inertia load"
    NoVersionServer.routes
    (Dream.request ~headers:(inertia_header ~inertia:true ()) "");
  test
    "test inertia with stale version. Server has version \"0\""
    WithVersionServer.routes
    (Dream.request ~headers:(inertia_header ~inertia:true ~version:"1" ()) "");
  [ `POST; `PATCH; `PUT; `DELETE ]
  |> List.iter (fun method_ ->
    test
      (Fmt.str
         "test inertia with stale version and a %s method. Server has version \"0\""
         (Dream.method_to_string method_))
      WithVersionServer.routes
      (Dream.request ~method_ ~headers:(inertia_header ~inertia:true ~version:"1" ()) ""))
;;
