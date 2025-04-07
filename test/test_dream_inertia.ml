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

  let handler request = Inertia.render ~component:"Test" ~props:(`Assoc []) request
end

let test message (handler : Dream.handler) request =
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
  Fmt.pr "test: %s\n%a\n%a\n\n" message pp_req request pp_resp (response, body);
  Lwt.return ()
;;

let inertia_header ?(accept = "text/html, application/xhtml+xml") ?version ?inertia () =
  let string_header h v = Option.map (fun (v : string) -> h, v) v in
  let version = string_header "X-Inertia-Version" version in
  let inertia = Option.map (fun i -> "X-Inertia", string_of_bool i) inertia in
  [ Some ("Accept", accept); inertia; version ] |> List.filter_map (fun x -> x)
;;

module NoVersionServer = Server (struct
    let version = None
  end)

let test_initial_load =
  test
    "test initial load"
    NoVersionServer.handler
    (Dream.request ~headers:(inertia_header ()) "")
;;

let test_inertia_request =
  test
    "test inertia load"
    NoVersionServer.handler
    (Dream.request ~headers:(inertia_header ~inertia:true ()) "")
;;

module WithVersionServer = Server (struct
    let version = Some "0"
  end)

let test_inertia_is_stale_version =
  test
    "test inertia with stale version. Server has version \"0\""
    WithVersionServer.handler
    (Dream.request ~headers:(inertia_header ~inertia:true ~version:"1" ()) "")
;;

let () =
  Lwt_main.run
    (let%lwt () = test_initial_load in
     let%lwt () = test_inertia_request in
     test_inertia_is_stale_version)
;;
