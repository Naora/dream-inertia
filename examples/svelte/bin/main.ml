open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

type permission = { kind : string } [@@deriving yojson]

let handler a = Lwt.return (a + 1)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* a = handler 1 in
     let b = a + 1 in
     print_endline (string_of_int b);
     Lwt.return_unit)
;;

let get path ~component props =
  Dream.get path (fun request -> App.Inertia.render request ~component ~props)
;;

let home_handler =
  get
    "/"
    ~component:"Home"
    [ Load
        ( "event"
        , fun () ->
            { id = "123456"
            ; title = "title"
            ; start_date = "26/03/24"
            ; description = "the day where all began"
            }
            |> yojson_of_event
            |> Lwt.return )
    ; Defer
        ( "permissions"
        , fun () ->
            let open Lwt.Syntax in
            let* _ = Lwt_unix.sleep 3. in
            Lwt.return ({ kind = "read" } |> yojson_of_permission) )
    ]
;;

let about_handler = get "/about" ~component:"About" []
let () = Dream.run @@ Dream.logger @@ Dream.router [ home_handler; about_handler ]
