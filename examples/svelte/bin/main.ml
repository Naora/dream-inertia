open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

module Inertia = Dream_inertia.Make (struct
    let render ~head ~app = Index.render head app
    let version () = Some "3"

    let shared _ =
      Some [ Dream_inertia.prop "user" (fun () -> Lwt.return (`String "Felicita")) ]
    ;;
  end)

type permission = { kind : string } [@@deriving yojson]

let home_handler request =
  let open Dream_inertia in
  let open Inertia in
  render
    request
    ~component:"Home"
    ~props:
      [ prop "event" (fun () ->
          { id = "123456"
          ; title = "title"
          ; start_date = "26/03/24"
          ; description = "the day where all began"
          }
          |> yojson_of_event
          |> Lwt.return)
      ; prop "birthday" (fun () ->
          { id = "123"
          ; title = "Johanna"
          ; start_date = "26/03/1985"
          ; description = "the day where all began"
          }
          |> yojson_of_event
          |> Lwt.return)
      ]
    ~deferred:
      [ defer "permissions" (fun () ->
          let open Lwt.Syntax in
          let* _ = Lwt_unix.sleep 3. in
          Lwt.return ({ kind = "read" } |> yojson_of_permission))
      ]
;;

let about_handler request =
  let open Inertia in
  render request ~component:"About"
;;

let redirect_handler request =
  let open Inertia in
  location request "//git.jogun.me"
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" home_handler
       ; Dream.get "/about" about_handler
       ; Dream.get "/redirect" redirect_handler
       ]
;;
