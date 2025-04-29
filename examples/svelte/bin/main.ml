open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

let render page_data = Index.render @@ Dream_inertia.app page_data

type permission = { kind : string } [@@deriving yojson]

let home_handler request =
  let open Dream_inertia in
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
      ; prop ~merge:Merge "events" (fun () ->
          let events =
            [ { id = "123"
              ; title = "Johannas event"
              ; start_date = "26/03/1985"
              ; description = "the day where all began"
              }
            ]
            |> List.map yojson_of_event
          in
          Lwt.return (`List events))
      ; defer "permissions" (fun () ->
          let open Lwt.Syntax in
          let* _ = Lwt_unix.sleep 3. in
          Lwt.return ({ kind = "read" } |> yojson_of_permission))
      ]
;;

let about_handler request =
  let open Dream_inertia in
  render request ~component:"About"
;;

let redirect_handler request =
  let open Dream_inertia in
  location request "//git.jogun.me"
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream_inertia.inertia
       ~xsrf:true
       ~version:"3"
       ~renderer:render
       ~props:[ Dream_inertia.prop "user" (fun () -> Lwt.return (`String "Felicita")) ]
  @@ Dream.router
       [ Dream.get "/" home_handler
       ; Dream.get "/about" about_handler
       ; Dream.get "/redirect" redirect_handler
       ]
;;
