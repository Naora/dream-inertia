open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

type permission = { kind : string } [@@deriving yojson]

let home_handler request =
  let open App.Inertia in
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
      ]
    ~deferred:
      [ deferred "permissions" (fun () ->
          let open Lwt.Syntax in
          let* _ = Lwt_unix.sleep 3. in
          Lwt.return ({ kind = "read" } |> yojson_of_permission))
      ]
;;

let about_handler request =
  let open App.Inertia in
  render request ~component:"About"
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [ Dream.get "/" home_handler; Dream.get "/about" about_handler ]
;;
