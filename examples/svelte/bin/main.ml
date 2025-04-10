open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

type permission = { kind : string } [@@deriving yojson]

(* TODO: ajouter ces methods d'aide dans la lib *)
let get path ~component ~props ~deferred =
  Dream.get path (fun request -> App.Inertia.render request ~component ~props ~deferred)
;;

let prop name resolver = Dream_inertia.{ name; resolver }

let deferred name ?(group = "default") resolver =
  Dream_inertia.{ prop = { name; resolver }; group }
;;

let home_handler =
  get
    "/"
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

let about_handler = get "/about" ~component:"About" ~props:[] ~deferred:[]
let () = Dream.run @@ Dream.logger @@ Dream.router [ home_handler; about_handler ]
