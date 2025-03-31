open Ppx_yojson_conv_lib.Yojson_conv

type event =
  { id : string
  ; title : string
  ; start_date : string
  ; description : string
  }
[@@deriving yojson]

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           let event =
             { id = "123456"
             ; title = "title"
             ; start_date = "26/03/24"
             ; description = "the day where all began"
             }
             |> yojson_of_event
           in
           App.Inertia.render request ~component:"Home" ~props:event)
       ; Dream.get "/about" (fun request ->
           App.Inertia.render request ~component:"About" ~props:(`Assoc []))
       ]
;;
