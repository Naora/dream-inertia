open Lwt.Syntax

let shared_props props inner request =
  Context.of_request request |> Context.set_shared_props props |> Context.update request;
  inner request
;;

let encrypt_history ?(encrypt = true) inner request =
  Context.of_request request
  |> Context.set_encrypt_history encrypt
  |> Context.update request;
  inner request
;;

let inertia
      ?(xsrf = false)
      ?(encrypt_history = false)
      ?props
      ?version
      ~renderer
      inner
      request
  =
  Context.make ~encrypt_history ~version ~renderer ~props ~request;
  if not xsrf
  then inner request
  else (
    let xsrf = Dream.header request "X-XSRF-TOKEN" in
    let* response =
      match xsrf with
      | Some token ->
        let* valid = Dream.verify_csrf_token request token in
        (match valid with
         | `Expired _ | `Wrong_session | `Invalid ->
           Response.respond_with_conflict @@ Dream.target request
         | `Ok -> inner request)
      | None -> inner request
    in
    let new_token = Dream.csrf_token request in
    Dream.set_cookie response request "XSRF-TOKEN" new_token;
    Lwt.return response)
;;
