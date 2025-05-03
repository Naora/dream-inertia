# dream-inertia
## Description
This is a OCaml implementation of the Inertia protocol for the Dream web framework. It allows you to build single-page applications (SPAs) using Inertia.js and OCaml.

## Features
- Support for client-side rendering (CSR)
- Support for shared data
- Support for merged props
- Support for always and optional props
- Support for CSRF token
- Support for history encryption

## Installation
To install the package, use OPAM:
```bash
opam install dream-inertia
```
## Usage
To use the package, you need to create a Dream server and configure it to use Inertia. Here is an example:
```ocaml
open Dream
open Inertia
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Inertia.get "/"
      (fun request ->
        let inertia = Inertia.create ~page:"Home" ~props:[] () in
        Inertia.render inertia);
    Inertia.post "/"
      (fun request ->
        let inertia = Inertia.create ~page:"Home" ~props:[] () in
        Inertia.render inertia);
  ]
  |> Dream.serve
```
## Configuration
To configure Inertia, you can use the `Inertia.configure` function. Here is an example:
```ocaml
open Dream
open Inertia
let () =
  Inertia.configure
    ~csrf_token:"my_csrf_token"
    ~history_encryption:true
    ~clear_history:true
    ~shared_data:[("key", "value")]
    ();
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Inertia.get "/"
      (fun request ->
        let inertia = Inertia.create ~page:"Home" ~props:[] () in
        Inertia.render inertia);
    Inertia.post "/"
      (fun request ->
        let inertia = Inertia.create ~page:"Home" ~props:[] () in
        Inertia.render inertia);
  ]
  |> Dream.serve
```
## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
## TODO
- [x] Support for Inertia.js
- [x] Support for Dream web framework
- [x] Support for OCaml
- [x] Support for server-side rendering (SSR)
- [x] Support for client-side rendering (CSR)
- [x] Support for shared data
- [x] Support for location method
- [x] Support for merged props
- [x] Support for always props
- [x] Support for optional props
- [x] Support for CSRF token
- [x] Support for history encryption
- [ ] Support for SSR
- [ ] Support for error validation on backend side


