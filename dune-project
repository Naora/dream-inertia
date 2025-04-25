(lang dune 3.14)

(name dream-inertia)

(generate_opam_files true)

(source
 (github Naora/dream-inertia))

(authors "Joris Gundermann")

(maintainers "Joris Gundermann")

(license MIT)

(package
 (name dream-inertia)
 (synopsis "Inertia protocol for OCaml Dream")
 (description "Server side implementation of the Inertia protocol for OCaml Dream web framework")
 (depends 
   ocaml 
   (dream (>= 1.0.0~alpha8))
   fmt 
   yojson 
   ppx_yojson_conv 
   dune)
 (tags
  (inertia dream protocol web)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
