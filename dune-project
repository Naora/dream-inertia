(lang dune 3.14)

(name dream_inertia)

(generate_opam_files true)

(source
 (github Naora/dream-inertia))

(authors "Joris Gundermann")

(maintainers "Joris Gundermann")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name dream_inertia)
 (synopsis "Inertia protocol for OCaml Dream")
 (description "Server side implementation of the Inertia protocol for OCaml Dream web framework")
 (depends ocaml dream fmt yojson ppx_yojson_conv alcotest dune)
 (tags
  (topics inertia dream protocol web)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
