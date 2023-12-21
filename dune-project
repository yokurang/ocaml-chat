(lang dune 3.12)

(name one_on_one_chat)

(generate_opam_files true)

(source
 (github yokurang/alan-ahrefs-challenge.git))

(authors "Alan Matthew")

(maintainers "Alan Matthew")

(license LICENSE)

(documentation https://github.com/yokurang/https://github.com/yokurang/alan-ahrefs-challenge.git)

(package
 (name one_on_one_chat)
 (synopsis "A one on one chat application")
 (description "A one on one chat application built using OCaml with a focus on
  robustness, code simplicity and maintainability")
 (depends ocaml dune core async sexplib ppx_jane)
 (tags
  ("chat" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
