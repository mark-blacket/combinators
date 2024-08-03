(lang dune 3.14)
(name combinators)
(generate_opam_files true)
(package
  (name combinators)
  (synopsis "Small generic parser combinator library")
  (depends ocaml dune (ounit2 :with-test) (ppx_import :with_test) (ppx_deriving :with_test)))
