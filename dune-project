(lang dune 1.11)
(name ppx)
(using cinaps 1.0)

(generate_opam_files true)

(maintainers "opensource@janestreet.com")
(authors "Jane Street Group, LLC")
(source (github ocaml-ppx/ppx))

(package
 (name ppx)
 (synopsis "Base library and tools for ppx rewriters")
 (description "
A comprehensive toolbox for ppx development. It features:
- a OCaml AST / parser / pretty-printer snapshot,to create a full
   frontend independent of the version of OCaml;
- a library for library for ppx rewriters in general, and type-driven
  code generators in particular;
- a feature-full driver for OCaml AST transformers;
- a quotation mechanism allowing  to write values representing the
   OCaml AST in the OCaml syntax;
- a generator of open recursion classes from type definitions.")
 (depends
  (ocaml (and (>= 4.07) (< 4.08)))
  dune
  (ppx_expect :with-test)
  (ocaml (and (>= 4.04.1) (< 4.08.0)))
  (base (and (>= v0.11.0) :with-test))
  (stdio (and (>= v0.11.0) :with-test))
  dune
  (ocaml-compiler-libs (>= v0.11.0))
  (ocaml-migrate-parsetree (>= 1.0.9))
  (ppx_derivers (>= 1.0))
  (cinaps :with-test)
  (ocamlfind :with-test)))

(package
 (name astlib)
 (synopsis "Compatibility layer for OCaml compiler AST")
 (description "Provides types for the OCaml compiler AST, along with versioned
interfaces for constructing and deconstructing them so that ppx transformers can
be easily migrated to new versions of the compiler.")
 (depends
  (ocaml (and (>= 4.07) (< 4.08)))
  (base (>= v0.11.0))
  (dune (>= 1.8.0))
  (ocaml-compiler-libs (>= v0.11.0))
  (ocaml-migrate-parsetree (>= 1.0.9))
  (ppx_derivers (>= 1.0))
  (cinaps :with-test)
  ppx_jane
  base_quickcheck
  expect_test_helpers_kernel
  (stdio (>= v0.11.0))))

(package
 (name astlib_first_draft)
 (synopsis "Compatibility layer for OCaml compiler AST")
 (description "Provides types for the OCaml compiler AST, along with versioned
interfaces for constructing and deconstructing them so that ppx transformers can
be easily migrated to new versions of the compiler.")
 (depends
  (ocaml (and (>= 4.07) (< 4.08)))
  (base (>= v0.11.0))
  (dune (>= 1.8.0))
  (ocaml-compiler-libs (>= v0.11.0))
  (ocaml-migrate-parsetree (>= 1.0.9))
  (ppx_derivers (>= 1.0))
  (cinaps :with-test)
  ppx_jane
  base_quickcheck
  expect_test_helpers_kernel
  (stdio (>= v0.11.0))))
