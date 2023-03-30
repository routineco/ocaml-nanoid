(lang dune 3.0)

(name nanoid)

(version 1.0.1)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github routineco/ocaml-nanoid))

(documentation https://routineco.github.io/ocaml-nanoid/)

(package
 (name nanoid)
 (synopsis "Nano ID implementation for OCaml")
 (depends
  (alcotest :with-test)
  (conf-npm :with-test)
  cryptokit
  (js_of_ocaml :with-test)
  (ocaml
   (>= 4.08)))
 (depopts js_of_ocaml))
