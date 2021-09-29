(lang dune 2.8)

(name nanoid)

(version 0.1.0)

(generate_opam_files true)

(license LGPL-3.0-or-later)

(maintainers "mefyl <mefyl@gruntech.org>")

(authors "mefyl <mefyl@gruntech.org>")

(source
 (github mefyl/nanoid))

(package
 (name nanoid)
 (synopsis "Nano ID implementation for OCaml")
 (depends
  (alcotest :with-test)
  cryptokit
  (js_of_ocaml :with-test)
  (ocamlformat
   (and
    :dev
    (= 0.17.0))))
 (depopts js_of_ocaml))
