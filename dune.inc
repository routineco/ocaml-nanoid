(rule (target nanoid.%{version:nanoid}.opam) (deps (:opam nanoid.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/nanoid#%{version:nanoid}\" }")))))
(rule (alias default) (target nanoid-ios.opam)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite-ios --input
    %{dep:nanoid.opam} --cross nanoid,cryptokit --cross-both ""
    --cross-exclude js_of_ocaml))))
(rule (target nanoid-ios.%{version:nanoid}.opam)
 (deps (:opam nanoid-ios.opam))
 (action
  (with-stdout-to %{target}
   (progn (cat %{opam})
    (echo
     "url { src: \"git://git@gitlab.routine.co:routine/nanoid#%{version:nanoid}\" }")))))
(rule (deps (universe)) (target nanoid.opam.locked)
 (action (run %{bin:opam} lock nanoid)))
(rule (alias extdeps) (mode (promote (until-clean)))
 (target nanoid.opam.extdeps)
 (action
  (with-stdout-to %{target}
   (run %{dep:.logistic/dune/extdeps/extdeps.exe} rewrite --input
    %{dep:nanoid.opam.locked} --local nanoid))))
