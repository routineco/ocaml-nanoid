let () =
  let open Alcotest in
  run "Nanoid"
    [ ("pseudo-seeded", [ test_case "generate" `Quick Tests.generate ]) ]
