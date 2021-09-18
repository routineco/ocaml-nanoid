let generate () =
  let ids = List.init 256 (fun _ -> Nanoid.nanoid ()) in
  List.iter
    (fun id ->
      Printf.eprintf "%S\n" id;
      Alcotest.(check int "nanoid length" 21 @@ String.length id))
    ids;
  let unique = List.sort_uniq String.compare ids in
  Alcotest.(check int "unicity" (List.length ids) (List.length unique))

let () = generate ()

let () =
  let open Alcotest in
  run "Nanoid" [ ("pseudo-seeded", [ test_case "generate" `Quick generate ]) ]
