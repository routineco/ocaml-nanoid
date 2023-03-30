open Base
open Result

let ( let* ) = Result.( >>= )
let ( let+ ) = Option.( >>| )

let parse path =
  Stdio.In_channel.with_file path ~f:(fun input ->
      let lexbuf = Lexing.from_channel input in
      OpamParser.FullPos.main OpamLexer.token lexbuf path)

let pp_file = OpamPrinter.FullPos.format_opamfile
let input = Cmdliner.Arg.(opt (some string) None & info [ "i"; "input" ])

let local =
  Cmdliner.Arg.(
    opt (list string) [] & info ~doc:"Routine local packages" [ "local" ])

let repo_name = Cmdliner.Arg.(opt (some string) None & info [ "n"; "name" ])
let cross_both = Cmdliner.Arg.(opt (list string) [] & info [ "cross-both" ])

let cross_exclude =
  Cmdliner.Arg.(opt (list string) [] & info [ "cross-exclude" ])

let cross_only = Cmdliner.Arg.(opt (list string) [] & info [ "cross" ])

let pp_pos fmt
    {
      OpamParserTypes.FullPos.filename;
      start = start_line, start_col;
      stop = stop_line, stop_col;
    } =
  if start_line <> stop_line then
    Caml.Format.fprintf fmt "%s:%d.%d-%d.%d" filename start_line start_col
      stop_line stop_col
  else
    Caml.Format.fprintf fmt "%s:%d.%d-%d" filename start_line start_col stop_col

let () =
  let open Cmdliner in
  let generate =
    let generate name local cross_both cross_exclude cross_only =
      let* repository, packages =
        let (status, stdout), stderr =
          Shexp_process.(
            run_exit_status "dune" [ "describe"; "opam-files" ]
            |> capture [ Stdout ] |> capture [ Stderr ] |> eval)
        in
        match status with
        | Exited 0 -> (
          match Sexplib.Sexp.parse stdout with
          | Done (List files, _) -> (
            List.map files ~f:(function
              | List [ Atom name; _ ] ->
                Result.return
                  (String.chop_suffix_if_exists ~suffix:".opam" name)
              | sexp ->
                Result.fail
                  (Caml.Format.asprintf
                     "dune describe opam-files yielded invalid entry:@ @[%a@]"
                     Sexplib.Sexp.pp sexp))
            |> Result.all
            >>= function
            | hd :: _ as packages -> (
              match name with
              | Some name -> Result.return (name, packages)
              | None -> Result.return (hd, packages))
            | [] -> Result.fail "dune describe opam-files yielded no entries")
          | _ ->
            Result.fail
              (Caml.Format.asprintf
                 "dune describe opam-files yielded invalid sexp:@ @[%s@]" stdout)
          )
        | _ ->
          Result.fail
            (Caml.Format.asprintf "dune describe opam-files failed:@ @[%s@]"
               stderr)
      in
      let generate package =
        let open Sexplib.Sexp in
        let opam_rule suffix =
          List
            [
              Atom "rule";
              List
                [
                  Atom "target";
                  Atom (package ^ suffix ^ ".%{version:" ^ package ^ "}.opam");
                ];
              List
                [
                  Atom "deps";
                  List [ Atom ":opam"; Atom (package ^ suffix ^ ".opam") ];
                ];
              List
                [
                  Atom "action";
                  List
                    [
                      Atom "with-stdout-to";
                      Atom "%{target}";
                      List
                        [
                          Atom "progn";
                          List [ Atom "cat"; Atom "%{opam}" ];
                          List
                            [
                              Atom "echo";
                              Atom
                                ("url { src: \
                                  \"git://git@gitlab.routine.co:routine/"
                               ^ repository ^ "#%{version:" ^ package ^ "}\" }"
                                );
                            ];
                        ];
                    ];
                ];
            ]
        and locked_rule =
          List
            [
              Atom "rule";
              List [ Atom "deps"; List [ Atom "universe" ] ];
              List [ Atom "target"; Atom (package ^ ".opam.locked") ];
              List
                [
                  Atom "action";
                  List
                    [
                      Atom "run"; Atom "%{bin:opam}"; Atom "lock"; Atom package;
                    ];
                ];
            ]
        and extdeps_rule =
          List
            [
              Atom "rule";
              List [ Atom "alias"; Atom "extdeps" ];
              List
                [
                  Atom "mode";
                  List [ Atom "promote"; List [ Atom "until-clean" ] ];
                ];
              List [ Atom "target"; Atom (package ^ ".opam.extdeps") ];
              List
                [
                  Atom "action";
                  List
                    [
                      Atom "with-stdout-to";
                      Atom "%{target}";
                      List
                        [
                          Atom "run";
                          Atom "%{dep:.logistic/dune/extdeps/extdeps.exe}";
                          Atom "rewrite";
                          Atom "--input";
                          Atom ("%{dep:" ^ package ^ ".opam.locked}");
                          Atom "--local";
                          Atom (String.concat ~sep:"," (packages @ local));
                        ];
                    ];
                ];
            ]
        and ios_rule =
          List
            [
              Atom "rule";
              List [ Atom "alias"; Atom "default" ];
              List [ Atom "target"; Atom (package ^ "-ios.opam") ];
              List
                [
                  Atom "action";
                  List
                    [
                      Atom "with-stdout-to";
                      Atom "%{target}";
                      List
                        [
                          Atom "run";
                          Atom "%{dep:.logistic/dune/extdeps/extdeps.exe}";
                          Atom "rewrite-ios";
                          Atom "--input";
                          Atom ("%{dep:" ^ package ^ ".opam}");
                          Atom "--cross";
                          Atom
                            (String.concat ~sep:","
                               (packages @ local @ cross_only));
                          Atom "--cross-both";
                          Atom (String.concat ~sep:"," cross_both);
                          Atom "--cross-exclude";
                          Atom (String.concat ~sep:"," cross_exclude);
                        ];
                    ];
                ];
            ]
        in
        [ opam_rule ""; ios_rule; opam_rule "-ios"; locked_rule; extdeps_rule ]
      in
      List.map ~f:generate packages
      |> List.concat
      |> List.iter ~f:(Caml.Format.printf "@[%a@]@." Sexplib.Sexp.pp_hum)
      |> Result.return
    in
    Term.(
      const generate $ Arg.value repo_name $ Arg.value local
      $ Arg.value cross_both $ Arg.value cross_exclude $ Arg.value cross_only)
    |> Cmd.(v (info "generate"))
  and rewrite =
    let open OpamParserTypes.FullPos in
    let pos f ({ pelem; _ } as pos) =
      match f pelem with Some pelem -> Some { pos with pelem } | None -> None
    in
    let rewrite exclude path =
      let excluded = List.mem ~equal:String.equal exclude in
      let file = parse path in
      let rec rewrite_contents contents =
        List.filter_map ~f:(pos rewrite_item) contents
      and rewrite_item = function
        | Variable (({ pelem = name; _ } as name_pos), value) as item -> (
          match name with
          | "depends" ->
            let+ value = pos rewrite_depends value in
            Variable (name_pos, value)
          | "pin-depends" ->
            let+ value = pos rewrite_pin_depends value in
            Variable (name_pos, value)
          | "build" -> None
          | _ -> Some item)
        | Section section ->
          let+ section = rewrite_section section in
          Section section
      and rewrite_section = function
        | { section_kind = { pelem = "url"; _ }; _ } -> None
        | section -> Some section
      and rewrite_depends = function
        | List ({ pelem = items; _ } as value) -> (
          let f = function
            | Option ({ pelem = String name; _ }, _) as depend ->
              Option.some_if (not (excluded name)) depend
            | _ -> failwith "unrecognized depends entry"
          in
          match List.filter_map ~f:(pos f) items with
          | [] -> None
          | { pos; _ } :: _ as l ->
            let pin_depend package version =
              {
                pelem =
                  Option
                    ( { pelem = String package; pos },
                      {
                        pelem =
                          [
                            {
                              pelem =
                                Prefix_relop
                                  ( { pelem = `Eq; pos },
                                    { pelem = String version; pos } );
                              pos;
                            };
                          ];
                        pos;
                      } );
                pos;
              }
            in
            Some
              (List
                 {
                   value with
                   pelem =
                     pin_depend "ocamlformat" "0.24.1"
                     :: { pelem = String "opam-file-format"; pos }
                     :: { pelem = String "sexplib"; pos }
                     :: { pelem = String "shexp"; pos }
                     :: l;
                 }))
        | _ -> failwith "pin-depends expects a list"
      and rewrite_pin_depends = function
        | List { pelem = [ { pelem = String name; _ }; _ ]; _ } as pin ->
          let name = String.lsplit2_exn ~on:'.' name |> fst in
          Option.some_if (not (excluded name)) pin
        | List ({ pelem = items; _ } as value) -> (
          let f = function
            | List { pelem = [ { pelem = String name; _ }; _ ]; _ } as pin ->
              let name = String.lsplit2_exn ~on:'.' name |> fst in
              Option.some_if (not (excluded name)) pin
            | _ -> failwith "unrecognized pin-depends entry"
          in
          match List.filter_map ~f:(pos f) items with
          | [] -> None
          | l -> Some (List { value with pelem = l }))
        | _ -> failwith "pin-depends expects a list"
      in
      Caml.Format.printf "@[%a@]" pp_file
        { file with file_contents = rewrite_contents file.file_contents }
      |> Result.return
    in
    Term.(const rewrite $ Arg.value local $ Arg.required input)
    |> Cmd.(v (info "rewrite"))
  and rewrite_ios =
    let open OpamParserTypes.FullPos in
    let pos f ({ pelem; _ } as pos) =
      match f pelem with
      | Result.Ok pelem -> Result.return { pos with pelem }
      | Result.Error (None, msg) -> Result.Error (Some pos.pos, msg)
      | Result.Error _ as error -> error
    in
    let filter_doc_test =
      let rec filter_opt = function
        | Ident "with-doc" | Ident "with-test" -> false
        | Logop ({ pelem = `And; _ }, { pelem = l; _ }, { pelem = r; _ }) ->
          filter_opt l && filter_opt r
        | Logop ({ pelem = `Or; _ }, { pelem = l; _ }, { pelem = r; _ }) ->
          filter_opt l || filter_opt r
        | _ -> true
      in
      function
      | Option (_, { pelem = options; _ }) ->
        List.map ~f:(fun { pelem; _ } -> filter_opt pelem) options
        |> List.fold ~f:( && ) ~init:true
      | _ -> true
    in
    let rewrite path cross cross_both cross_exclude =
      let package = String.rsplit2 ~on:'.' path |> Option.value_exn |> fst in
      let file = parse path in
      let rec rewrite_contents contents =
        List.map ~f:(pos rewrite_item) contents |> Result.all
      and rewrite_item = function
        | Variable (({ pelem = name; _ } as name_pos), value) as item -> (
          match name with
          | "build" ->
            let* build = pos rewrite_build value in
            Result.return (Variable (name_pos, build))
          | "depends" | "depopts" ->
            let* depends = pos rewrite_depends value in
            Result.return (Variable (name_pos, depends))
          | _ -> Result.return item)
        | Section _ as section -> Result.return section
      and rewrite_build = function
        | List commands ->
          let* commands =
            pos
              (fun commands ->
                let* commands =
                  List.map ~f:(pos rewrite_command) commands |> Result.all
                in
                Result.return commands)
              commands
          in
          Result.return (List commands)
        | _ -> Result.fail (None, {|expected a list for "build"|})
      and rewrite_depends = function
        | List depends ->
          let rec rewrite ({ pelem = dep; _ } as item) =
            if filter_doc_test dep then
              match dep with
              | String dep when List.mem ~equal:String.equal cross_exclude dep
                ->
                None
              | String dep when List.mem ~equal:String.equal cross_both dep ->
                Some
                  [
                    { item with pelem = String dep };
                    { item with pelem = String (dep ^ "-ios") };
                  ]
              | String dep
                when List.mem ~equal:String.equal ("ocaml" :: cross) dep ->
                Some [ { item with pelem = String (dep ^ "-ios") } ]
              | Option (value, options) ->
                let+ values = rewrite value in
                List.map values ~f:(fun value ->
                    { item with pelem = Option (value, options) })
              | _ -> Some [ item ]
            else None
          in
          let* depends =
            pos
              (fun depends ->
                List.filter_map ~f:rewrite depends
                |> List.concat |> Result.return)
              depends
          in
          Result.return (List depends)
        | _ -> Result.fail (None, {|expected a list for "build"|})
      and rewrite_command = function
        | List
            ({
               pelem =
                 ({ pelem = String "dune"; _ } as dune)
                 :: ({ pelem = String "build"; _ } as dune_command)
                 :: tail;
               _;
             } as command) ->
          let tail =
            { pelem = String "-x"; pos = dune_command.pos }
            :: { pelem = String "ios"; pos = dune_command.pos }
            :: List.filter_map
                 ~f:(function
                   | { pelem = Ident "name"; _ } as item ->
                     Some { item with pelem = String package }
                   | { pelem; _ } as item ->
                     Option.some_if (filter_doc_test pelem) item)
                 tail
          in
          Result.return
            (List { command with pelem = dune :: dune_command :: tail })
        | List _ as command -> Result.return command
        | Option (value, options) ->
          let* value = pos rewrite_command value in
          Result.return (Option (value, options))
        | _ -> Result.fail (None, "expected a list for build command")
      in
      match rewrite_contents file.file_contents with
      | Result.Ok contents ->
        Caml.Format.printf "@[%a@]" pp_file
          { file with file_contents = contents }
        |> Result.return
      | Result.Error (Some pos, msg) ->
        Result.Error
          (Caml.Format.asprintf "%a: %s" pp_pos
             { pos with filename = path }
             msg)
      | Result.Error (None, msg) -> Result.Error msg
    in
    Term.(
      const rewrite $ Arg.required input $ Arg.value cross_only
      $ Arg.value cross_both $ Arg.value cross_exclude)
    |> Cmd.(v (info "rewrite-ios"))
  in
  Cmdliner.Cmd.(
    eval_result (group (info "extdeps") [ generate; rewrite; rewrite_ios ]))
  |> Caml.exit
