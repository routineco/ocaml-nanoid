open Js_of_ocaml

let nanoid = Js.Unsafe.pure_js_expr "require('nanoid').nanoid"

let nanoid ?size () =
  match size with
  | None -> Js.Unsafe.fun_call nanoid [||] |> Js.to_string
  | Some size ->
    Js.Unsafe.fun_call nanoid [| Js.Unsafe.inject size |] |> Js.to_string
