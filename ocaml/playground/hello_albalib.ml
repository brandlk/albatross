(* open Fmlib
 * open Module_types *)
(* open Printf *)
open Albalib


module AstExprP = Parser_lang.Make (Ast.Expression)

let test () =
  let open AstExprP in
  let p = run (expression ()) "" in
  let r = match result p with
    | Some _ -> "success"
    | None -> "error" in
  print_endline r;;

test ()
