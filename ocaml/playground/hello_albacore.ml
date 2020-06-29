open Alba_core
open Printf
open Albalib
open Fmlib.Module_types
open Fmlib.Common

let test1 () = 
  let term1 = (Term.Value (Term.Value.Int 1)) in
  let tc1 = Typecheck.check term1 (Context.gamma (Standard_context.make ())) in
  match tc1 with
      | Ok _ -> printf "ok\n";
      | Error _ -> printf "error\n";;

let test2 () =
  let ctx = Standard_context.make () in
  let _ = match (Context.find_name "List" ctx) with
    | x::_ -> x
    | _ -> assert false
  in
  let open Term in
  let term = lambda "x" (Gamma.int_type (Context.gamma ctx)) (variable 0) in
  printf "%s\n" (Term_printer.string_of_term term (Context.gamma ctx))

let print_content () =
  let ctx = Standard_context.make () in
  let terms = List.init (Context.count ctx) (fun x -> Term.variable x) in
  List.map (fun x -> printf "%s\n" (Term_printer.string_of_term x (Context.gamma ctx))) terms;;

let test_context () =
  let ctx = Standard_context.make () in
  let i = Context.find_name "Int" ctx in
  List.map (fun x -> printf "%i " x) i;;

(* test2 () *)

(* test2 (); *)
(* print_endline ""; *)
(* print_content () *)
(* test_context () *)
(* let test () = printf "Value (Int 1): %s\n" (Term_printer.string_of_term term1 Gamma.empty);; *)

module Lcu =
  struct
    type t =
      | Var of string
      | Lambda of string * t
      | App of t * t

    let rec to_string (e: t): string =
      match e with
      | Var x -> x
      | Lambda (x,e1) -> "(" ^ "\\" ^ x ^ "." ^ (to_string e1) ^ ")"
      | App (e1,e2) -> "(" ^ (to_string e1) ^ " " ^ (to_string e2) ^ ")"
  end

module Make (Final: ANY) =
  struct
    module P =
      Fmlib.Character_parser.Simple (Final)
    include P

    
    let var_name: string t =
      word
        Char.is_letter
        (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
        "variable"

    let variable: Lcu.t t =
      (* get_position >>= fun p ->
       * return (printf "variable in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ -> *)
      var_name >>= fun v -> return (Lcu.Var v)

    let whitespace_char: char t =
      one_of_chars " \n\t" "whitespace_char"

    let whitespace: unit t =
      map (fun _ -> ())
        (skip_zero_or_more whitespace_char)

    let rec lambda (): Lcu.t t =
      (* get_position >>= fun p ->
       * return (printf "lambda in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ -> *)

      return (fun v e -> Lcu.Lambda (v,e))
      |. (char '\\')
      |. whitespace
      |= var_name
      |. whitespace
      |. (char '.')
      |. whitespace
      |= expression ()

    
    and basic_expression (): Lcu.t t =
      (* get_position >>= fun p ->
       * return (printf "basic_expression in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ -> *)

      (return identity
       |. char '('
       |= expression ()
       |. char ')')
      <|> variable
      <|> lambda ()

    and expression (): Lcu.t t =
      (* get_position >>= fun p ->
       * return (printf "expression in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ -> *)
      return () >>= fun _ ->
      one_or_more_separated (basic_expression ()) whitespace >>= fun lst ->
      return (match lst with
              | [] -> assert false
              | hd :: tl ->
                 List.fold_left (fun acc e -> Lcu.App (acc,e)) hd tl) 
        
    let test_pipeline: string t =
      return (fun _ _ -> "blubb")
      |= var_name
      |. char 'x'
      |= var_name
  end


let test_variable () =
  let module LcuP = Make (Lcu) in
  let open LcuP in
  let p = run variable "xyz" in
  let r = match result p with
    | Some (Var a) -> a
    | _ -> assert false in
  printf "result: %s\n" r;;


let test_lcu (input: string) =
  let module LcuP = Make (Lcu) in
  let open LcuP in
  let p = run (expression ()) input in
  match result p with
  | Some e ->
     printf "success:\n%s\n\n" (Lcu.to_string e);
  | _ ->
     printf "error\n\n";;



test_lcu "\\x.x";
test_lcu "x y z";
test_lcu "\\x . \\y.x";
test_lcu "(\\x.x)(\\x.x)";
test_lcu "\\x.x\\x.x"

