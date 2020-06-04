open Fmlib
open Module_types
open Printf

module Pair (A: ANY) (B: ANY) =
  struct
    type t = (A.t * B.t)
  end

module CharP = Character_parser.Simple (Char)
module StringP = Character_parser.Simple (String)
module PairP = Character_parser.Simple (Pair (Char) (Char))

let test_letter () =
  let open CharP in
  let p = run letter "a" in
  let r = match result p with
    | Some a -> a
    | None -> assert false in
  printf "result: %c\n" r;
  printf "needs_more: %b\n" (needs_more p);
  printf "has_ended: %b\n" (has_ended p);;  

let test_letter_error () =
  let open CharP in
  let p = run letter "1" in
  match result p with
    | Some _ -> assert false
    | None -> if (has_succeeded p) then print_endline "success" else print_endline("error")

let test_pair () =
  let open PairP in
  let p = run (letter
               >>= fun c ->
               space
               >>= fun _ ->
               digit
               >>= fun d ->
               return (c,d)) "a 1" in
  let (a,b) = match result p with
    | Some a -> a
    | None -> assert false in
  printf "result: %c, %c\n" a b

let test_unexpected () =
  let open CharP in
  let p = run (unexpected "blubb" >>= fun _ -> letter) "123" in
  printf "result: %s\n" (result_string p (fun x -> Char.escaped x));
  printf "lookahead %s\n" (lookahead_string p);
  printf "line %d, column %d\n" (line p) (column p);
  printf "has ended: %b\n" (has_ended p);
  printf "expectations: ";
  (fun _ -> ()) (List.map (fun e -> printf "%s " (let (s,_)=e in s)) (Error.expectations (error p)));
  print_endline "";;
    (* (match result p with
     *  | Some y -> y
     *  | None -> 'n') *)

let main () =
  print_endline "test_letter: ";
  test_letter ();
  print_endline "test_letter_error: ";
  test_letter_error ();
  print_endline "test_pair: ";
  test_pair ();
  print_endline "test_unexpected: ";
  test_unexpected ();;

main ()
