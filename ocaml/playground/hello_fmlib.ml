open Fmlib
open Printf

module CharP = Character_parser.Simple (Char)
module StringP = Character_parser.Simple (String)

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

let main () =
  print_endline "test_letter: ";
  test_letter ();
  print_endline "test_letter_error: ";
  test_letter_error ();;

main ()
