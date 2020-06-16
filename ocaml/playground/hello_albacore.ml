open Alba_core
open Printf

let test1 () = 
  let term1 = (Term.Value (Term.Value.Int 1)) in
  let tc1 = Typecheck.check term1 Gamma.empty in
  match tc1 with
      | Ok _ -> printf "ok";
      | Error _ -> printf "error";;

test1 ()

(* let test () = printf "Value (Int 1): %s\n" (Term_printer.string_of_term term1 Gamma.empty);; *)
