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

print_content ();;

(* let test_context () = *)
  (* let ctx = Standard_context.make () in
   * let i = Context.find_name "Int" ctx in
   * List.map (fun x -> printf "%i " x) i;; *)
  (* let ctx = Context.empty in
   * let pushed = Context.push_local "x" (Term.Sort Term.Sort.Proposition)
   *                (Context.push_local "x" (Term.Sort (Term.Sort.Any 0))
   *                 (Context.push_local "y" (Term.Sort (Term.Sort.Any 1)) ctx))
   * in
   * let x::_ = Context.find_name "x" pushed in
   * let y::_ = Context.find_name "y" pushed in
   * let x_leveled = Context.level_of_index x pushed in
   * let y_leveled = Context.level_of_index y pushed in
   * printf "x: %i, y: %i\n" x_leveled y_leveled;; *)

(* test2 () *)

(* test2 (); *)
(* print_endline ""; *)
(* print_content () *)
(* test_context () *)
(* let test () = printf "Value (Int 1): %s\n" (Term_printer.string_of_term term1 Gamma.empty);; *)

(* test_context ();; *)


module Binder =
  struct
    type t =
      | Lambda
      | Pi
  end

module Make (Final: ANY) =
  struct
    module P =
      Fmlib.Character_parser.Normal (Context) (Final) (String) (String)
    include P


    let whitespace_char: char t =
      one_of_chars " \n\t" "whitespace_char"
    
    let whitespace: unit t =
      map (fun _ -> ())
        (skip_zero_or_more whitespace_char)
    
    let var_name: string t =
      word
        Char.is_letter
        (fun c -> Char.is_letter c || Char.is_digit c || c = '_')
        "variable"

    let variable: Term.t t =
      get_position >>= fun p ->
      return (printf "variable in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->
      var_name >>= fun v ->
      get_state >>= fun ctx ->
      match v with
      | "Any"         -> return (Term.Sort (Term.Sort.Any 0))
      | "Proposition" -> return (Term.Sort Term.Sort.Proposition)
      | _             ->
         match Alba_core.Context.find_name v ctx with
         | []    -> fail ("unbound variable: " ^ v)
         | i::[] -> return (Term.Variable (Alba_core.Context.level_of_index i ctx))
         | _     -> fail ("more than one binding for variable: " ^ v)


    let lam: Binder.t t =
      backtrackable
        (string "\\" >>= fun () ->
         not_followed_by (string "/") "not /")
        "\\" >>= fun () ->
      return (Binder.Lambda)

    let pi: Binder.t t =
      string "\\/" >>= fun () -> return Binder.Pi
    
    
    let rec binding (): Term.t t =
      get_position >>= fun p ->
      return (printf "binding in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->

      (lam <|> pi) >>= fun b ->
      whitespace >>= fun _ ->
      get_state >>= fun old_ctx ->
      one_or_more_separated
        (typed_var () >>= fun (v,t) ->
         update (Alba_core.Context.push_local v t) >>= fun _ ->
         return (v,t)
        )
        whitespace >>= fun lst ->
      whitespace >>= fun _ ->
      char '.' >>= fun _ ->
      whitespace >>= fun _ ->
      expression () >>= fun e ->
      update (fun _ -> old_ctx) >>= fun _ ->
      return (
          List.fold_right
            (fun (v,t) exp -> match b with
                              | Binder.Lambda ->
                                 Term.Lambda (t,exp,Term.Lambda_info.typed v)
                              | Binder.Pi     ->
                                 Term.Pi (t,exp,Term.Pi_info.typed v))
            lst
            e)
      

    and typed_var (): (string*Term.t) t =
      return (fun v t -> (v,t))
      |. char '('
      |= var_name
      |. whitespace
      |. char ':'
      |. whitespace
      |= expression ()
      |. char ')'

    
    and basic_expression (): Term.t t =
      get_position >>= fun p ->
      return (printf "basic_expression in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->
    
      (return identity
       |. char '('
       |= expression ()
       |. char ')')
      <|> variable
      <|> binding ()
    
    and expression (): Term.t t =
      get_position >>= fun p ->
      return (printf "expression in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->
      return () >>= fun _ ->
      one_or_more_separated (basic_expression ()) whitespace >>= fun lst ->
      return (match lst with
              | [] -> assert false
              | hd :: tl ->
                 List.fold_left (fun acc e -> Term.Appl (acc,e,Term.Application_info.Normal)) hd tl)
        
    let test_pipeline: string t =
      return (fun _ _ -> "blubb")
      |= var_name
      |. char 'x'
      |= var_name
  end


let test_variable1 () =
  let module P = Make (Term) in
  let open P in
  let p = run variable Alba_core.Context.empty "xyz" in
  let r = match result p with
    | Some _ -> assert false
    | None   -> error p in
  printf "is_semantic: %b\n" (Error.is_semantic r);
  printf "semantic: %s\n" (Error.semantic r);;

let test_variable2 () =
  let module P = Make (Term) in
  let open P in
  let ctx = (Alba_core.Context.push_local "x" (Term.Sort Term.Sort.Proposition)
               Alba_core.Context.empty) in
  let p = run variable ctx "x" in
  let r = match result p with
    | Some t -> t
    | None   -> assert false in
  printf "%s\n" (Term_printer.string_of_term r (Alba_core.Context.gamma ctx));;


let test_term (input: string) =
  let module P = Make (Term) in
  let open P in
  let ctx = Standard_context.make () in
  let p = run (expression ()) ctx input in
  let r = match result p with
    | Some t -> Term_printer.string_of_term t (Alba_core.Context.gamma ctx)
    | None -> let err = error p in
              if Error.is_semantic err
              then Error.semantic err
              else String.concat "," (List.map (fun (e,_) -> e) (Error.expectations err)) in
  printf "%s\n" r;;
  
(* test_variable1 (); *)
(* test_variable2 () *)

(* test_term "Any";
 * test_term "\\(x:Any).x";
 * test_term "\\/(x:Any).x";
 * test_term "\\(x: Int).x";
 * test_term "\\(T: \\/(x:Int).Any).T";
 * test_term "\\(x:Any).\\(y:Any).y";
 * test_term "\\(x:Any) (y: Any)(z :Any). x y z";
 * test_term "\\(Any:Proposition).Any" *)

let test_print_inductive (name: string) =
  let ctx = Context.push_local "x" (Term.any) (Standard_context.make ()) in
  let gamma = Context.gamma ctx in
  let i::_ = Context.find_name name ctx in
  let level = i in(*Context.level_of_index i ctx in *)
  let Some(ind) = Gamma.inductive_at_level level gamma in
  let typ = Gamma.type_at_level level gamma in
  let raw = Gamma.raw_type_at_level level gamma in
  printf "%s\n" (Print_inductive.string_of_inductive ind gamma);
  printf "%s\n" (Gamma.name_at_level level gamma);
  printf "%i\n" i;
  printf "%s\n" (Term_printer.string_of_term typ gamma);
  printf "%s\n" (Term_printer.string_of_term raw gamma);
  let entry = Gamma.entry ((Gamma.count gamma)-1) gamma in
  printf "%s\n" entry.name;;
  

test_print_inductive "Decision"
