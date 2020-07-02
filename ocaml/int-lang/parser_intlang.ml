open Alba_core
open Printf
open Fmlib.Module_types
open Fmlib.Common




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

    let one_or_more_separated_backtrackable (p: 'a t) (sep: _ t): 'a list t =
      return (fun a l -> a :: l)
      |= p
      |= zero_or_more (backtrackable (sep >>= fun _ -> p) "zero_or_more")

    let zero_or_more_separated_backtrackable (p: 'a t) (sep: _ t): 'a list t =
      one_or_more_separated_backtrackable p sep
      <|> return []

    let whitespace_char: char t =
      one_of_chars " \n\t" "whitespace_char"
    
    let whitespace: unit t =
      get_position >>= fun p ->
      return (printf "whitespace in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->
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
      one_or_more_separated_backtrackable
        (typed_var () >>= fun (v,t) ->
         update (Alba_core.Context.push_local v t) >>= fun _ ->
         return (v,t))
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
      get_position >>= fun p ->
      return (printf "typed_var in column: %d\n" (Fmlib.Character_parser.Position.column p)) >>= fun _ ->
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


