open Fmlib
open Common


module Pi_info = Term.Pi_info

module Lambda_info = Term.Lambda_info



type definition =
  | No
  | Builtin of Term.Value.t
  | Definition of Term.t


type entry = {
    name: string;
    typ: Term.typ;
    definition: definition
  }


type t = entry Segmented_array.t


let bruijn_convert (i:int) (n:int): int =
  n - i - 1



let count (c:t): int =
  Segmented_array.length c



let is_valid_index (i:int) (c:t): bool =
  0 <= i && i < count c


let index_of_level (i:int) (c:t): int =
  bruijn_convert i (count c)


let level_of_index (i:int) (c:t): int =
  bruijn_convert i (count c)


let entry (i:int) (c:t): entry =
  assert (is_valid_index i c);
  Segmented_array.elem i c


let raw_type_at_level (i:int) (c:t): Term.typ =
  (entry i c).typ


let type_at_level (i:int) (c:t): Term.typ =
  let cnt = count c in
  Term.up (cnt - i) (entry i c).typ



let variable_at_level (i:int) (c:t): Term.t =
    Term.Variable (index_of_level i c)



let term_at_level (i:int) (c:t): Term.t =
    variable_at_level i c



let name_of_level (i:int) (c:t): string =
    (entry i c).name


let name_of_index0 (i:int) (c:t): string =
  (entry (bruijn_convert i (count c)) c).name


let name_at_level (level: int) (gamma: t): string =
    (Segmented_array.elem level gamma).name



let name_of_index (i: int) (gamma: t): string =
  name_of_index0 i gamma



let empty: t =
  Segmented_array.empty


let push (name: string) (typ:Term.typ) (definition:definition) (c:t): t =
    Segmented_array.push
      {name; typ; definition}
      c


let push_local (nme: string) (typ: Term.typ) (c:t): t =
  push nme typ No c


let push_unnamed (typ: Term.typ) (c: t): t =
  push_local " " typ c


let remove_last (n: int) (c: t): t =
  Segmented_array.remove_last n c


let add_entry (name: string) (typ:Term.typ*int) (def:definition) (c:t): t =
  let typ,n = typ
  and cnt = count c
  in
  assert (n <= cnt);
  let typ = Term.up (cnt - n) typ
  in
  push name typ def c


let int_level    = 0
let char_level   = 1
let string_level = 2
let eq_level     = 8


let binary_type (level:int): Term.typ * int =
  Pi (Variable 0,
      Pi (Variable 1,
          Variable 2,
          Pi_info.arrow),
      Pi_info.arrow),
  (level + 1)


let int_type (c:t) =
  Term.Variable (index_of_level int_level c)


let char_type (c:t) =
  Term.Variable (index_of_level char_level c)


let string_type (c:t) =
  Term.Variable (index_of_level string_level c)


let standard (): t =
  (* Standard context. *)
  let open Term
  in
  empty

  |> add_entry "Int" (Term.any ,0) No

  |> add_entry "Character" (Term.any, 0) No

  |> add_entry "String" (Term.any, 0) No

  |> add_entry
       "+"
       (binary_type int_level)
       (Builtin Term.Value.int_plus)

  |> add_entry
       "-"
       (binary_type int_level)
       (Builtin Term.Value.int_minus)

  |> add_entry
       "*"
       (binary_type int_level)
       (Builtin Term.Value.int_times)

  |> add_entry
       "+"
       (binary_type string_level)
       (Builtin Term.Value.string_concat)

  |> add_entry
       (* List: Any -> Any *)
       "List"
       (Term.(Pi (any, any, Pi_info.arrow)), 0)
       No

  |> add_entry (* 8 *)
       (* (=) (A: Any): A -> A -> Proposition *)
       "="
       (Term.(
          Pi (any,
              Pi (Variable 0,
                  (Pi (Variable 1,
                       proposition,
                       Pi_info.arrow)),
                  Pi_info.arrow),
              Pi_info.typed "A")),
        0)
       No

  |> add_entry
       (* identity: all (A: Any): A -> A :=
            \ A x := x *)
       "identity"
       (Term.(
          Pi (any,
              Pi (Variable 0,
                  Variable 1,
                  Pi_info.arrow),
              Pi_info.typed "A")),
        0)
       (Definition
          (Term.(
             Lambda (any,
                     Lambda (Variable 0,
                             Variable 0,
                             Lambda_info.typed "x"),
                     Lambda_info.typed "A"))))

    |> add_entry
        (* true: Proposition *)
        "true"
        (Term.proposition, 0)
        No

    |> add_entry
        (* false: Proposition *)
        "false"
        (Term.proposition, 0)
        No

    |> (* (=>) (a b: Proposition): Proposition := a -> b *)
       (let typ =
            product "_"
                proposition
                (product "_" proposition proposition)
        and def =
            let a = Variable 0
            and b = Variable 1 in
            to_index 0
                (lambda "a" proposition
                   (lambda "b" proposition
                        (arrow a b)))
        in
        add_entry
            "=>" (typ,0) (Definition def)
        )

    |> (* (|>) (A: Any) (a: A) (B: Any) (f: A -> B): B := f a *)
        (let biga = Variable 0
         and a    = Variable 1
         and bigb = Variable 2
         and f    = Variable 3
         in
         let args = ["A", any;
                     "a", biga;
                     "B", any;
                     "f", arrow biga bigb]
         in
         let typ = product_in args bigb
         and def = lambda_in args (application f a)
         in
         add_entry
            "|>"
            (to_index 0 typ, 0)
            (Definition (to_index 0 def))
        )

    |> (* (<|) (A: Any) (B: Any) (f: A -> B) (a: A): B := f a *)
        (let biga = Variable 0
         and bigb = Variable 1
         and f    = Variable 2
         and a    = Variable 3
         in
         let args = ["A", any;
                     "B", any;
                     "f", arrow biga bigb;
                     "a", biga]
         in
         let typ = product_in args bigb
         and def = lambda_in args (application f a)
         in
         add_entry
            "<|"
            (to_index 0 typ, 0)
            (Definition (to_index 0 def))
        )

    (* leibniz (A: Any) (f: A -> Proposition)
               (a b: A)
               : a = b => f a => f b *)
    |>  (let n = eq_level + 1 in
         let biga = Variable (n + 0)
         and f    = Variable (n + 1)
         and a    = Variable (n + 2)
         and b    = Variable (n + 3)
         and eq   = Variable eq_level
         in
         let args = ["A", any;
                     "f", arrow biga proposition;
                     "a", biga;
                     "b", biga;
                     "eq", binary
                            a
                            (implicit_application eq biga)
                            b;
                     "fa", application f a]
         in
         let typ = product_in args (application f b)
         in
         add_entry
            "leibniz" (to_index n typ, n)
            No
        )



let type_of_literal (v: Term.Value.t) (c: t): Term.typ =
  let open Term in
  match v with
  | Value.Int _ ->
      int_type c

  | Value.Char _ ->
      char_type c

  | Value.String _ ->
      string_type c

  | Value.Unary _ | Value.Binary _ ->
      assert false (* Illegal call! *)




let type_of_variable (i: int) (c: t): Term.typ =
  type_at_level (level_of_index i c) c


let type_of_term (t:Term.t) (c:t): Term.typ =
  let rec typ t c =
    let open Term in
    match t with
    | Sort s ->
        type_of_sort s

    | Value v ->
        type_of_literal v c

    | Variable i ->
        type_of_variable i c

    | Typed (_, tp) ->
       tp

    | Appl (f, a, _) ->
       (match typ f c with
        | Pi (_, rt, _) ->
           apply rt a
        | _ ->
           assert false (* Illegal call! Term is not welltyped. *)
       )

    | Lambda (tp, exp, info) ->
       let c_inner = push_local (Lambda_info.name info) tp c in
       let rt      = typ exp c_inner
       in
       Pi (tp, rt, Pi_info.typed (Lambda_info.name info))

    | Pi (tp, rt, info) ->
       let name = Pi_info.name info in
       (match
          typ tp c, typ rt (push_local name tp c)
        with
        | Sort s1, Sort s2 ->
          let open Sort in
          (match s1, s2 with
            | Proposition, Any i ->
              Sort (Any i)

            | Any i, Any j ->
              Sort (Any (max i j))

            | _, Proposition ->
              Sort Proposition
          )

        | _, _ ->
           assert false (* Illegal call: term is not welltyped! *)
       )
  in
  typ t c



let definition_term (idx: int) (c: t): Term.t option =
  match
    (entry (level_of_index idx c) c).definition
  with
  | Definition def ->
     Some def

  | _ ->
     None



let compute (t:Term.t) (c:t): Term.t =
  let open Term in
  let rec compute term steps c =
    match term with
    | Sort _ | Value _ ->
        term, steps

    | Variable i ->
       (match (entry (level_of_index i c) c).definition with
        | No ->
            term, steps

        | Builtin v ->
           Term.Value v, steps + 1

        | Definition def ->
           def, steps + 1
       )

    | Typed (e, _ ) ->
       compute e steps c

    | Appl (Value f, Value arg, _) ->
        Value (Value.apply f arg), steps + 1

    | Appl (Value f, arg, mode) ->
        let arg, new_steps = compute arg steps c in
        if steps < new_steps then
          compute (Appl (Value f, arg, mode)) new_steps c
        else
          Appl (Value f, arg, mode), steps

    | Appl (Lambda (_, exp, _), arg, _) ->
        compute (apply exp arg) (steps + 1) c

    | Appl (Variable i, arg, mode) ->
      let f, new_steps = compute (Variable i) steps c in
      if steps < new_steps then
        compute (Appl (f, arg, mode)) new_steps c
      else
        term, new_steps

    | Appl (f, arg, mode) ->
        let f, new_steps = compute f steps c in
        if steps < new_steps then
          compute (Appl (f, arg, mode)) new_steps c
        else
          term, new_steps

    | Lambda _ ->
        term, steps

    | Pi (arg_tp, res_tp, info) ->
        let c_inner = push_local (Pi_info.name info) arg_tp c in
        let res_tp, new_steps = compute res_tp steps c_inner in
        if steps < new_steps then
            compute (Pi (arg_tp, res_tp, info)) new_steps c
        else
            term, steps
  in
  fst (compute t 0 c)


let key_split
      (t: Term.t)
      (args: (Term.t * Term.Application_info.t) list)
      (c: t)
    : Term.t * (Term.t * Term.Application_info.t) list
  =
  let rec split t args =
    match t with
    | Term.Variable i ->
       (match definition_term i c with
        | None ->
           t, args
        | Some def ->
           split def args)

    | Term.Appl (Term.Lambda (_, exp, _), arg, _) ->
       split (Term.apply exp arg) args


    | Term.Appl (f, arg, mode) ->
       split f ((arg, mode) :: args)

    | Term.Typed (term, _) ->
        term, args

    | _ ->
       t, args
  in
  split t args


let key_normal (t: Term.t) (c: t): Term.t =
  let key, args = key_split t [] c in
  List.fold_left
    (fun res (arg, mode) ->
      Term.Appl (res, arg, mode))
    key
    args



let rec sort_of_kind (k: Term.typ) (c:t): Term.Sort.t option =
    let open Term
    in
    match key_normal k c with
    | Sort s ->
        Some s
    | Pi (arg_tp, res_tp, _) ->
        sort_of_kind res_tp (push_local "_" arg_tp c)
    | _ ->
        None


let is_kind (k: Term.typ) (c: t): bool =
    Option.has (sort_of_kind k c)





let is_subtype (_: Term.typ) (_: Term.typ) (_: t): bool =
  assert false (* nyi *)





let rec typecheck (term: Term.t) (c: t): Term.typ option =
  let open Term in
  match term with
  | Sort s ->
      Some (type_of_sort s)

  | Value v ->
      Some (type_of_literal v c)

  | Variable i ->
      Some (type_of_variable i c)

  | Appl (f, arg, _ ) ->
      Option.(
        typecheck f c >>= fun f_type ->
        typecheck arg c >>= fun arg_type ->
        let key, args = key_split f_type [] c in
        ( match key, args with
          | Pi (tp, rt, _ ), []  when is_subtype arg_type tp c ->
              Some (apply rt arg)
          | _ ->
              None ))

  | Typed (_, _ ) ->
      assert false (* nyi *)

  | Lambda (_, _, _ ) ->
      assert false (* nyi *)

  | Pi (_, _, _) ->
      assert false (* nyi *)





let add_vars_from (level: int) (t: Term.t) (c: t) (set: Int_set.t): Int_set.t =
  Term.fold_free
    (fun i set ->
      let j = level_of_index i c in
      if j < level then
        set
      else
        Int_set.add j set)
    t
    set



let signature (c: t) (tp: Term.typ): Signature.t =
  let rec split c tp lst =
    match key_normal tp c with
    | Term.Pi (arg_tp, res_tp, _ ) ->
       let c_inner = push_unnamed arg_tp c in
       split c_inner res_tp ((c, arg_tp, tp) :: lst)

    | _ ->
       c, tp, lst
  in
  let c_inner, res_tp, args = split c tp []
  and cnt = count c
  in
  let nargs = count c_inner - cnt
  and set = add_vars_from cnt res_tp c_inner Int_set.empty
  in
  let _, _, sign =
    List.fold_left
      (fun (i, set, sign) (c, arg_tp, tp) ->
        assert (0 < i);
        let i = i - 1 in
        let implicit = Int_set.mem (cnt + i) set
        and set = add_vars_from cnt arg_tp c set
        in
        let sign = Signature.push sign tp arg_tp implicit in
        i, set, sign)
      (nargs, set, Signature.make cnt nargs res_tp)
      args
  in
  sign
