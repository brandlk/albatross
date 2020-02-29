open Fmlib
open Common

module Algo = Gamma_algo.Make (Gamma_holes)
module Uni  = Unifier.Make (Gamma_holes)


module Stack =
struct
    let split (stack: 'a list): 'a * 'a list =
        match stack with
        | [] ->
            assert false (* Illegal call! *)
        | hd :: tl ->
            hd, tl
end


type entry = {
    cnt0: int;
    bnd0: int;
}

type t = {
    gh: Gamma_holes.t;
    sp: int;
    stack: int list;
    entry: entry;
    entries: entry list;
}


let index_of_level (level: int) (bc: t): int =
    Gamma_holes.index_of_level level bc.gh


let string_of_term (term: Term.t) (bc: t): string =
    Term_printer.string_of_term term (Gamma_holes.context bc.gh)
let _ = string_of_term



let count (bc: t): int =
    Gamma_holes.count bc.gh

let count_base (bc: t): int =
    Gamma_holes.count_base bc.gh


let count_locals (bc: t): int =
    Gamma_holes.count_locals bc.gh


let count_bounds (bc: t): int =
    Gamma_holes.count_bounds bc.gh


let type_at_level (level: int) (bc: t): Term.typ =
    Gamma_holes.type_at_level level bc.gh


let type_of_term (term: Term.t) (bc: t): Term.typ =
    Algo.type_of_term term bc.gh



let key_normal (term: Term.t) (bc: t): Term.t =
    Algo.key_normal term bc.gh


let is_kind (typ: Term.typ) (bc: t): bool =
    Algo.is_kind typ bc.gh



let required_type (bc: t): Term.typ =
    type_at_level bc.sp bc



let term_at_level (level: int) (bc: t): Term.t =
    Gamma_holes.expand
        (Term.Variable (index_of_level level bc))
        bc.gh




let top_term (bc: t): Term.t =
    term_at_level bc.sp bc



let add_one_implicit
    (term: Term.t) (typ: Term.typ) (bc: t)
    : (Term.t * Term.typ * t) option
    =
    let open Term in
    match typ with
    | Pi (arg, res, _) when is_kind arg bc && has_variable 0 res ->
        Some (
            Appl (
                up1 term,
                Variable 0,
                Application_info.Implicit),
            res,
            {bc with gh = Gamma_holes.push_hole arg bc.gh}
        )
    | _ ->
        None






let add_implicits
    (_: Term.t) (_: Term.typ) (_:t)
    : Term.t * Term.typ * t
    =
    assert false




let unify (act: Term.typ) (req: Term.typ) (bc: t): t option =
    Printf.printf "unify %s with %s\n"
        (string_of_term act bc) (string_of_term req bc);
    Option.map
        (fun gh -> {bc with gh})
        (Uni.unify act req true bc.gh)



let unify_plus (term: Term.t) (bc: t): (Term.t * t) option =
    let rec uni term tp bc =
        let tp = key_normal tp bc
        and req = required_type bc in
        match Uni.unify tp req true bc.gh with
        | Some gh ->
            Some (Gamma_holes.expand term gh, {bc with gh})
        | None ->
            Option.(add_one_implicit term tp bc
                    >>= fun (term, tp, bc) ->
                    uni term tp bc)
    in
    uni term (type_of_term term bc) bc




let set_term (term: Term.t) (bc: t): t =
    {bc with
        gh =
            Gamma_holes.fill_hole (index_of_level bc.sp bc) term bc.gh}



let make (gamma: Gamma.t): t =
    let cnt0 = Gamma.count gamma in
    {
        gh =
            Gamma_holes.(
                make gamma
                |> push_hole Term.(any_uni 2)
                |> push_hole Term.(Variable 0)
            );

        sp = cnt0 + 1;

        stack = [];

        entry = {
            cnt0;
            bnd0 = 0;
        };

        entries = [];
   }


let final
    (bc: t)
    : (Term.t * Term.typ, Term.t list * Term.t * Term.typ * Gamma.t) result
    =
    let cnt0 = count_base bc
    and nlocs = count_locals bc in
    assert (bc.stack = []);
    assert (bc.entries = []);
    assert (bc.sp = cnt0 + 1);
    let term = top_term bc in
    let typ  = type_of_term term bc in
    match Term.down nlocs term, Term.down nlocs typ with
    | Some term, Some typ ->
        Ok (term, typ)
    | _ ->
        assert false (* nyi: find the unresolved holes. *)




let candidate (term: Term.t) (nargs: int) (bc: t): t option =
    assert (nargs = 0);
    if 0 < nargs then
        let tp = type_of_term term bc in
        let term, tp, bc = add_implicits term tp bc in
        Option.map
            (set_term term)
            (unify tp (required_type bc) bc)
    else
        (* Missing: adding of implicit arguments!!! *)
        match unify_plus term bc with
        | None ->
            None
        | Some (term, bc) ->
            Some (set_term term bc)



let base_candidate
    (term: Term.t)
    (nargs: int)
    (bc: t)
    : t option
    =
    let term = Term.up (count_locals bc) term in
    candidate term nargs bc



let bound (ibound: int) (nargs: int) (bc: t): (t, Term.typ) result =
    match
        candidate (Gamma_holes.variable_of_bound ibound bc.gh) nargs bc
    with
    | None ->
        Error (required_type bc)
    | Some bc ->
        Ok bc







module Product =
(* ... A: Any1, x: A, B: Any1, y: B, ... , RT: Any1 *)
struct
    let start (bc: t): t =
        let cnt0 = count bc
        and bnd0 = count_bounds bc
        in
        {
            gh = Gamma_holes.push_hole Term.(any_uni 1) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0;

            entries = bc.entry :: bc.entries;

            entry = {cnt0; bnd0}
        }

    let next (name: string) (typed: bool) (bc: t): t =
        let cnt0 = count bc
        and tp = top_term bc
        in
        {bc with
            gh = Gamma_holes.(
                push_bound name typed tp bc.gh
                |> push_hole Term.(any_uni 1)
            );

            stack = bc.sp :: bc.stack;

            sp = cnt0 + 1
        }

    let end_ (nargs: int) (bc: t): (t, int) result =
        let res = top_term bc
        and arg_tps, stack =
            let rec args nargs stack tps =
                if nargs = 0 then
                    tps, stack
                else
                    let sp, stack = Stack.split stack in
                    args (nargs - 1) stack (term_at_level sp bc :: tps)
            in
            args nargs bc.stack []
        in
        let rec find_incomplete i tps =
            if i = nargs then
                None
            else
                let tp, tps = Stack.split tps in
                if Int_set.is_empty
                    (Gamma_holes.unfilled_holes bc.entry.cnt0 tp bc.gh)
                then
                    find_incomplete (i + 1) tps
                else
                    Some i
        in
        match find_incomplete 0 arg_tps with
        | Some i ->
            Error i
        | None ->
            let sp, stack = Stack.split stack in
            let tp = Gamma_holes.pi bc.entry.cnt0 bc.entry.bnd0 res bc.gh in
            let entry, entries = Stack.split bc.entries in
            Ok (set_term tp
                    {   gh = Gamma_holes.remove_bounds nargs bc.gh;
                        sp;
                        stack;
                        entry;
                        entries})
end


module Typed =
struct
    let start (bc: t): t =
        let cnt0 = count bc in
        {bc with
            gh = Gamma_holes.push_hole Term.(any_uni 1) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0
        }

    let expression (bc: t): t =
        let cnt0 = count bc in
        {bc with
            gh = Gamma_holes.push_hole (top_term bc) bc.gh;

            stack = bc.sp :: bc.stack;

            sp = cnt0;
        }

    let end_ (nargs: int) (bc: t): (t, unit) result =
        let tp, stack = Stack.split bc.stack in
        let sp, stack = Stack.split stack in
        let tp = term_at_level tp bc
        and exp = top_term bc
        in
        let term = Term.Typed (exp, tp)
        and bc = {bc with sp; stack} in
        match candidate term nargs bc with
        | None ->
            assert false (* nyi *)
        | Some bc ->
            Ok bc

end
