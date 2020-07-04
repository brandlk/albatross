open Alba_core
open Albalib
open Printf
open Fmlib

let print_type (name: string) =
  let ctx = Standard_context.make () in
  let gamma = Context.gamma ctx in
  let level::_ = Context.find_name name ctx in
  let typ = Gamma.type_at_level level gamma in
  printf "%s\n" (Term_printer.string_of_term typ gamma);;

let print_definition (name: string) =
  let ctx = Standard_context.make () in
  let gamma = Context.gamma ctx in
  let level::_ = Context.find_name name ctx in
  let index = Context.index_of_level level ctx in
  let Some(def_term) = Gamma.definition_term index gamma in
  printf "%s\n" (Term_printer.string_of_term def_term gamma);;

let test_print_inductive (name: string) =
  let ctx = Standard_context.make () in
  let gamma = Context.gamma ctx in
  let level::_ = Context.find_name name ctx in
  let Some(ind) = Gamma.inductive_at_level level gamma in
  let typ = Gamma.type_at_level level gamma in
  let (c0,t0) = Inductive.constructor 0 0 ind in
  let (c1,t1) = Inductive.constructor 0 1 ind in
  printf "%s\n" (Print_inductive.string_of_inductive ind gamma);
  printf "type: %s\n" (Term_printer.string_of_term typ gamma);
  printf "constructors:\n%s:%s\n%s:%s\n"
    c0 (Term_printer.string_of_term t0 gamma)
    c1 (Term_printer.string_of_term t1 gamma);;


let test_build () =
  let params = Array.of_list [("A",Term.any)] in
  let positive_params = Common.Int_set.empty in

  let nprevious = 0 in
  let name = "Mylist" in
  let kind = Term.any in
  let indices = Array.of_list [] in
  let sort = Term.any in
  (* let header = Inductive.Header.make name kind indices sort in *)
  (* let c0 = Inductive.Header.make "empty" (Term.application (Term.variable *) 
  ();;

test_print_inductive "List";

