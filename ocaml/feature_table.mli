(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Support
open Term
open Signature

(** Structure that implements a table of all global features in the system *)

type t

type implementation_status = No_implementation | Builtin | Deferred

val count:      t -> int
    (** The number of features in the table *)

val count_fgs:  int -> t -> int
    (** [count_fgs i ft] returns the number of formal generics of the feature [i] *)

val anchor: int -> t -> int
    (** [anchor i ft] returns the formal generic which serves as an anchor for
        inheritance. Raises [Not_found] if there is no unique anchor *)

val variant: int -> int -> t -> int
    (** [variant idx cls ft] returns the variant of the feature [idx] in the
        class [cls] *)

val variant_term: term -> int -> int -> t -> term
    (** [variant t nb cls ft] returns the variant of the term [t] with [nb]
        bound variables in the class [cls] *)

val string_of_signature: int -> t -> string

val implication_index: int
val all_index:         int
val some_index:        int

val base_table: unit -> t

val class_table:  t -> Class_table.t

val has_current_module: t -> bool
    (** Is there a current module? *)

val current_module:  t -> int
    (** The current module *)

val count_modules: t -> int
    (** The number of modules in the system *)

val find_module: int -> int list -> t -> int
    (** [find_module name lib ft] finds the module [lib.name] in [ft] *)

val module_name: int -> t -> string
    (** [module_name mdl ft] returns the name of the module [mdl] *)

val used_modules: int -> t -> IntSet.t
  (** [used_modules mdl ft] returns the used modules of module [mdl] *)

val add_used_module:    int -> int list -> IntSet.t -> t -> unit
val add_current_module: int -> IntSet.t -> t -> unit
val set_interface_check: IntSet.t -> t -> unit

val is_private: t -> bool
   (** Are we within the implementation of a module? *)

val is_public:  t -> bool
   (** Are we either using or  checking an interface? *)

val is_interface_check:  t -> bool
   (** Are we checking an interface? *)

val is_interface_use:  t -> bool
   (** Are we using an interface? *)



val implication_term: term -> term -> int -> t -> term

val expand_focus_term: term -> int -> t -> term
    (** [expand_focus_term t nb ft] expands the variable in the focus of [t]
        within an environment with [nb] bound variables (i.e. a variable [i]
        with [nb<=i] refers to the global feature [i-nb])

        Note: The function doesn't do any beta reductions in the term [t]
        which would have been possible before the expansion. *)


val expand_term: term->int->t->term
  (** [expand_term t nbound ft] expands the definitions of the term [t] within
      an environment with [nbound] bound variables, i.e. a variable [i] with
      [nbound<=i] refers to the global feature [i-nbound]

      Note: [expand_term] doesn't do any beta reductions in the term [t] which
      would have been possible before the expansion. *)


val normalize_term: term->int->t->term
  (** [normalize_term t nbound ft] expands the definitions of the term [t] and
     beta reduce it within an environment with [nbound] bound variables,
     i.e. a variable [i] with [nbound<=i] refers to the global feature
     [i-nbound] *)


val find_funcs: feature_name -> int -> t -> (int * Tvars.t * Sign.t) list
  (** [find_funcs fn nargs ft] finds all functions with name [fn] and [nargs]
      arguments in the feature table [ft] and returns the indices with the
      corresponding type variables and signatures. The signatures will be up-
      or downgraded if necessary and possible to match the requirement to have
      [nargs] arguments. *)


val put_function:
    feature_name withinfo -> Tvars.t -> int array
      -> Sign.t -> implementation_status -> term option -> t -> unit
  (** [put_function fn tvs fnms sign imp_stat term_opt ft] adds the function
      with then name [fn], the formal generics of [tvs], the arguments [fnms],
      the signature [sign] the implementation status [imp_stat] and an
      optional definition term [term_opt] to the feature table [ft] *)


val term_to_string: term -> int array -> t -> string

val do_inherit: int -> (int * type_term array) list -> info -> t -> unit
  (** [do_inherit cls anc_lst info ft] let the class [cls] inherit the
      features from all ancestors [par_idx[par_args]] in the ancestor list
      [anc_lst]. *)

val export_inherited: int -> (int * type_term array) list  -> t -> unit
  (** [export_inherited cls anc_lst ft] let the class [cls] export all
      inherited features from all ancestors [par_idx[par_args]] in the
      ancestor list [anc_lst]. *)


val print: t -> unit
