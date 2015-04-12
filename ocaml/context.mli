(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
 *)

(** Context with stacked declarations of formal arguments *)


open Signature
open Support
open Term
open Container

type t
val make:  int -> t

val module_table: t -> Module_table.t
val class_table: t  -> Class_table.t
val feature_table:t -> Feature_table.t

val current_module:     t -> int
val used_modules:       int -> t -> IntSet.t
val add_used_module:    (int * int list) -> IntSet.t -> t -> unit
val add_current_module: int -> IntSet.t -> t -> unit
val set_interface_check: IntSet.t -> t -> unit

val is_private:        t -> bool
val is_public:         t -> bool
val is_interface_check:t -> bool
val is_interface_use:  t -> bool
    (** Are we using an interface? *)

val verbosity: t -> int

val find_module:        (int * int list) -> t -> int

val push_with_gap:  entities list withinfo -> return_type -> bool -> bool
  -> int -> t -> t
val push:  entities list withinfo -> return_type -> bool -> bool -> t -> t
val push_untyped_with_gap: int array -> bool -> int -> t -> t
val push_untyped: int array -> t -> t
val pop:   t -> t

val is_global:   t -> bool
val is_toplevel: t -> bool
val depth:       t -> int
val arity:     t -> int
val argument:  int -> t -> int * Tvars.t * Sign.t

val has_result:  t -> bool
val result_type: t -> type_term

val count_type_variables: t -> int
    (** The number of cumulated type variables in this context and all
        preceeding contexts *)

val count_local_type_variables: t -> int
    (** The number of type variables in this context without all preceeding
        contexts *)

val count_formal_generics: t -> int
    (** The number of formal generics in this context and all preceeding
        contexts *)

val count_last_arguments:  t -> int
    (** The number of formal arguments in this context without the preceeding
        contexts *)

val count_last_formal_generics:  t -> int
    (** The number of formal generics in this context without the preceeding
        contexts *)

val count_arguments:  t -> int
    (** The number of formal arguments in this context and all preceeding
        contexts *)


val implication_index: t -> int

val make_lambda:      int -> int array -> term -> bool -> t -> term
val make_application: term -> term array -> int -> bool -> t -> term
val beta_reduce:      int -> term -> term array -> int -> t -> term

val quantified:      bool -> int -> int array -> term -> t -> term
val all_quantified:  int -> int array -> term -> t -> term
val some_quantified: int -> int array -> term -> t -> term


val argument_name: int -> t -> int
    (** The name of the [i]th formal argument *)

val argument_type: int -> t -> type_term
    (** The type of the [i]th formal argument *)

val fgnames: t   -> int array

val local_argnames: t -> int array

val tvs: t -> Tvars.t

val ith_arguments_string: int -> t -> string
val local_arguments_string: t -> string

val type_variables: t -> TVars_sub.t
    (** The type variables and their substitutions *)

val boolean: t -> term

val update_type_variables: TVars_sub.t -> t -> unit

val string_of_term:       term -> bool -> int -> t -> string
val string_of_term_outer: term -> int -> t -> string
val sign2string:    Sign.t -> t -> string
val signature_string: t -> string
val named_signature_string: t -> string
val signature:  t -> Sign.t


val owner:          t -> int
val anchor_class:   t -> int
val check_deferred: t -> unit

val find_identifier: int ->          int -> t -> (int * Tvars.t * Sign.t) list
val find_feature:    feature_name -> int -> t -> (int * Tvars.t * Sign.t) list
val variable_data:   int -> t -> Tvars.t * Sign.t

val put_formal_generic: int withinfo -> type_t withinfo -> t -> unit

val fully_expanded: term -> int -> t -> term

val split_equality: term -> int -> t -> int * int * term * term
val definition: int -> int -> t -> int * int array * term
val expanded_definition: int -> int -> t -> int * int array * term

val preconditions: int -> int -> t -> int * int array * term list
