(**

    [Gamma_holes] is a context with holes which can be filled later. A hole is a
    local unnamed variable with a type (i.e. an assumption that an element with
    this type exists), initially without value. Later on the value can be
    provided.

*)

open Fmlib
open Common


type t


val context: t -> Gamma.t
(** The current context with holes. *)


val base_context: t -> Gamma.t
(** The initial context without holes. *)


val count: t -> int
(** The size of the context. *)


val count_base: t -> int
(** The size of the base context. *)


val count_bounds: t -> int
(** The number of bound variables which have been entered. *)


val count_locals: t -> int
(** The number of holes and bound variable which have been entered. *)


val index_of_level: int -> t -> int



val definition_term: int -> t -> Term.t option



val is_hole: int -> t -> bool
(** Is the variable a hole? *)


val is_bound: int -> t -> bool
(** Is the variable a bound variable? *)


val bound_number: int -> t -> int
(**
    [bound_number idx gh]

    Return the number of the bound variable at [idx].

    Precondition:
    {[is_bound idx gh]}
*)





val variable_of_bound: int -> t -> Term.t
(**
    [variable_of_bound i gh]

    Compute the variable corresponding to the [i]th bound variable.

    Precondition:
    {[i < count_bounds gh]}
*)




val has_value: int -> t -> bool
(** Has the variable a value i.e. is it a hole with value. *)


val value: int -> t -> Term.t option
(** The optional value of the hole. *)


val unfilled_holes: int -> Term.t -> t -> Int_set.t
(** [unfilled_holes cnt0 term gh]

    List of unfilled holes in [term] starting at level [cnt0].

    The returned list contains the De Bruijn levels of the unfilled holes.

    Precondition:
    {[cnt0 <= count gh]}

*)





val expand: Term.t -> t -> Term.t
(** [expand term gh] Replace all holes in [term] with its values, if
available. *)




val is_expanded: Term.t -> t -> bool
(**
    [is_expanded term gh] Is the term [term] expanded i.e. does it not contain
    any filled holes?
*)



val term_of_term_n: Term.t_n -> t -> Term.t
(**
    [term_of_term_n tn gh]

    Lift the term [tn] into the context and expand it.
*)



val fill_hole: int -> Term.t -> t -> t
(** [fill_hole idx value gh] Fill the hole at [idx] with [value].

    Preconditions:
    {[is_hole idx gh
      not (has_value idx gh)
      is_expanded value gh
    ]}
*)


val push_hole: Term.typ -> t -> t
(** [push_hole typ gh] Add a hole of type [typ] to [gh]. *)


val push_bound: string -> bool -> Term.typ -> t -> t
(** [push_bound name is_typed gh] adds a bound variable to the context. The
bound variable can be later used to construct binders like [Pi (arg_tp, res_tp,
info] or [Lambda (arg_tp, exp, info]. [is_typed] is used to construct the
binder. *)


val push_local: string -> Term.typ -> t -> t
(** [push_local name typ gh] is synonym for [push_bound name true gh] *)



val type_at_level: int -> t -> Term.typ


val type_of_variable: int -> t -> Term.typ
(**
    [type_of_variable idx gh]

    Return the expanded type of the variable [idx].
*)


val type_of_literal: Term.Value.t -> t -> Term.typ




val pi: int -> int -> Term.typ -> t -> Term.typ
(** [pi cnt0 bnd0 result_tp gh]

    Compute a product type with [result_tp] using the bound variables starting
    from [bnd0].

    {[all (a: A) (b: B) ... : RT]}

    Preconditions:
    {[cnt0   <= count gh
      0      <=  bnd0
      bnd0   <  count_bounds gh
      cnt0   <= level_of_bound (bnd0)]}
    and [A, B, ..., RT] do not contain unfilled holes starting at level [cnt0].
*)

val lambda: int -> int -> Term.t -> t -> Term.t
(** [lambda cnt0 bnd0 exp gh]

    Compute a function term with the inner expression [exp] using the bound
    variables starting from [bnd0].

    {[\ (a: A) (b: B) ... := exp]}

    Preconditions:
    {[cnt0   <= count gh
      0      <=  bnd0
      bnd0   <  count_bounds gh
      cnt0   <= level_of_bound (bnd0)]}
    and [A, B, ..., exp] do not contain unfilled holes starting at level [cnt0].

*)


val make: Gamma.t -> t
