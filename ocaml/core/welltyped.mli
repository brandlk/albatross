(** Construction of wellformed contexts and welltyped terms. *)


open Fmlib
open Module_types



(** {1 Basics} *)

(** Type of a wellformed context. *)
type context

(** Type of a wellformed context with a term and its type. *)
type judgement



(** Printing of welltyped items. *)
module Print (PP: Pretty_printer.SIG):
sig
    val judgement: judgement -> PP.t
end




val empty: context (** An empty context. *)

(** Extract the context from the encapsulation. *)
val extract_context: context -> Context.t

(** Extract the judgement from the encapsulation. *)
val extract_judgement: judgement -> Context.t * Term.t * Term.typ






(** {1 Term building} *)

(** A builder for welltyped terms in wellformed contexts. *)
module Builder (Info: ANY):
sig
    type name = Info.t * string


    type problem = Info.t * Type_error.t


    (** ['a t] Combinator type building an ['a].

        [term t] is the type of a combinator, building a term

        [defintion t] is the type of a combinator building a definition

        [context t] is the type of a combinator building and adding a global
        declaration to the context
     *)
    type t

    type tl = unit -> t

    (** ['a res] The result of a building process. *)
    type 'a res = ('a, problem) result

    (** [make_term c term] Build the term [term] in context [c]. *)
    val make_term: context -> t -> judgement res


    (** Combinators: Primitive and compound combinators to build terms or build
        and add globals to the context. *)
    module Construct:
    sig
        val sort:
            Info.t -> Sort.t -> t

        val variable:
            Info.t -> int -> t

        val identifier: Info.t -> string -> t
        (** [identifier info name] Build the term represented by [name]. *)


        val unknown: Info.t -> t
        (** Unknown term. The compiler is asked to derive. *)


        val application:
            Info.t -> tl -> tl -> t


        (** [lambda name typ exp] Build the lambda term [\ (name: typ) := exp].
         *)
        val lambda:
            Info.t -> name -> tl -> tl -> t


        (** [pi name typ res] Build the product [all (name: typ): res].
         *)
        val pi:
            Info.t -> name -> tl -> tl -> t
    end
end






(** {1 Type checking} *)

(** A type checker. *)
module Check:
sig
    type 'a res = ('a, Type_error.t) result

    (** [check term context] Check [term] in the wellformed [context] and return
        a judgement, i.e.  a welltyped term with its type in the same context or
        return a type error. *)
    val check_term: Term.t -> context -> judgement res
end
