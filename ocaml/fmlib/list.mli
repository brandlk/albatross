open Common_module_types


(** {1 List Monad}*)

(** A list of values of type ['a]. *)
type 'a t = 'a list


(** [return a] makes a singleton list with the element [a]. *)
val return: 'a -> 'a t


(** [l >>= f] applies the function [f] to all elements of the list [l] and
   concatenates all lists. *)
val (>>=): 'a t -> ('a -> 'b t) -> 'b t


(** [f >=> g] composes the two monadic functions [f] and [g]. *)
val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)


(** [flst <*> lst] is equivalent to [flst >>= fun f -> map f lst] i.e. it maps
   all functions contained in [flst] over the list [lst] and then concatenates
   the results. *)
val (<*>): ('a -> 'b) t -> 'a t -> 'b t

(** [join] is the same as {!val:concat}. *)
val join: 'a list list -> 'a list


(** {1 Modified list functions}*)

(** [find p l] finds an element [e] in the list [l] which satisfies [p e]. *)
val find: ('a -> bool) ->'a t -> 'a option




(** {1 List functions from Stdlib}*)

(** [append a b] prepends the lists [a] in front of the list [b]. Synonym [a @
   b]. *)
val append: 'a list -> 'a list -> 'a list

(** [concat ll] concatenates all lists contained in the list of lists [ll]. *)
val concat: 'a list list -> 'a list



(** [rev a] reverses the list [a]. *)
val rev: 'a list -> 'a list

(** [rev_append a b] prepends the lists [rev a] in front of the list [b]. *)
val rev_append: 'a list -> 'a list -> 'a list

val length: 'a t -> int

val filter: ('a -> bool) -> 'a t -> 'a t

val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val map : ('a -> 'b) -> 'a list -> 'b list

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val rev_map : ('a -> 'b) -> 'a list -> 'b list

val for_all : ('a -> bool) -> 'a list -> bool

val exists : ('a -> bool) -> 'a list -> bool


(** {1 Additional list functions}*)

(** [map_and_filter f list] maps the list with [f] and removes the element for
   which [f e = None].*)
val map_and_filter: ('a -> 'b option) -> 'a list -> 'b list

(** {1 Monadic list folding}*)

(** Monadic list folding *)
module Monadic_fold (M:MONAD):
sig
  (** [fold_left f lst start] leftfolds the function [f] over the list [lst]
     starting with the value [start].  Continuation of the fold is determined
     by the bind operator [>>=] of the monad [M]. E.g. if the monad [M] is
     [Option] the folding stops as soon as [f e acc] returns the value [None].
   {[
        fold_left f [a b c ...] s =
          M.(f a s   >>= fun acc ->
             f b acc >>= fun acc ->
             f c acc >>= fun acc ->
             ...)
     ]}
 *)
  val fold_left:  ('a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t

  (** The same as [fold_left] just right folding.
   {[
        fold_right f [... x y z] s =
        fold_left f (rev [... x y z]) s =
          M.(f z s   >>= fun acc ->
             f y acc >>= fun acc ->
             f x acc >>= fun acc ->
             ...)
   ]}*)
  val fold_right: ('a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t

  (** The same as [fold_left] except that the folding function receives the
     position of the first argument in the list as an additional argument. *)
  val foldi_left: (int -> 'a -> 'b -> 'b M.t) -> 'a t -> 'b -> 'b M.t
  end
