(** Normalized condition environments (base module) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Datatypes } *)
(*  ********************************************************************** *)

type ('a,'b,'c) t = {
  compare_cond : 'a -> 'a -> int;
  negate_cond : 'b -> 'a -> 'a;
  support_cond : 'b -> 'a -> string PSette.t;
  mutable print_cond : 'b -> Format.formatter -> 'a -> unit;

  cudd : 'c Cudd.Man.t;
    (** CUDD manager *)
  mutable bddindex0 : int;
    (** First index for conditions *)
  mutable bddsize : int;
    (** Number of indices dedicated to conditions *)
  mutable bddindex : int;
    (** Next free index in BDDs used by {!idb_of_cond}. *)
  bddincr : int;
  mutable condidb : ('a,int*bool) PDMappe.t;
    (** Two-way association between a condition and a pair of a
	BDD index and a polarity *)
  mutable supp : 'c Cudd.Bdd.t;
    (** Support of conditions *)
  mutable careset : 'c Cudd.Bdd.t;
    (** Boolean formula indicating which logical combination known
	as true could be exploited for simplification.  For
	instance, [x>=1 => x>=0]. *)
}

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

val print : 'b -> Format.formatter -> ('a, 'b, 'c) t -> unit

(*  ********************************************************************** *)
(** {2 Constructors} *)
(*  ********************************************************************** *)

val make :
  ?bddindex0:int ->
  ?bddsize:int ->
  'c Cudd.Man.t ->
  compare_cond:('a -> 'a -> int) ->
  negate_cond:('b -> 'a -> 'a) ->
  support_cond:('b -> 'a -> string PSette.t) ->
  print_cond:('b -> Format.formatter -> 'a -> unit) -> ('a, 'b, 'c) t

val copy : ('a, 'b, 'c) t -> ('a, 'b, 'c) t

(*  ********************************************************************** *)
(** {2 Internal functions} *)
(*  ********************************************************************** *)

val permutation : ('a, 'b, 'c) t -> int array
    (** Compute the permutation for normalizing the environment *)
val permute_with : ('a, 'b, 'c) t -> int array -> unit
    (** Apply the given permutation to the environment *)
val normalize_with : ('a, 'b, 'c) t -> int array
    (** Combine the two previous functions, and return the permutation *)
val reduce_with : ('a, 'b, 'c) t -> 'c Cudd.Bdd.t -> unit
    (** Remove from the environment all conditions that do not
	belong to the given support. Does not perform
	normalization (so there may be "holes" in the allocation
	of indices *)
val clear : ('a, 'b, 'c) t -> unit
    (** Clear all the conditions (results in a normalized environments) *)
val check_normalized : 'a -> ('b, 'a, 'c) t -> bool

(*  ********************************************************************** *)
(** {2 Operations} *)
(*  ********************************************************************** *)

val cond_of_idb : ('a, 'b, 'c) t -> int * bool -> 'a
val idb_of_cond : 'a -> ('b, 'a, 'c) t -> 'b -> int * bool
val compute_careset : ('a, 'b, 'c) t -> normalized:bool -> unit
val is_leq : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool
val is_eq : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool
val shift : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) t
val lce : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
val permutation12 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int array
val permutation21 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int array

(*  ********************************************************************** *)
(** {2 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}
val make_value : 'a -> 'b -> ('a,'b) value
