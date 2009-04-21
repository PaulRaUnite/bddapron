(** Normalized condition environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

class type ['a,'b,'c] t = object('d)
  constraint 'b = < bddindex : int; bddindex0 : int; .. >

  method permutation : int array
    (** Compute the permutation for normalizing the environment *)
  method permute : int array -> unit
    (** Apply the given permutation to the environment *)
  method normalize : int array
    (** Combine the two previous functions, and return the permutation *)

  method compute_careset : normalized:bool -> unit
    (** Computes the careset, using [self#compare_cond] *)
  method reduce : 'c Cudd.Bdd.t -> unit
    (** Remove from the environment all conditions that do not
	belong to the given support. Does not perform
	normalization (so there may be "holes" in the allocation
	of indices *)
  method clear : unit
    (** Clear all the conditions (results in a normalized environments) *)

  method cond_of_idb : int*bool -> 'a
  method idb_of_cond : 'b -> 'a -> int*bool

  val v_compare_cond : 'a -> 'a -> int
  val v_negate_cond : 'b -> 'a -> 'a
  val v_support_cond : 'b -> 'a -> string PSette.t
  val mutable v_print_cond : 'b -> Format.formatter -> 'a -> unit
  val v_cudd : 'c Cudd.Man.t
    (** CUDD manager *)

  val mutable v_bddindex0 : int
    (** First index for finite-type variables *)
  val mutable v_bddsize : int
    (** Number of indices dedicated to finite-type variables *)
  val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
  val v_bddincr : int
  val mutable v_cond : ('a,int*bool) PDMappe.t
    (** Two-way association between a condition and a pair of a BDD index and a polarity *)
  val mutable v_cond_supp : 'c Cudd.Bdd.t
    (** Support of conditions *)
  val mutable v_careset : 'c Cudd.Bdd.t
    (** Boolean formula indicating which logical
	combination known as true could be exploited for simplification.
	For instance, [x>=1 => x>=0]. *)

  method compare_cond : 'a -> 'a -> int
  method negate_cond : 'b -> 'a -> 'a
  method support_cond : 'b -> 'a -> string PSette.t
  method print_cond : 'b -> Format.formatter -> 'a -> unit

  method cudd : 'c Cudd.Man.t
  method bddindex0 : int
  method bddsize : int
  method bddindex : int
  method bddincr : int
  method cond : ('a,int*bool) PDMappe.t
  method cond_supp : 'c Cudd.Bdd.t
  method careset : 'c Cudd.Bdd.t

  method set_bddindex0 : int -> unit
  method set_bddsize : int -> unit
  method set_bddindex : int -> unit
  method set_cond : ('a,int*bool) PDMappe.t -> unit
  method set_cond_supp : 'c Cudd.Bdd.t -> unit
  method set_careset : 'c Cudd.Bdd.t -> unit
end

class ['a,'b,'c] make :
  ?bddindex0:int ->
  ?bddsize:int ->
  'c Cudd.Man.t ->
  compare_cond:('a -> 'a -> int) ->
  negate_cond:('b -> 'a -> 'a) ->
  support_cond:('b -> 'a -> string PSette.t) ->
  print_cond:('b -> Format.formatter -> 'a -> unit) ->
  ['a,'b,'c] t

val make :
  ?bddindex0:int ->
  ?bddsize:int ->
  'c Cudd.Man.t ->
  compare_cond:('a -> 'a -> int) ->
  negate_cond:('b -> 'a -> 'a) ->
  support_cond:('b -> 'a -> string PSette.t) ->
  print_cond:('b -> Format.formatter -> 'a -> unit) ->
  ('a,'b,'c) t

val print : 'b -> Format.formatter -> ('a,'b,'c) #t -> unit

val check_normalized : 'b -> ('a,'b,'c) #t -> bool
val is_leq : ('a,'b1,'c) #t -> ('a,'b2,'c) #t -> bool
val is_eq : ('a,'b1,'c) #t -> ('a,'b2,'c) #t -> bool
val shift : (('a,'b,'c) #t as 'd) -> int -> 'd
val lce : (('a,'b,'c) #t as 'd) -> 'd -> 'd
val permutation12 : ('a,'b1,'c) #t -> ('a,'b2,'c) #t -> int array
val permutation21 : ('a,'b1,'c) #t -> ('a,'b2,'c) #t -> int array

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
    
