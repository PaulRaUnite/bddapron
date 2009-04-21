(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type cond = [`Apron of Apronexpr.Condition.t]
  (** Conditions *)

val compare_cond :
  [< `Apron of Apronexpr.Condition.t ] ->
  [< `Apron of Apronexpr.Condition.t ] -> int
val negate_cond : [> Apronexpr.typ ] #Apronexpr.env -> cond -> cond
val cond_support : 'a -> [< `Apron of Apronexpr.Condition.t ] -> string PSette.t

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)
module O : sig
  class type ['a,'b] t =
	  [[> cond] as 'a,
	  [> Apronexpr.typ ] #Apronexpr.env as 'b,
	  Cudd.Man.v]
	    Bdd.Cond.t
  class ['a, 'b] make :
    ?bddindex0:int -> ?bddsize:int ->
	     Cudd.Man.v Cudd.Man.t ->
	     compare_cond:('a -> 'a -> int) ->
	     negate_cond:('b -> 'a -> 'a) ->
	     support_cond:('b -> 'a -> string PSette.t) ->
	     print_cond:('b -> Format.formatter -> 'a -> unit) ->
	     ['a, 'b] t
  val make :
    ?bddindex0:int -> ?bddsize:int ->
    Cudd.Man.v Cudd.Man.t ->
    compare_cond:('a -> 'a -> int) ->
    negate_cond:('b -> 'a -> 'a) ->
    support_cond:('b -> 'a -> string PSette.t) ->
    print_cond:('b -> Format.formatter -> 'a -> unit) -> ('a, 'b) t
  val print : 'b -> Format.formatter -> ('a, 'b) #t -> unit
end


(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type t = [cond, Env.t] O.t
class make : ?bddindex0:int -> ?bddsize:int -> Cudd.Man.v Cudd.Man.t -> [cond, Env.t] O.make
val make : ?bddindex0:int -> ?bddsize:int -> Cudd.Man.v Cudd.Man.t -> t
val print : Env.t -> Format.formatter -> t -> unit

val check_normalized : Env.t -> t -> bool
val is_leq : t -> t -> bool
val is_eq : t -> t -> bool
val shift : t -> int -> t
val lce : t -> t -> t
val permutation12 : t -> t -> int array
val permutation21 : t -> t -> int array

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
