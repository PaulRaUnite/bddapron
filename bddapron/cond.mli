(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type cond = [`Apron of Apronexpr.Condition.t]
  (** Conditions *)

val print_cond : 'a -> Format.formatter -> [< cond ] -> unit
val compare_cond :
  [< `Apron of Apronexpr.Condition.t ] ->
  [< `Apron of Apronexpr.Condition.t ] -> int
val negate_cond : ('a,'b,'c) Env.O.t -> cond -> cond
val support_cond : 'a -> [< `Apron of Apronexpr.Condition.t ] -> string PSette.t

module O : sig
  type 'a t = (cond,'a,Cudd.Man.v) Bdd.Cond.t
  constraint 'a = ('b,'c,'d) Env.O.t

  val make :
    ?bddindex0:int ->
    ?bddsize:int ->
    Cudd.Man.vt -> 'a t
end

type t = Env.t O.t

val make :
  ?bddindex0:int ->
  ?bddsize:int ->
  Cudd.Man.vt -> t

val copy : 'a O.t -> 'a O.t

val print : 'a -> Format.formatter -> 'a O.t -> unit
val cond_of_idb : 'a O.t -> int*bool -> cond
val idb_of_cond : 'a -> 'a O.t -> cond -> int*bool
val check_normalized : 'a -> 'a O.t -> bool
val is_leq : 'a O.t -> 'a O.t -> bool
val is_eq : 'a O.t -> 'a O.t -> bool
val shift : 'a O.t -> int -> 'a O.t
val lce : 'a O.t -> 'a O.t -> 'a O.t
val permutation12 : 'a O.t -> 'a O.t -> int array
val permutation21 : 'a O.t -> 'a O.t -> int array

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
