(** Normalized condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
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

val print : Env.t -> Format.formatter -> t -> unit
