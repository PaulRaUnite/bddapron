(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type typ = [
  | Bdd.Env.typ
  | Apronexpr.typ
]
  (** Types *)

type typdef = Bdd.Env.typdef
  (** Type definitions *)

type cond = ApronexprDD.cond
  (** Conditions *)


val compare_cond :
  [< `Apron of Apronexpr.Condition.t ] ->
  [< `Apron of Apronexpr.Condition.t ] -> int
val negate_cond : [> Apronexpr.typ ] #Apronexpr.db -> cond -> cond
val cond_support : 'a -> [< `Apron of Apronexpr.Condition.t ] -> string PSette.t
  
(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)
module O : sig
  class type ['a,'b,'c] t = object
    inherit [[> typ] as 'a,[> typdef] as 'b,[> cond] as 'c,Cudd.Man.v] Bdd.Env.O.t
    val mutable v_apron_env : Apron.Environment.t
    method apron_env : Apron.Environment.t
    method set_apron_env : Apron.Environment.t -> unit

    method rename_vars_apron : (string * string) list -> int array option * Apron.Dim.perm option
  end

  class ['a,'b] make :
    ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
    ['a, 'b, cond] t

  val make :
    ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
    ('a, 'b, cond) t
      
  val print : Format.formatter -> (typ, typdef, [>  ]) #t -> unit
  val unify : (('a,'b,'c) #t as 'd) -> 'd -> 'd
end


(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type t = (typ,typdef,cond) O.t
  (** Environments *)

val make :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t -> t
    (** Create an environment with a CUDD manager *)

(** {3 Functional version of some methods} *)

val add_typ : (('a,'b,'c) #O.t as 'e) -> string -> 'b -> 'e
val add_vars : (('a,'b,'c) #O.t as 'e) -> (string * 'a) list -> 'e
val remove_vars : (('a,'b,'c) #O.t as 'e) -> string list -> 'e
val rename_vars : (('a,'b,'c) #O.t as 'e) -> (string * string) list -> 'e

val unify : t -> t -> t
    (** Unify environments (non-disjoint union of environments) *)

(** {3 Printing functions} *)

val print_typ : Format.formatter -> [< typ ] -> unit
val print_typdef : Format.formatter -> [< typdef ] -> unit
val print_cond : 'a -> Format.formatter -> [< cond ] -> unit
val print : Format.formatter -> t -> unit

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a,'b) value = ('a,'b) Bdd.Env.value = {
  env : 'a;
  value : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
  (** Constructor *)
