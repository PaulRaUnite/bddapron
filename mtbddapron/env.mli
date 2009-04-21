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

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)
module O : sig
  class type ['a,'b] t = object
    inherit [[> typ] as 'a,[> typdef] as 'b,Cudd.Man.v] Bdd.Env.O.t
    val mutable v_apron_env : Apron.Environment.t
    method apron_env : Apron.Environment.t
    method set_apron_env : Apron.Environment.t -> unit

    method rename_vars_apron : (string * string) list -> int array option * Apron.Dim.perm option
  end

  class ['a,'b] make :
    ?bddindex0:int -> ?bddsize:int ->
    ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
    ['a, 'b] t

  val make :
    ?bddindex0:int -> ?bddsize:int ->
    ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
    ('a, 'b) t
      
  val print : Format.formatter -> (typ, typdef) #t -> unit
  val unify : (('a,'b) #t as 'c) -> 'c -> 'c
end


(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type t = (typ,typdef) O.t
  (** Environments *)

val make :
  ?bddindex0:int -> ?bddsize:int -> 
  ?relational:bool -> Cudd.Man.v Cudd.Man.t -> t
    (** Create an environment with a CUDD manager *)

(** {3 Functional version of some methods} *)

val add_typ : (('a,'b) #O.t as 'e) -> string -> 'b -> 'e
val add_vars : (('a,'b) #O.t as 'e) -> (string * 'a) list -> 'e
val remove_vars : (('a,'b) #O.t as 'e) -> string list -> 'e
val rename_vars : (('a,'b) #O.t as 'e) -> (string * string) list -> 'e

val unify : t -> t -> t
    (** Unify environments (non-disjoint union of environments) *)

(** {3 Printing functions} *)

val print_typ : Format.formatter -> [< typ ] -> unit
val print_typdef : Format.formatter -> [< typdef ] -> unit
val print : Format.formatter -> t -> unit

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a,'b) value = ('a,'b) Bdd.Env.value = {
  env : 'a;
  val0 : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
  (** Constructor *)
