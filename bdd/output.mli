(** Output of BDDs/MTBDDs *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** BDD node *)
type bnode =
  | BIte of int * int * bool * int
      (** [BIte(idcond,idnodeThen,signElse,idnodeElse)] *)
  | BTrue
      (** Terminal case. Not needed in principle *)

(** Database *)
type 'a bdd = {
  cond : SetteI.t ref;
  mutable bdef : bnode MappeI.t;
  bhash : ('a Cudd.Bdd.t, int) Hashhe.t;
  mutable blastid : int;
}

(** MTBDD node *)
type 'a mnode = 
  | MIte of int * int * int 
      (** MIte(idcond,idnodeThen,idnodeElse) *)
  | MCst of 'a

(** Database *)
type 'a mtbdd = {
  cond : SetteI.t ref;
  mutable mdef : 'a mnode MappeI.t;
  lhash : ('a Cudd.Mtbdd.unique, unit) PHashhe.t;
  mhash : ('a Cudd.Mtbdd.t, int) Hashhe.t;
  mutable mlastid : int;
}

(** RDD node *)
type rnode = 
  | RIte of int * int * int 
      (** RIte(idcond,idnodeThen,idnodeElse) *)
  | RCst of float

(** Database *)
type rdd = {
  cond : SetteI.t ref;
  mutable rdef : rnode MappeI.t;
  mutable lset : float Sette.t;
  rhash : (Cudd.Rdd.t, int) Hashhe.t;
  mutable rlastid : int;
}

(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)
val make_bdd : cond:SetteI.t ref -> 'a bdd
      (** Create a database for printing BDDs

	  [cond] allows to share the same set of conditions between
	  BDDs and MTBDDs. *)

val signid_of_bdd : 'a bdd -> 'a Cudd.Bdd.t -> bool * int
      (** Output the BDD and return its identifier *)

val make_mtbdd :
  table:'a Cudd.Mtbdd.unique Cudd.Mtbdd.table -> 
  cond:SetteI.t ref -> 
  'a mtbdd
      (** Create a database for printing MTBDDs
	  
	  [cond] allows to share the same set of conditions between
	  BDDs and MTBDDs. *)

val id_of_mtbdd : 'a mtbdd -> 'a Cudd.Mtbdd.t -> int
      (** Output the MTBDD and return its identifier *)

val iter_cond_ordered : SetteI.t -> 'a Cudd.Man.t -> (int -> unit) -> unit
      (** Iterate the function on all the registered conditions, from level 0
      to higher levels. *)
val iter_bdef_ordered : 'a bdd -> (int -> bnode -> unit) -> unit
      (** Iterate on definitions of BDD identifiers, in a topological order. *)
val iter_mdef_ordered : 'a mtbdd -> (int -> 'a mnode -> unit) -> unit
      (** Iterate on definitions of MTBDD identifiers, in a topological order. *)
