(** Separation of Boolean formula in purely Boolean/conditional parts *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type vdd = bool Cudd.Vdd.t
val vdd_of_bdd : Cudd.Man.v Cudd.Bdd.t -> bool Cudd.Vdd.t
val bdd_of_vdd : bool Cudd.Vdd.t -> Cudd.Man.v Cudd.Bdd.t

type typ = Bool | Cond | Other
type info = {
  mutable minlevelbool : int;
  mutable maxlevelbool : int;
  mutable minlevelcond : int;
  mutable maxlevelcond : int;
  varlevel : int array;
  levelvar : int array;
  vartyp : typ array;
  leveltyp : typ array;
}

val make_info :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t -> info
  (** Builds a temporary record of type [info] which gathers
      various informations on environment and condition. *)

val split_level : Cudd.Bdd.vt -> int -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list
  (** Decompose a BDD [f] into a disjunction [[(f1,g1);...;(fN,gN)]] such that
      - all decisions in f1...fN have levels strictly less than [level];
      - all decisions in g1...gN have levels greater than or equal to [level];
      - f1...fN are pairwise disjoint.
  *)

val decompose_boolcond :
  ('a, 'b, 'c, 'd, 'e) Env.t0 -> ('f, 'g, 'h, 'i) Cond.t ->
  Cudd.Bdd.vt -> (Cudd.Bdd.vt * Cudd.Bdd.vt) list
  (** Decompose a BDD [f] into a disjunction [[(f1,g1);...;(fN,gN)]] such that
      - all decisions in f1...fN are Boolean variables;
      - all decisions in g1...gN are (non-Boolean) conditions;
      - f1...fN are pairwise disjoint.
  *)

val conjunction_of_minterm :
  ?first:int -> ?last:int ->
  (int * bool -> 'a) -> Cudd.Man.tbool array -> 'a Normalform.conjunction
  (** [conjunction_of_minterm of_idb minterm] translates a
      minterm into an explicit conjunction of type ['a
      Normalform.conjunction], using the function [of_idb] to
      produce elements of type ['a].

      The optional arguments [first] and [last] allows focusing only
      on a part of the minterm.
  *)

val dnf_of_bdd :
  ?first:int -> ?last:int ->
  (int * bool -> 'a) ->
  'b Cudd.Bdd.t -> 'a Normalform.dnf
  (** Converts a BDD into a disjunctive normal form.  The
      arguments are the same as in {!conjunction_of_minterm}. *)
