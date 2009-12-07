(** Enumerated expressions with BDDs *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type 'a typ = [
  `Benum of 'a
]
  (** A type is just a name *)

type 'a typdef = [
  `Benum of 'a array
]
  (** An enumerated type is defined by its (ordered) set of labels *)

(** {3 Datatype representing a BDD register of enumerated type} *)

type 'a t = {
  typ: string;
    (** Type of the value (refers to the database, see below) *)
  reg: 'a Reg.t
    (** Value itself *)
}

type dt = Cudd.Man.d t
type vt = Cudd.Man.v t

(*  *********************************************************************** *)
(** {2 Constants and Operation(s)} *)
(*  *********************************************************************** *)

val of_label : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'd t
  (** Create a register of the type of the label containing the label *)
val is_cst : 'd t -> bool
  (** Does the register contain a constant value ? *)
val to_code : 'd t -> int
  (** Convert a constant register to its value as a code. *)
val to_label : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a
  (** Convert a constant register to its value as a label. *)
val equal_label : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a -> 'd Cudd.Bdd.t
  (** Under which condition the register is equal to the label ? *)
val equal : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'd t -> 'd Cudd.Bdd.t
  (** Under which condition the 2 registers are equal ? *)
val ite : 'd Cudd.Bdd.t -> 'd t -> 'd t -> 'd t
  (** If-then-else operator. The types of the 2 branches should be the same. *)

(*  *********************************************************************** *)
(** {2 Decomposition in guarded form} *)
(*  *********************************************************************** *)

val guard_of_label : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> 'a -> 'd Cudd.Bdd.t
  (** Return the guard of the label in the BDD register. *)

val guardlabels : ('a,'b,'c,'d,'e) Env.O.t -> 'd t -> ('d Cudd.Bdd.t * 'a) list
  (** Return the list [g -> label] represented by the BDD register. *)

(*  *********************************************************************** *)
(** {2 Evaluation} *)
(*  *********************************************************************** *)

val cofactor : 'd t -> 'd Cudd.Bdd.t -> 'd t
val restrict : 'd t -> 'd Cudd.Bdd.t -> 'd t
val tdrestrict : 'd t -> 'd Cudd.Bdd.t -> 'd t

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

val print : 
  (Format.formatter -> int -> unit) ->
  Format.formatter -> 'd t -> unit
  (** [print f fmt t] prints the register [t] using the formatter
    [fmt] and the function [f] to print BDDs indices. *)
val print_minterm :
  (Format.formatter -> 'd Cudd.Bdd.t -> unit) ->
  ('a,'b,'c,'d,'e) Env.O.t -> Format.formatter -> 'd t -> unit
  (** [print_minterm f fmt t] prints the register [t] using the formatter
    [fmt] and the function [f] to convert BDDs indices to
    names. *)

(*  ********************************************************************** *)
(** {2 Internal functions} *)
(*  ********************************************************************** *)

val size_of_typ : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int
  (** Return the cardinality of a type (the number of its labels) *)
val maxcode_of_typ : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int
  (** Return the maximal integer corresponding to a label belonging to the
    type. Labels are indeed associated numbers from 0 to this number. *)
val mem_typcode : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int -> bool
  (** Does the integer code some label of the given type ? *)
val labels_of_typ : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> 'a array
  (** Return the array of labels defining the type *)
val code_of_label : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int
  (** Return the code associated to the label *)
val label_of_typcode : ('a,'b,'c,'d,'e) Env.O.t -> 'a -> int -> 'a
  (** Return the label associated to the given code interpreted as of type the
    given type. *)

module Minterm : sig
  val iter: ('a,'b,'c,'d,'e) Env.O.t -> 'a -> ('a -> unit) -> Reg.Minterm.t -> unit
    (** Iter the function on all label of the given type contained in the
      minterm. *)
  val map: ('a,'b,'c,'d,'e) Env.O.t -> 'a -> ('a -> 'f) -> Reg.Minterm.t -> 'f list
    (** Apply the function to all label of the given type contained in the
      minterm and return the list of the results. *)
end

val permute : 'd t -> int array -> 'd t
  (** Permutation (scale [Cudd.Bdd.permute]) *)
val varmap : 'a t -> 'a t
  (** Permutation (scale [Cudd.Bdd.varmap]) *)
val vectorcompose : 'd Cudd.Bdd.t array -> 'd t -> 'd t
  (** Composition (scale [Cudd.Bdd.vectorcompose]) *)


