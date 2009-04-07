(** Purely arithmetic expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** Types of numerical variables (distinction is exploited when
    negating constraints) *)
type typ = [
  | `Int 
  | `Real
]

class type ['a] db = object
  constraint 'a = [>typ]
  method typ_of_var : string -> 'a
end

(*  ********************************************************************** *)
(** {2 Expressions} *)
(*  ********************************************************************** *)

(*  ==================================================================== *)
(** {3 Linear expressions} *)
(*  ==================================================================== *)

module Lin :
  sig
    type term = Mpqf.t * string
    type t = {
      cst : Mpqf.t;
      lterm : term list;
    }
    val normalize : t -> t
    val compare_lterm : term list -> term list -> int
    val compare : t -> t -> int
    val var : string -> t
    val zero : t
    val one : t
    val cst : Mpqf.t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val scale : Mpqf.t -> t -> t
    val negate : t -> t
    val support : t -> string PSette.t
    val substitute_by_var : t -> string MappeS.t -> t
    val normalize_as_constraint : t -> t
    val print : Format.formatter -> t -> unit

    val to_linexpr1 : Apron.Environment.t -> t -> Apron.Linexpr1.t
  end

(*  ==================================================================== *)
(** {3 Polynomial expressions} *)
(*  ==================================================================== *)

module Poly :
  sig
    type varexp = string * int
    type monomial = varexp list
    type term = Mpqf.t * monomial
    type t = term list
    val compare_varexp : varexp -> varexp -> int
    val compare_monomial : monomial -> monomial -> int
    val normalize_monomial : monomial -> monomial
    val normalize : t -> t
    val normalize_full : t -> t
    val compare : t -> t -> int
    val cst : Mpqf.t -> t
    val var : string -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val scale : Mpqf.t * monomial -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val negate : t -> t
    val support : t -> string PSette.t
    val substitute_by_var : t -> string MappeS.t -> t
    val normalize_as_constraint : t -> t
    val print : Format.formatter -> t -> unit
  end

(*  ==================================================================== *)
(** {3 Tree expressions} *)
(*  ==================================================================== *)

module Tree :
  sig

    type unop = Apron.Texpr1.unop = Neg | Cast | Sqrt
    type binop = Apron.Texpr1.binop = Add | Sub | Mul | Div | Mod
    type typ =
      Apron.Texpr1.typ =
      | Real
      | Int
      | Single
      | Double
      | Extended
      | Quad
    type round = Apron.Texpr1.round = Near | Zero | Up | Down | Rnd
    type t =
      Apron.Texpr1.expr =
      | Cst of Apron.Coeff.t
      | Var of Apron.Var.t
      | Unop of unop * t * typ * round
      | Binop of binop * t * t * typ * round
    val support : t -> string PSette.t
    val substitute_by_var : t -> string MappeS.t -> t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end

(*  ==================================================================== *)
(** {3 Conversions} *)
(*  ==================================================================== *)

val lin_of_poly : Poly.t -> Lin.t
val lin_of_tree : Tree.t -> Lin.t
val poly_of_tree : Tree.t -> Poly.t
val tree_of_lin : Lin.t -> Tree.t
val tree_of_poly : Poly.t -> Tree.t

(*  ********************************************************************** *)
(** {2 General expressions and operations} *)
(*  ********************************************************************** *)

type t =
  | Lin of Lin.t
  | Poly of Poly.t
  | Tree of Tree.t
type expr = t

val var : 'a #db -> string -> t
val zero : t
val one : t
val cst : Apron.Coeff.t -> t
val add : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t -> t
val sub : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t -> t
val mul : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t -> t
val div : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t -> t
val gmod : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t -> t
val negate : t -> t
val cast : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t
val sqrt : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t
val support : t -> string PSette.t
val substitute_by_var : t -> string MappeS.t -> t
val normalize : t -> t
val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int
val normalize_as_constraint : t -> t
val is_dependent_on_integer_only : 'a #db -> t -> bool
val typ_of_expr : 'a #db -> t -> [`Int | `Real]
val print : Format.formatter -> t -> unit
val print_typ : Format.formatter -> [>typ] -> unit

val to_texpr1 : Apron.Environment.t -> t -> Apron.Texpr1.t
val to_apron : 
  Apron.Environment.t -> t -> 
      [ 
	| `Linexpr1 of Apron.Linexpr1.t
	| `Texpr1 of Apron.Texpr1.t
      ]

(*  ********************************************************************** *)
(** {2 Constraints} *)
(*  ********************************************************************** *)

module Condition :
  sig
    type typ = Apron.Tcons1.typ = 
      EQ | SUPEQ | SUP | DISEQ | EQMOD of Apron.Scalar.t
    type t = typ * expr
    val make : 'a #db -> typ -> expr -> [ `Cond of t | `Bool of bool ]
    val negate : 'a #db -> t -> t
    val support : t -> string PSette.t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
    val to_tcons1 : Apron.Environment.t -> t -> Apron.Tcons1.t
    val to_apron : 
      Apron.Environment.t -> t -> 
	[ 
	  | `Lincons1 of Apron.Lincons1.t
	  | `Tcons1 of Apron.Tcons1.t
	]
  end
