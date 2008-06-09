(** Purely arithmetic formula *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

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
    type expr = {
      cst : Mpqf.t;
      lterm : term list;
    }
    val normalize : expr -> expr
    val compare_lterm : term list -> term list -> int
    val compare : expr -> expr -> int
    val var : string -> expr
    val cst : Mpqf.t -> expr
    val add : expr -> expr -> expr
    val sub : expr -> expr -> expr
    val scale : Mpqf.t -> expr -> expr
    val negate : expr -> expr
    val support : expr -> SetteS.t
    val substitute_by_var : expr -> string MappeS.t -> expr
    val normalize_as_constraint : expr -> expr
    val print : Format.formatter -> expr -> unit

    val to_linexpr1 : Apron.Environment.t -> expr -> Apron.Linexpr1.t
  end

(*  ==================================================================== *)
(** {3 Polynomial expressions} *)
(*  ==================================================================== *)

module Poly :
  sig
    type varexp = string * int
    type monomial = varexp list
    type term = Mpqf.t * monomial
    type expr = term list
    val compare_varexp : varexp -> varexp -> int
    val compare_monomial : monomial -> monomial -> int
    val normalize_monomial : monomial -> monomial
    val normalize : expr -> expr
    val normalize_full : expr -> expr
    val compare : expr -> expr -> int
    val cst : Mpqf.t -> expr
    val var : string -> expr
    val add : expr -> expr -> expr
    val sub : expr -> expr -> expr
    val scale : Mpqf.t * monomial -> expr -> expr
    val mul : expr -> expr -> expr
    val div : expr -> expr -> expr
    val negate : expr -> expr
    val support : expr -> SetteS.t
    val substitute_by_var : expr -> string MappeS.t -> expr
    val normalize_as_constraint : expr -> expr
    val print : Format.formatter -> expr -> unit
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
    type expr =
      Apron.Texpr1.expr =
      | Cst of Apron.Coeff.t
      | Var of Apron.Var.t
      | Unop of unop * expr * typ * round
      | Binop of binop * expr * expr * typ * round
    val support : expr -> SetteS.t
    val substitute_by_var : expr -> string MappeS.t -> expr
    val print : Format.formatter -> expr -> unit
    val compare : expr -> expr -> int
  end

(*  ==================================================================== *)
(** {3 Conversions} *)
(*  ==================================================================== *)

val lin_of_poly : Poly.expr -> Lin.expr
val lin_of_tree : Tree.expr -> Lin.expr
val poly_of_tree : Tree.expr -> Poly.expr
val tree_of_lin : Lin.expr -> Tree.expr
val tree_of_poly : Poly.expr -> Tree.expr

(*  ==================================================================== *)
(** {3 General expressions and operations} *)
(*  ==================================================================== *)

type expr =
  | Lin of Lin.expr
  | Poly of Poly.expr
  | Tree of Tree.expr

val var : 'a #db -> string -> expr
val cst : Apron.Coeff.t -> expr
val add : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr -> expr
val sub : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr -> expr
val mul : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr -> expr
val div : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr -> expr
val gmod : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr -> expr
val negate : expr -> expr
val cast : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr
val sqrt : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> expr -> expr
val support : expr -> SetteS.t
val substitute_by_var : expr -> string MappeS.t -> expr
val normalize : expr -> expr
val equal : expr -> expr -> bool
val hash : expr -> int
val compare : expr -> expr -> int
val normalize_as_constraint : expr -> expr
val is_dependent_on_integer_only : 'a #db -> expr -> bool
val typ_of_expr : 'a #db -> expr -> [`Int | `Real]
val print : Format.formatter -> expr -> unit
val print_typ : Format.formatter -> [>typ] -> unit

val to_texpr1 : Apron.Environment.t -> expr -> Apron.Texpr1.t
val to_apron : 
  Apron.Environment.t -> expr -> 
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
    val support : t -> SetteS.t
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
