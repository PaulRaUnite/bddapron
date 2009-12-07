(** Abstract syntax tree for BDDAPRON expressions *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]

(** Unary operators *)
type unop = [
| `Not
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Boolean/finite-type binary operators *)
type bbinop = Or | And | EQ | NEQ | GT | GEQ | LEQ | LT

(** Binary operators *)
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Expressions *)
type expr =
  | Cst of cst
  | Ref of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | If of expr * expr * expr
  | In of expr * expr list

exception Error of string

(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)

val error : ('a, Format.formatter, unit, 'b) format4 -> 'a

val is_zero : expr -> bool

val precedence_of_unop : unop -> int
val precedence_of_binop : binop -> int
val precedence_of_expr : expr -> int

val print_typdef : Format.formatter -> string Env.typdef -> unit
val print_typ : Format.formatter -> [<string Env.typ] -> unit
val print_cst : Format.formatter -> cst -> unit
val print_unop : Format.formatter -> unop -> unit
val print_bbinop : Format.formatter -> bbinop -> unit
val print_binop : Format.formatter -> binop -> unit
val print_expr : Format.formatter -> expr -> unit
