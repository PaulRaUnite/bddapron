(** Decision Diagrams on top of arithmetic expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val table : Apronexpr.t Cudd.Mtbdd.table

type t = Apronexpr.t Cudd.Mtbdd.t
val of_expr : [> `Apron of 'a ] -> 'a
val to_expr : t -> [> `Apron of t ]
val print :
  (Format.formatter -> Cudd.Bdd.vt -> unit) ->
  Format.formatter -> t -> unit
val is_zero : Apronexpr.t -> bool
val is_one : Apronexpr.t -> bool
val absorbant_zero :
  Apronexpr.t Cudd.Mtbdd.unique -> Apronexpr.t Cudd.Mtbdd.unique option
val absorbant_one :
  Apronexpr.t Cudd.Mtbdd.unique -> Apronexpr.t Cudd.Mtbdd.unique option
val cst : Cudd.Man.vt -> Apron.Coeff.t -> t
val var : ('a,'b,'c) Env.O.t -> string -> t
val add :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t -> t
val sub :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t -> t
val mul :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t -> t
val div :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t -> t
val gmod :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t -> t
val negate : t -> t
val cast :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t
val sqrt :
  ?typ:Apron.Texpr1.typ ->
  ?round:Apron.Texpr1.round ->
  t -> t
val support_leaf : t -> string PSette.t
val support_cond : t -> Cudd.Bdd.vt
val substitute_linexpr :
  Cudd.Man.vt -> Apronexpr.Lin.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute_polyexpr :
  Cudd.Man.vt -> Apronexpr.Poly.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute_treeexpr :
  Cudd.Man.vt -> Apronexpr.Tree.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute :
  Cudd.Man.vt -> Apronexpr.t -> (string, [> `Apron of t ]) PMappe.t -> t
module Condition :  sig
  val make : 'a -> 'a Cond.O.t -> Apronexpr.Condition.typ -> t -> Cudd.Bdd.vt
  val supeq : 'a -> 'a Cond.O.t -> t -> Cudd.Bdd.vt
  val sup : 'a -> 'a Cond.O.t -> t -> Cudd.Bdd.vt
  val eq : 'a -> 'a Cond.O.t -> t -> Cudd.Bdd.vt
  val substitute :
    'a -> 'a Cond.O.t ->
    Apronexpr.Condition.t ->
    (string, [> `Apron of t]) PMappe.t -> 
    Cudd.Bdd.vt
end
