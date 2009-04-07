(** Decision Diagrams on top of arithmetic expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

val table : Apronexpr.t Cudd.Mtbdd.table

type t = Apronexpr.t Cudd.Mtbdd.t
type cond = [ `Apron of Apronexpr.Condition.t ]
val of_expr : [> `Apron of 'a ] -> 'a
val to_expr : t -> [> `Apron of t ]
val print :
  (Format.formatter -> Cudd.Man.v Cudd.Bdd.t -> unit) -> Format.formatter -> t -> unit
val is_zero : Apronexpr.t -> bool
val is_one : Apronexpr.t -> bool
val absorbant_zero :
  Apronexpr.t Cudd.Mtbdd.unique -> Apronexpr.t Cudd.Mtbdd.unique option
val absorbant_one :
  Apronexpr.t Cudd.Mtbdd.unique -> Apronexpr.t Cudd.Mtbdd.unique option
val cst : Cudd.Man.v Cudd.Man.t -> Apron.Coeff.t -> t
val var :
  Cudd.Man.v Cudd.Man.t ->
  [> Apronexpr.typ ] #Apronexpr.db -> string -> t
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
val support_cond : t -> Cudd.Man.v Cudd.Bdd.t
val substitute_linexpr :
  Cudd.Man.v Cudd.Man.t -> Apronexpr.Lin.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute_polyexpr :
  Cudd.Man.v Cudd.Man.t -> Apronexpr.Poly.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute_treeexpr :
  Cudd.Man.v Cudd.Man.t -> Apronexpr.Tree.t -> (string, [> `Apron of t ]) PMappe.t -> t
val substitute :
  Cudd.Man.v Cudd.Man.t -> Apronexpr.t -> (string, [> `Apron of t ]) PMappe.t -> t
module Condition :
  sig
    val make :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.Condition.typ -> t -> Cudd.Man.v Cudd.Bdd.t
    val supeq :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      t -> Cudd.Man.v Cudd.Bdd.t
    val sup :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      t -> Cudd.Man.v Cudd.Bdd.t
    val eq :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      t -> Cudd.Man.v Cudd.Bdd.t
    val substitute :
      Cudd.Man.v Cudd.Man.t ->
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.Condition.t -> (string, [> `Apron of t ]) PMappe.t -> Cudd.Man.v Cudd.Bdd.t
  end
