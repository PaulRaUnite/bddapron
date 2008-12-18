(** Arithmetical Decision Diagrams *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type 'a manager = 'a Mtbdd2.manager
type 'a t = 'a Mtbdd2.t

type expr = Apronexpr.expr t
type cond = [`Apron of Apronexpr.Condition.t]

val of_expr : [>`Apron of expr] -> expr
val to_expr : expr -> [>`Apron of expr]

(*  ********************************************************************** *)
(** {2 Specific functions} *)
(*  ********************************************************************** *)

val make_manager : unit -> Apronexpr.expr manager

val print :
  (Format.formatter -> Bdd.t -> unit) -> 
  Format.formatter -> expr -> unit
val print_manager : 
  Format.formatter -> Apronexpr.expr manager -> unit

val cst : 
  Manager.t -> Apronexpr.expr manager -> 
  Apron.Coeff.t -> Apronexpr.expr t
val var :
  Manager.t -> Apronexpr.expr manager ->
  [> Apronexpr.typ ] #Apronexpr.db -> string -> Apronexpr.expr t
val add :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
  Apronexpr.expr t -> Apronexpr.expr t -> Apronexpr.expr t
val sub :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
  Apronexpr.expr t -> Apronexpr.expr t -> Apronexpr.expr t
val mul :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
  Apronexpr.expr t -> Apronexpr.expr t -> Apronexpr.expr t
val div :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
  Apronexpr.expr t -> Apronexpr.expr t -> Apronexpr.expr t
val gmod :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
  Apronexpr.expr t -> Apronexpr.expr t -> Apronexpr.expr t
val negate : Apronexpr.expr t -> Apronexpr.expr t
val cast :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 
  Apronexpr.expr t -> Apronexpr.expr t
val sqrt :
  ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 
  Apronexpr.expr t -> Apronexpr.expr t

val support_leaf : Apronexpr.expr t -> SetteS.t
val support_cond : 'a t -> Bdd.t

val substitute :
  Manager.t -> Apronexpr.expr manager ->
  Apronexpr.expr -> [> `Apron of expr ] MappeS.t -> expr

module Condition :
  sig
    val make :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.Condition.typ -> Apronexpr.expr t -> Bdd.t
    val supeq :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.expr t -> Bdd.t
    val sup :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.expr t -> Bdd.t
    val eq :
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.expr t -> Bdd.t

   val substitute :
      Manager.t -> Apronexpr.expr manager ->
      < idb_of_cond : [> `Apron of Apronexpr.Condition.t ] -> int * bool;
        typ_of_var : string -> [> Apronexpr.typ ]; .. > ->
      Apronexpr.Condition.t -> [> `Apron of expr ] MappeS.t -> Bdd.t
  end

(*  ********************************************************************** *)
(** {2 Standard Mtbdd2 functions} *)
(*  ********************************************************************** *)

val leaf_of_id : 'a manager -> int -> 'a
val id_of_leaf : 'a manager -> 'a -> int
val of_idd : 'a manager -> Idd.t -> 'a t
val to_idd : 'a t -> Idd.t
val manager : 'a t -> Manager.t
val is_cst : 'a t -> bool
val topvar : 'a t -> int
val dthen : 'a t -> 'a t
val delse : 'a t -> 'a t
val cofactor : 'a t -> Bdd.t -> 'a t
val dval : 'a t -> 'a
val is_background : 'a t -> bool
val support : 'a t -> Bdd.t
val supportsize : 'a t -> int
val is_var_in : int -> 'a t -> bool
val vectorsupport : 'a t array -> Bdd.t
val vectorsupport2 : Bdd.t array -> 'a t array -> Bdd.t
val background : Manager.t -> 'a manager -> 'a t
val ite : Bdd.t -> 'a t -> 'a t -> 'a t
val compose : int -> Bdd.t -> 'a t -> 'a t
val vectorcompose : Bdd.t array -> 'a t -> 'a t
val is_equal : 'a t -> 'a t -> bool
val is_equal_when : 'a t -> 'a t -> Bdd.t -> bool
val is_eval_cst : 'a option t -> Bdd.t -> 'a option
val is_ite_cst : Bdd.t -> 'a option t -> 'a option t -> 'a option
val size : 'a t -> int
val nbpaths : 'a t -> float
val nbnonzeropaths : 'a t -> float
val nbminterms : int -> 'a t -> float
val density : int -> 'a t -> float
val nbleaves : 'a t -> int
val varmap : 'a t -> 'a t
val permute : 'a t -> int array -> 'a t
val iter_cube : (Manager.tbool array -> 'a -> unit) -> 'a t -> unit
val guard_of_nonbackground : 'a t -> Bdd.t
val leaves : 'a t -> 'a array
val guardleafs : 'a t -> (Bdd.t * 'a) array
val guard_of_leaf : 'a t -> 'a -> Bdd.t
val constrain : 'a t -> Bdd.t -> 'a t
val tdconstrain : 'a t -> Bdd.t -> 'a t
val restrict : 'a t -> Bdd.t -> 'a t
val tdrestrict : 'a t -> Bdd.t -> 'a t
val mapleaf1 : 'a manager -> (Bdd.t -> 'b -> 'a) -> 'b t -> 'a t
val mapleaf2 :
  'a manager -> (Bdd.t -> 'b -> 'c -> 'a) -> 'b t -> 'c t -> 'a t
val mapunop : 'a manager -> ('b -> 'a) -> 'b t -> 'a t
val mapbinop :
  ?commutative:bool ->
  ?idempotent:bool ->
  ?absorbant:('a * 'b * 'c * 'c) ->
  ?neutral:('a * 'b) ->
  'c manager -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val mapterop :
  'a manager -> ('b -> 'c -> 'd -> 'a) -> 'b t -> 'c t -> 'd t -> 'a t
val mapcmpop :
  ?bottom:'a -> 
  ?top:'b ->
  ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val mapexistop : 
  absorbant:'a -> 
  ('a -> 'a -> 'a) -> Bdd.t -> 'a t -> 'a t
val mapexistandop :
  absorbant:'a -> 
  ('a -> 'a -> 'a) -> Bdd.t -> Bdd.t -> 'a t -> 'a t
val mapexistandapplyop :
  absorbant:'a -> 
  ('a -> 'a) -> ('a -> 'a -> 'a) -> Bdd.t -> Bdd.t -> 'a t -> 'a t

val wrapunop : (Idd.t -> Idd.t) -> 'a t -> 'a t
val wrapbinop : (Idd.t -> Idd.t -> Idd.t) -> 'a t -> 'a t -> 'a t
