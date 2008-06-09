(** Finite-type and arithmetical expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type typ = [
  | Bddexpr.typ
  | Apronexpr.typ
]
type typdef = Bddexpr.typdef
type cond = ApronexprDD.cond
type expr = [
  | Bddexpr.expr
  | `Apron of ApronexprDD.expr
]

val print_typ : Format.formatter -> [< typ] -> unit
val print_typdef : Format.formatter -> [< typdef] -> unit

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig

class type ['a,'b,'c] env = object
  inherit [[> typ] as 'a,[> typdef] as 'b,[> ] as 'c] Bddenv.t
  val v_apronexprdd : Apronexpr.expr Mtbdd2.manager
  method apronexprdd : Apronexpr.expr Mtbdd2.manager
  val mutable v_apron_env : Apron.Environment.t
  method apron_env : Apron.Environment.t
  method set_apron_env : Apron.Environment.t -> unit
end

class ['a,'b] make_env : ?boolfirst:bool -> Manager.t -> ['a, 'b, cond] env

val make_env : ?boolfirst:bool -> Manager.t -> (typ,typdef,cond) env
  (** Create a new database *)

val add_typ : (('a,'b,'c) #env as 'd) -> string -> 'b -> 'd
val add_vars : (('a,'b,'c) #env as 'd) -> (string * 'a) list -> 'd
val remove_vars : (('a,'b,'c) #env as 'd) -> string list -> 'd

module Bool : sig
  type t = Bdd.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val dtrue : ('a,'b,'c) #env -> t
  val dfalse : ('a,'b,'c) #env -> t
  val of_bool : ('a,'b,'c) #env -> bool -> t
  val var : ('a,'b,'c) #env -> string -> t
  val ite : ('a,'b,'c) #env -> t -> t -> t -> t

  val dnot : ('a,'b,'c) #env -> t -> t
  val dand : ('a,'b,'c) #env -> t -> t -> t
  val dor : ('a,'b,'c) #env -> t -> t -> t
  val xor : ('a,'b,'c) #env -> t -> t -> t
  val nand : ('a,'b,'c) #env -> t -> t -> t
  val nor : ('a,'b,'c) #env -> t -> t -> t
  val nxor : ('a,'b,'c) #env -> t -> t -> t
  val leq : ('a,'b,'c) #env -> t -> t -> t
  val eq : ('a,'b,'c) #env -> t -> t -> t

  val is_true : ('a,'b,'c) #env -> t -> bool
  val is_false : ('a,'b,'c) #env -> t -> bool
  val is_cst : ('a,'b,'c) #env -> t -> bool
  val is_leq : ('a,'b,'c) #env -> t -> t -> bool
  val is_eq : ('a,'b,'c) #env -> t -> t -> bool
  val is_and_false : ('a,'b,'c) #env -> t -> t -> bool
  val exist : ('a,'b,'c) #env -> string list -> t -> t
  val forall : ('a,'b,'c) #env -> string list -> t -> t

  val cofactor : ('a,'b,'c) #env -> t -> t -> t
  val restrict : ('a,'b,'c) #env -> t -> t -> t
  val tdrestrict : ('a,'b,'c) #env -> t -> t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,cond) #env -> t -> (string*string) list -> t
  val substitute : ('a,'b,cond) #env -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #env -> Format.formatter -> t -> unit
end

module Bint : sig
  type t = Bddint.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val of_int : ('a,'b,'c) #env -> [> `Tbint of bool * int ] -> int -> t
  val var : ('a,'b,'c) #env -> string -> t
  val ite : ('a,'b,'c) #env -> Bool.t -> t -> t -> t

  val neg : ('a,'b,'c) #env -> t -> t
  val succ : ('a,'b,'c) #env -> t -> t
  val pred : ('a,'b,'c) #env -> t -> t
  val add : ('a,'b,'c) #env -> t -> t -> t
  val sub : ('a,'b,'c) #env -> t -> t -> t
  val shift_left : ('a,'b,'c) #env -> int -> t -> t
  val shift_right : ('a,'b,'c) #env -> int -> t -> t
  val scale : ('a,'b,'c) #env -> int -> t -> t
  val zero : ('a,'b,'c) #env -> t -> Bool.t
  val eq : ('a,'b,'c) #env -> t -> t -> Bool.t
  val eq_int : ('a,'b,'c) #env -> t -> int -> Bool.t
  val supeq : ('a,'b,'c) #env -> t -> t -> Bool.t
  val supeq_int : ('a,'b,'c) #env -> t -> int -> Bool.t
  val sup : ('a,'b,'c) #env -> t -> t -> Bool.t

  val cofactor : ('a,'b,'c) #env -> t -> Bool.t -> t
  val restrict : ('a,'b,'c) #env -> t -> Bool.t -> t
  val tdrestrict : ('a,'b,'c) #env -> t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,cond) #env -> t -> (string*string) list -> t
  val substitute : ('a,'b,cond) #env -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #env -> Format.formatter -> t -> unit
end

module Benum : sig
  type t = Bddenum.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val var : ('a,'b,'c) #env -> string -> t
  val ite : ('a,'b,'c) #env -> Bool.t -> t -> t -> t
  val eq : ('a,'b,'c) #env -> t -> t -> Bool.t
  val eq_label : ('a,'b,'c) #env -> t -> string -> Bool.t
  val cofactor : ('a,'b,'c) #env -> t -> Bool.t -> t
  val restrict : ('a,'b,'c) #env -> t -> Bool.t -> t
  val tdrestrict : ('a,'b,'c) #env -> t -> Bool.t -> t
  val permute : t -> int array -> t
  val substitute_by_var : ('a,'b,cond) #env -> t -> (string*string) list -> t
  val substitute : ('a,'b,cond) #env -> t -> (string*expr) list -> t
  val print : ('a,'b,'c) #env -> Format.formatter -> t -> unit
end

module Apron : sig
  type t = ApronexprDD.expr
  val of_expr : [> `Apron of t ] -> t
  val to_expr : t -> [> `Apron of t ]
  val cst : ('a,'b,'c) #env -> Apron.Coeff.t -> t
  val var : ('a,'b,'c) #env -> string -> t
  val add :('a,'b,'c) #env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : ('a,'b,'c) #env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : ('a,'b,'c) #env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : ('a,'b,'c) #env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : ('a,'b,'c) #env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val negate : ('a,'b,'c) #env -> t -> t
  val cast :
    ('a,'b,'c) #env -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
  val sqrt :
    ('a,'b,'c) #env -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t

  val supeq : ('a,'b,[> cond]) #env -> t -> Bool.t
  val sup : ('a,'b,[> cond]) #env -> t -> Bool.t
  val eq : ('a,'b,[> cond]) #env -> t -> Bool.t

  val ite : ('a,'b,'c) #env -> Bool.t -> t -> t -> t

  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,cond) #env -> t -> (string*string) list -> t
  val substitute : ('a,'b,cond) #env -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #env -> Format.formatter -> t -> unit
end

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr: [<expr] -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

val var : ('a,'b,'c) #env -> string -> expr
  (** Expression representing the litteral var *)

val ite : ('a,'b,'c) #env -> Bool.t -> expr -> expr -> expr
  (** If-then-else operation *)

val substitute_by_var : ('a,'b,cond) #env -> expr -> (string*string) list -> expr
    (** Parallel substitution of variables by variables *)
val substitute : ('a,'b,cond) #env -> expr -> (string*expr) list -> expr
    (** Parallel substitution of variables by expressions *)

val support : ('a,'b,cond) #env -> expr -> SetteS.t
    (** Return the full support of the expression *)

val eq : ('a,'b,cond) #env -> expr -> expr -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
      arithmetic expressions, do not take into account the careset. *)

val support_cond : ('a,'b,'c) #env -> expr -> Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)
val vectorsupport_cond : ('a,'b,'c) #env -> expr array -> Bdd.t
    (** Return the support of an array of expressions as a conjunction of the
      BDD identifiers involved in the expressions *)

(** Printing functions *)

val print_cond : ('a,'b,'c) #env -> Format.formatter -> [<cond] -> unit
val print_expr : ('a,'b,'c) #env -> Format.formatter -> [<expr] -> unit
val print_bdd : ('a,'b,'c) #env -> Format.formatter -> Bdd.t -> unit
val print_env : Format.formatter -> (typ,typdef,cond) env -> unit

val compose_of_substitution :
  ('a,'b,cond) #env -> (string*expr) list -> Bdd.t array option * expr MappeS.t

val tbddidd_of_texpr : expr array -> Bdd.t array * Idd.t array
  (** Concatenates in pair of BDD and IDD arrays the BDDs/IDDs involved in the
    expressions *)

val texpr_of_tbddidd : expr array -> Bdd.t array -> Idd.t array -> expr array
  (** Inverse operation: rebuild an array of expressions from the old array of
    expressions (for the types) and the arrays of BDDs/IDDs.  *)

(*
  val cleanup : ('a,'b,'c) #env -> SetteI.t -> expr list -> unit
*)
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type env = [typ,typdef,cond] O.env
class make_env : ?boolfirst:bool -> Manager.t -> env
val make_env : ?boolfirst:bool -> Manager.t -> env
  (** Create a new database *)

val add_typ : env -> string -> typdef -> env
val add_vars : env -> (string * typ) list -> env
val remove_vars : env -> string list -> env

module Bool : sig
  type t = Bdd.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val dtrue : env -> t
  val dfalse : env -> t
  val of_bool : env -> bool -> t
  val var : env -> string -> t
  val ite : env -> t -> t -> t -> t

  val dnot : env -> t -> t
  val dand : env -> t -> t -> t
  val dor : env -> t -> t -> t
  val xor : env -> t -> t -> t
  val nand : env -> t -> t -> t
  val nor : env -> t -> t -> t
  val nxor : env -> t -> t -> t
  val leq : env -> t -> t -> t
  val eq : env -> t -> t -> t

  val is_true : env -> t -> bool
  val is_false : env -> t -> bool
  val is_cst : env -> t -> bool
  val is_leq : env -> t -> t -> bool
  val is_eq : env -> t -> t -> bool
  val is_and_false : env -> t -> t -> bool
  val exist : env -> string list -> t -> t
  val forall : env -> string list -> t -> t

  val cofactor : env -> t -> t -> t
  val restrict : env -> t -> t -> t
  val tdrestrict : env -> t -> t -> t
  val permute : t -> int array -> t

  val substitute_by_var : env -> t -> (string*string) list -> t
  val substitute : env -> t -> (string*expr) list -> t

  val print : env -> Format.formatter -> t -> unit
end

module Bint : sig
  type t = Bddint.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val of_int : env -> [`Tbint of bool * int ] -> int -> t
  val var : env -> string -> t
  val ite : env -> Bool.t -> t -> t -> t

  val neg : env -> t -> t
  val succ : env -> t -> t
  val pred : env -> t -> t
  val add : env -> t -> t -> t
  val sub : env -> t -> t -> t
  val shift_left : env -> int -> t -> t
  val shift_right : env -> int -> t -> t
  val scale : env -> int -> t -> t
  val zero : env -> t -> Bool.t
  val eq : env -> t -> t -> Bool.t
  val eq_int : env -> t -> int -> Bool.t
  val supeq : env -> t -> t -> Bool.t
  val supeq_int : env -> t -> int -> Bool.t
  val sup : env -> t -> t -> Bool.t

  val cofactor : env -> t -> Bool.t -> t
  val restrict : env -> t -> Bool.t -> t
  val tdrestrict : env -> t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : env -> t -> (string*string) list -> t
  val substitute : env -> t -> (string*expr) list -> t

  val print : env -> Format.formatter -> t -> unit
end

module Benum : sig
  type t = Bddenum.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val var : env -> string -> t
  val ite : env -> Bool.t -> t -> t -> t
  val eq : env -> t -> t -> Bool.t
  val eq_label : env -> t -> string -> Bool.t
  val cofactor : env -> t -> Bool.t -> t
  val restrict : env -> t -> Bool.t -> t
  val tdrestrict : env -> t -> Bool.t -> t
  val permute : t -> int array -> t
  val substitute_by_var : env -> t -> (string*string) list -> t
  val substitute : env -> t -> (string*expr) list -> t
  val print : env -> Format.formatter -> t -> unit
end

module Apron : sig
  type t = ApronexprDD.expr
  val of_expr : expr -> t
  val to_expr : t -> expr
  val cst : env -> Apron.Coeff.t -> t
  val var : env -> string -> t
  val add :env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : env ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val negate : env -> t -> t
  val cast :
    env -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
  val sqrt :
    env -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t

  val supeq : env -> t -> Bool.t
  val sup : env -> t -> Bool.t
  val eq : env -> t -> Bool.t

  val ite : env -> Bool.t -> t -> t -> t

  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : env -> t -> (string*string) list -> t
  val substitute : env -> t -> (string*expr) list -> t

  val print : env -> Format.formatter -> t -> unit
end

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr: expr -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

val var : env -> string -> expr
  (** Expression representing the litteral var *)

val ite : env -> Bool.t -> expr -> expr -> expr
  (** If-then-else operation *)

val cofactor : expr -> Bool.t -> expr
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val substitute_by_var : env -> expr -> (string*string) list -> expr
    (** Parallel substitution of variables by variables *)
val substitute : env -> expr -> (string*expr) list -> expr
    (** Parallel substitution of variables by expressions *)

val restrict : expr -> Bool.t -> expr
val tdrestrict : expr -> Bool.t -> expr
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val permute : expr -> int array -> expr
  (** Permutation (rather internal) *)

val support : env -> expr -> SetteS.t
    (** Return the full support of the expression *)

val eq : env -> expr -> expr -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
      arithmetic expressions, do not take into account the careset. *)

val support_cond : env -> expr -> Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)
val vectorsupport_cond : env -> expr array -> Bdd.t
    (** Return the support of an array of expressions as a conjunction of the
      BDD identifiers involved in the expressions *)

(** Printing functions *)

val print_typ : Format.formatter -> [<typ] -> unit
val print_typdef : Format.formatter -> [<typdef] -> unit
val print_cond : env -> Format.formatter -> [<cond] -> unit
val print_expr : env -> Format.formatter -> [<expr] -> unit
val print_env : Format.formatter -> env -> unit
(*
val cleanup : env -> SetteI.t -> expr list -> unit
*)
