(** Finite-type and arithmetical expressions linked to normalized environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** Very important remark:

  Most functions return expressions defined on the same environment than their
  input arguments, with the exception of

  - the various [substitute] and [substitute_by_var] functions
  - [Apron.condition], [Apron.sup], [Apron.supeq], [Apron.eq] functions
  - and of course [extend_environment] functions

  The mentioned functions may return a value defined on a super-environment,
  because they may require the creation of new external conditions in the
  environment.

  If you need to use such functions temporarily, it may be a
  good idea to use the module [BddApronexpr] while the environment
  is not stabilized, and then to use [make_expr] to convert the values.
*)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig

type 'a t = ('a, Expr0.t) Bdd.Env.value
  constraint 'a = ('b,'c,'d) #Env.O.t
type 'a expr = 'a t
 (** Type of general expressions *)

val make_env :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
  ('a, 'b, Env.cond) Env.O.t

module Bool : sig
  type 'a t = ('a, Cudd.Man.v Cudd.Bdd.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  val of_expr : ('a, [> `Bool of Cudd.Man.v Cudd.Bdd.t ]) Bdd.Env.value -> 'a t
  val to_expr : 'a t -> ('a, [> `Bool of Cudd.Man.v Cudd.Bdd.t ]) Bdd.Env.value

  val extend_environment : 'a t -> 'a -> 'a t

  val dtrue : 'a -> 'a t
  val dfalse : 'a -> 'a t
  val of_bool : 'a -> bool -> 'a t
  val var : 'a -> string -> 'a t

  (** {4 Logical connectors} *)

  val dnot : 'a t -> 'a t
  val dand : 'a t -> 'a t -> 'a t
  val dor : 'a t -> 'a t -> 'a t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
      keywords) *)

  val xor : 'a t -> 'a t -> 'a t
  val nand : 'a t -> 'a t -> 'a t
  val nor : 'a t -> 'a t -> 'a t
  val nxor : 'a t -> 'a t -> 'a t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : 'a t -> 'a t -> 'a t
    (** Same as [nxor] *)
  val leq : 'a t -> 'a t -> 'a t
    (** Implication *)

  val ite : 'a t -> 'a t -> 'a t -> 'a t
    (** If-then-else *)

  val is_true : 'a t -> bool
  val is_false : 'a t -> bool
  val is_cst : 'a t -> bool
  val is_eq : 'a t -> 'a t -> bool
  val is_leq : 'a t -> 'a t -> bool
  val is_inter_false : 'a t -> 'a t -> bool

  val exist : string list -> 'a t -> 'a t
  val forall : string list -> 'a t -> 'a t

  val cofactor : 'a t -> 'a t -> 'a t
  val restrict : 'a t -> 'a t -> 'a t
  val tdrestrict : 'a t -> 'a t -> 'a t

  val substitute_by_var : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * string) list -> 'a t
  val substitute : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * 'a expr) list -> 'a t

  val print : Format.formatter -> 'a t -> unit
end

module Bint : sig
  type 'a t = ('a, Cudd.Man.v Bdd.Int.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  val of_expr : ('a, [> `Bint of Cudd.Man.v Bdd.Int.t ]) Bdd.Env.value -> 'a t
  val to_expr : 'a t -> ('a, [> `Bint of Cudd.Man.v Bdd.Int.t ]) Bdd.Env.value
  val extend_environment : 'a t -> 'a -> 'a t

  val of_int :
    'a -> [`Tbint of bool * int ] -> int -> 'a t
  val var : 'a -> string -> 'a t

  val neg : 'a t -> 'a t
  val succ : 'a t -> 'a t
  val pred : 'a t -> 'a t
  val add : 'a t -> 'a t -> 'a t
  val sub : 'a t -> 'a t -> 'a t
  val mul : 'a t -> 'a t -> 'a t
  val shift_left : int -> 'a t -> 'a t
  val shift_right : int -> 'a t -> 'a t
  val scale : int -> 'a t -> 'a t
  val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  val zero : 'a t -> 'a Bool.t
  val eq : 'a t -> 'a t -> 'a Bool.t
  val supeq : 'a t -> 'a t -> 'a Bool.t
  val sup : 'a t -> 'a t -> 'a Bool.t
  val eq_int : 'a t -> int -> 'a Bool.t
  val supeq_int : 'a t -> int -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * string) list -> 'a t
  val substitute : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * 'a expr) list -> 'a t

  val guard_of_int: 'a t -> int -> 'a Bool.t
    (** Return the guard of the integer value. *)
  val guardints: 'a t -> ('a Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : Format.formatter -> 'a t -> unit
end

module Benum : sig
  type 'a t = ('a, Cudd.Man.v Bdd.Enum.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t
  val of_expr : ('a, [> `Benum of Cudd.Man.v Bdd.Enum.t ]) Bdd.Env.value -> 'a t
  val to_expr : 'a t -> ('a, [> `Benum of Cudd.Man.v Bdd.Enum.t ]) Bdd.Env.value
  val extend_environment : 'a t -> 'a -> 'a t

  val var : 'a -> string -> 'a t
  val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  val eq : 'a t -> 'a t -> 'a Bool.t
  val eq_label : 'a t -> string -> 'a Bool.t

  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * string) list -> 'a t
  val substitute : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * 'a expr) list -> 'a t

  val guard_of_label : 'a t -> string -> 'a Bool.t
    (** Return the guard of the label. *)

  val guardlabels : 'a t -> ('a Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

  val print : Format.formatter -> 'a t -> unit
end

module Apron : sig
  type 'a t = ('a, ApronexprDD.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  val of_expr :
    ('a, [> `Apron of ApronexprDD.t ]) Bdd.Env.value -> 'a t

  val to_expr :
    'a t -> ('a, [> `Apron of ApronexprDD.t ]) Bdd.Env.value

  val extend_environment : 'a t -> 'a -> 'a t

  val var : 'a -> string -> 'a t
  val cst : 'a -> Apron.Coeff.t -> 'a t
  val add :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val mul :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val sub :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val div :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t
  val gmod :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    'a t -> 'a t -> 'a t

  val negate : 'a t -> 'a t
  val sqrt : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t
  val cast : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t

  val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t

  val condition : Apron.Tcons1.typ -> (('b, 'c, Env.cond) #Env.O.t as 'a) t -> 'a Bool.t
  val supeq : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> 'a Bool.t
  val sup : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> 'a Bool.t
  val eq : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> 'a Bool.t
  val cofactor : 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t

  val substitute_by_var : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * string) list -> 'a t
  val substitute : (('b, 'c, Env.cond) #Env.O.t as 'a) t -> (string * 'a expr) list -> 'a t

  val print : Format.formatter -> 'a t -> unit
end

val typ_of_expr : 'a expr -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

val make : 'a -> Expr0.t -> 'a t
  (** Creation from an expression without environment *)

val extend_environment : 'a t -> 'a -> 'a t
val var : 'a -> string -> 'a t
  (** Expression representing the litteral var *)
val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)
val eq : (('b, 'c, Env.cond) #Env.O.t as 'a) expr -> 'a t -> 'a Bool.t
  (** Equality operation *)

val substitute_by_var :
  (('b, 'c, Env.cond) #Env.O.t as 'a) expr ->
  (string * string) list -> 'a t
    (** Variable renaming.
      The new variables should already have been declared *)
val substitute :
  (('b, 'c, Env.cond) #Env.O.t as 'a) expr ->
  (string * 'a t) list -> 'a t
    (** Parallel substitution of variables by expressions *)

val support : (('b, 'c, Env.cond) #Env.O.t as 'a) expr -> SetteS.t
    (** Support of the expression *)
val support_cond : 'a t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

val cofactor : 'a t -> 'a Bool.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
val restrict : 'a t -> 'a Bool.t -> 'a t
val tdrestrict : 'a t -> 'a Bool.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val print : Format.formatter -> 'a t -> unit
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

val make_env :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t -> Env.t

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)
type t = (Env.t, Expr0.t) Bdd.Env.value
type expr = t
  (** Type of general expressions *)

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type t = (Env.t, Cudd.Man.v Cudd.Bdd.t) Bdd.Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr

  val extend_environment : t -> Env.t -> t

  val dtrue : Env.t -> t
  val dfalse : Env.t -> t
  val of_bool : Env.t -> bool -> t
  val var : Env.t -> string -> t

  (** {4 Logical connectors} *)

  val dnot : t -> t
  val dand : t -> t -> t
  val dor : t -> t -> t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
      keywords) *)

  val xor : t -> t -> t
  val nand : t -> t -> t
  val nor : t -> t -> t
  val nxor : t -> t -> t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : t -> t -> t
    (** Same as [nxor] *)
  val leq : t -> t -> t
    (** Implication *)

  val ite : t -> t -> t -> t
    (** If-then-else *)

  val is_true : t -> bool
  val is_false : t -> bool
  val is_cst : t -> bool
  val is_eq : t -> t -> bool
  val is_leq : t -> t -> bool
  val is_inter_false : t -> t -> bool

  val exist : string list -> t -> t
  val forall : string list -> t -> t

  val cofactor : t -> t -> t
  val restrict : t -> t -> t
  val tdrestrict : t -> t -> t

  val substitute_by_var : t -> (string * string) list -> t
  val substitute : t -> (string * expr) list -> t

  val print : Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type t = (Env.t, Cudd.Man.v Bdd.Int.t) Bdd.Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr
  val extend_environment : t -> Env.t -> t

  val of_int :
    Env.t -> [`Tbint of bool * int ] -> int -> t
  val var : Env.t -> string -> t

  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val shift_left : int -> t -> t
  val shift_right : int -> t -> t
  val scale : int -> t -> t
  val ite : Bool.t -> t -> t -> t
  val zero : t -> Bool.t
  val eq : t -> t -> Bool.t
  val supeq : t -> t -> Bool.t
  val sup : t -> t -> Bool.t
  val eq_int : t -> int -> Bool.t
  val supeq_int : t -> int -> Bool.t

  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : t -> (string * string) list -> t
  val substitute : t -> (string * expr) list -> t

  val guard_of_int: t -> int -> Bool.t
    (** Return the guard of the integer value. *)
  val guardints: t -> (Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type t = (Env.t, Cudd.Man.v Bdd.Enum.t) Bdd.Env.value
  val of_expr : expr -> t
  val to_expr : t -> expr
  val extend_environment : t -> Env.t -> t

  val var : Env.t -> string -> t
  val ite : Bool.t -> t -> t -> t
  val eq : t -> t -> Bool.t
  val eq_label : t -> string -> Bool.t

  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : t -> (string * string) list -> t
  val substitute : t -> (string * expr) list -> t

  val guard_of_label : t -> string -> Bool.t
    (** Return the guard of the label. *)

  val guardlabels : t -> (Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

  val print : Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Arithmetic expressions} *)
(*  ====================================================================== *)
module Apron : sig
  type t = (Env.t, ApronexprDD.t) Bdd.Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr

  val extend_environment : t -> Env.t -> t

  val var : Env.t -> string -> t
  val cst : Env.t -> Apron.Coeff.t -> t
  val add :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod :
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t

  val negate : t -> t
  val sqrt : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t
  val cast : ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t

  val ite : Bool.t -> t -> t -> t

  val condition : Apron.Tcons1.typ -> t -> Bool.t
  val supeq : t -> Bool.t
  val sup : t -> Bool.t
  val eq : t -> Bool.t
  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : t -> (string * string) list -> t
  val substitute : t -> (string * expr) list -> t

  val print : Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Operations on general expressions} *)
(*  ====================================================================== *)

val typ_of_expr : t -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

(*
val make : Env.t -> Expr0.t -> expr
*)
  (** Creation from an expression without environment *)

val extend_environment : t -> Env.t -> t
val var : Env.t -> string -> t
  (** Expression representing the litteral var *)
val ite : Bool.t -> t -> t -> t
  (** If-then-else operation *)
val eq : t -> t -> Bool.t
  (** Equality operation *)

val substitute_by_var : t -> (string * string) list -> t
    (** Variable renaming.
      The new variables should already have been declared *)
val substitute : t -> (string * t) list -> t
    (** Parallel substitution of variables by expressions *)

val support : t -> SetteS.t
    (** Support of the expression *)
val support_cond : t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

val cofactor : t -> Bool.t -> t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
val restrict : t -> Bool.t -> t
val tdrestrict : t -> Bool.t -> t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val print : Format.formatter -> t -> unit


