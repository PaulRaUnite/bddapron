(** Finite-type and arithmetical expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type t = [
  | Cudd.Man.v Bdd.Expr0.t
  | `Apron of ApronexprDD.t
]
type expr = t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig

val make_env :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
  ('a, 'b, Env.cond) Env.O.t

val check_typ2 :
  [< t] -> [< t] -> [> Env.typ]

module Bool : sig
  type t = Cudd.Man.v Cudd.Bdd.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val dtrue : ('a,'b,'c) #Env.O.t -> t
  val dfalse : ('a,'b,'c) #Env.O.t -> t
  val of_bool : ('a,'b,'c) #Env.O.t -> bool -> t
  val var : ('a,'b,'c) #Env.O.t -> string -> t
  val ite : ('a,'b,'c) #Env.O.t -> t -> t -> t -> t

  val dnot : ('a,'b,'c) #Env.O.t -> t -> t
  val dand : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val dor : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val xor : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val nand : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val nor : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val nxor : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val leq : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val eq : ('a,'b,'c) #Env.O.t -> t -> t -> t

  val is_true : ('a,'b,'c) #Env.O.t -> t -> bool
  val is_false : ('a,'b,'c) #Env.O.t -> t -> bool
  val is_cst : ('a,'b,'c) #Env.O.t -> t -> bool
  val is_leq : ('a,'b,'c) #Env.O.t -> t -> t -> bool
  val is_eq : ('a,'b,'c) #Env.O.t -> t -> t -> bool
  val is_and_false : ('a,'b,'c) #Env.O.t -> t -> t -> bool
  val exist : ('a,'b,'c) #Env.O.t -> string list -> t -> t
  val forall : ('a,'b,'c) #Env.O.t -> string list -> t -> t

  val cofactor : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val restrict : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val tdrestrict : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,Env.cond) #Env.O.t -> t -> (string*string) list -> t
  val substitute : ('a,'b,Env.cond) #Env.O.t -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #Env.O.t -> Format.formatter -> t -> unit
end

module Bint : sig
  type t = Cudd.Man.v Bdd.Int.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val of_int : ('a,'b,'c) #Env.O.t -> [> `Tbint of bool * int ] -> int -> t
  val var : ('a,'b,'c) #Env.O.t -> string -> t
  val ite : ('a,'b,'c) #Env.O.t -> Bool.t -> t -> t -> t

  val neg : ('a,'b,'c) #Env.O.t -> t -> t
  val succ : ('a,'b,'c) #Env.O.t -> t -> t
  val pred : ('a,'b,'c) #Env.O.t -> t -> t
  val add : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val sub : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val mul : ('a,'b,'c) #Env.O.t -> t -> t -> t
  val shift_left : ('a,'b,'c) #Env.O.t -> int -> t -> t
  val shift_right : ('a,'b,'c) #Env.O.t -> int -> t -> t
  val scale : ('a,'b,'c) #Env.O.t -> int -> t -> t
  val zero : ('a,'b,'c) #Env.O.t -> t -> Bool.t
  val eq : ('a,'b,'c) #Env.O.t -> t -> t -> Bool.t
  val eq_int : ('a,'b,'c) #Env.O.t -> t -> int -> Bool.t
  val supeq : ('a,'b,'c) #Env.O.t -> t -> t -> Bool.t
  val supeq_int : ('a,'b,'c) #Env.O.t -> t -> int -> Bool.t
  val sup : ('a,'b,'c) #Env.O.t -> t -> t -> Bool.t

  val cofactor : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val restrict : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val tdrestrict : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,Env.cond) #Env.O.t -> t -> (string*string) list -> t
  val substitute : ('a,'b,Env.cond) #Env.O.t -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #Env.O.t -> Format.formatter -> t -> unit
end

module Benum : sig
  type t = Cudd.Man.v Bdd.Enum.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val var : ('a,'b,'c) #Env.O.t -> string -> t
  val ite : ('a,'b,'c) #Env.O.t -> Bool.t -> t -> t -> t
  val eq : ('a,'b,'c) #Env.O.t -> t -> t -> Bool.t
  val eq_label : ('a,'b,'c) #Env.O.t -> t -> string -> Bool.t
  val cofactor : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val restrict : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val tdrestrict : ('a,'b,'c) #Env.O.t -> t -> Bool.t -> t
  val permute : t -> int array -> t
  val substitute_by_var : ('a,'b,Env.cond) #Env.O.t -> t -> (string*string) list -> t
  val substitute : ('a,'b,Env.cond) #Env.O.t -> t -> (string*expr) list -> t
  val print : ('a,'b,'c) #Env.O.t -> Format.formatter -> t -> unit
end

module Apron : sig
  type t = ApronexprDD.t
  val of_expr : [> `Apron of t ] -> t
  val to_expr : t -> [> `Apron of t ]
  val cst : ('a,'b,'c) #Env.O.t -> Apron.Coeff.t -> t
  val var : ('a,'b,'c) #Env.O.t -> string -> t
  val add :('a,'b,'c) #Env.O.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : ('a,'b,'c) #Env.O.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : ('a,'b,'c) #Env.O.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : ('a,'b,'c) #Env.O.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : ('a,'b,'c) #Env.O.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val negate : ('a,'b,'c) #Env.O.t -> t -> t
  val cast :
    ('a,'b,'c) #Env.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
  val sqrt :
    ('a,'b,'c) #Env.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t

  val supeq : ('a,'b,'c) #Env.O.t -> t -> Bool.t
  val sup : ('a,'b,'c) #Env.O.t -> t -> Bool.t
  val eq : ('a,'b,'c) #Env.O.t -> t -> Bool.t

  val ite : ('a,'b,'c) #Env.O.t -> Bool.t -> t -> t -> t

  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : ('a,'b,Env.cond) #Env.O.t -> t -> (string*string) list -> t
  val substitute : ('a,'b,Env.cond) #Env.O.t -> t -> (string*expr) list -> t

  val print : ('a,'b,'c) #Env.O.t -> Format.formatter -> t -> unit
end

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr: [<t] -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

val var : ('a,'b,'c) #Env.O.t -> string -> t
  (** Expression representing the litteral var *)

val ite : ('a,'b,'c) #Env.O.t -> Bool.t -> t -> t -> t
  (** If-then-else operation *)

val substitute_by_var : ('a,'b,Env.cond) #Env.O.t -> t -> (string*string) list -> t
    (** Parallel substitution of variables by variables *)
val substitute : ('a,'b,Env.cond) #Env.O.t -> t -> (string*t) list -> t
    (** Parallel substitution of variables by expressions *)

val support : ('a,'b,Env.cond) #Env.O.t -> t -> SetteS.t
    (** Return the full support of the expression *)

val eq : ('a,'b,Env.cond) #Env.O.t -> t -> t -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
      arithmetic expressions, do not take into account the careset. *)

val support_cond : ('a,'b,'c) #Env.O.t -> t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

(** Printing functions *)

val print : ('a,'b,'c) #Env.O.t -> Format.formatter -> [<t] -> unit
val print_bdd : ('a,'b,'c) #Env.O.t -> Format.formatter -> Cudd.Man.v Cudd.Bdd.t -> unit

val compose_of_substitution :
  ('a,'b,Env.cond) #Env.O.t -> (string*t) list -> Cudd.Man.v Cudd.Bdd.t array option * t MappeS.t

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

val make_env :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t -> Env.t

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type t = Cudd.Man.v Cudd.Bdd.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val dtrue : Env.t -> t
  val dfalse : Env.t -> t
  val of_bool : Env.t -> bool -> t
  val var : Env.t -> string -> t
  val ite : Env.t -> t -> t -> t -> t

  val dnot : Env.t -> t -> t
  val dand : Env.t -> t -> t -> t
  val dor : Env.t -> t -> t -> t
  val xor : Env.t -> t -> t -> t
  val nand : Env.t -> t -> t -> t
  val nor : Env.t -> t -> t -> t
  val nxor : Env.t -> t -> t -> t
  val leq : Env.t -> t -> t -> t
  val eq : Env.t -> t -> t -> t

  val is_true : Env.t -> t -> bool
  val is_false : Env.t -> t -> bool
  val is_cst : Env.t -> t -> bool
  val is_leq : Env.t -> t -> t -> bool
  val is_eq : Env.t -> t -> t -> bool
  val is_and_false : Env.t -> t -> t -> bool
  val exist : Env.t -> string list -> t -> t
  val forall : Env.t -> string list -> t -> t

  val cofactor : Env.t -> t -> t -> t
  val restrict : Env.t -> t -> t -> t
  val tdrestrict : Env.t -> t -> t -> t
  val permute : t -> int array -> t

  val substitute_by_var : Env.t -> t -> (string*string) list -> t
  val substitute : Env.t -> t -> (string*expr) list -> t

  val print : Env.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type t = Cudd.Man.v Bdd.Int.t
  val of_expr : expr -> t
  val to_expr : t -> expr

  val of_int : Env.t -> [`Tbint of bool * int ] -> int -> t
  val var : Env.t -> string -> t
  val ite : Env.t -> Bool.t -> t -> t -> t

  val neg : Env.t -> t -> t
  val succ : Env.t -> t -> t
  val pred : Env.t -> t -> t
  val add : Env.t -> t -> t -> t
  val sub : Env.t -> t -> t -> t
  val mul : Env.t -> t -> t -> t
  val shift_left : Env.t -> int -> t -> t
  val shift_right : Env.t -> int -> t -> t
  val scale : Env.t -> int -> t -> t
  val zero : Env.t -> t -> Bool.t
  val eq : Env.t -> t -> t -> Bool.t
  val eq_int : Env.t -> t -> int -> Bool.t
  val supeq : Env.t -> t -> t -> Bool.t
  val supeq_int : Env.t -> t -> int -> Bool.t
  val sup : Env.t -> t -> t -> Bool.t

  val cofactor : Env.t -> t -> Bool.t -> t
  val restrict : Env.t -> t -> Bool.t -> t
  val tdrestrict : Env.t -> t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : Env.t -> t -> (string*string) list -> t
  val substitute : Env.t -> t -> (string*expr) list -> t

  val print : Env.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type t = Cudd.Man.v Bdd.Enum.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val var : Env.t -> string -> t
  val ite : Env.t -> Bool.t -> t -> t -> t
  val eq : Env.t -> t -> t -> Bool.t
  val eq_label : Env.t -> t -> string -> Bool.t
  val cofactor : Env.t -> t -> Bool.t -> t
  val restrict : Env.t -> t -> Bool.t -> t
  val tdrestrict : Env.t -> t -> Bool.t -> t
  val permute : t -> int array -> t
  val substitute_by_var : Env.t -> t -> (string*string) list -> t
  val substitute : Env.t -> t -> (string*expr) list -> t
  val print : Env.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Arithmetic expressions} *)
(*  ====================================================================== *)

module Apron : sig
  type t = ApronexprDD.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val cst : Env.t -> Apron.Coeff.t -> t
  val var : Env.t -> string -> t
  val add :Env.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : Env.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : Env.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : Env.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : Env.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val negate : Env.t -> t -> t
  val cast :
    Env.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
  val sqrt :
    Env.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t

  val supeq : Env.t -> t -> Bool.t
  val sup : Env.t -> t -> Bool.t
  val eq : Env.t -> t -> Bool.t

  val ite : Env.t -> Bool.t -> t -> t -> t

  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t

  val substitute_by_var : Env.t -> t -> (string*string) list -> t
  val substitute : Env.t -> t -> (string*expr) list -> t

  val print : Env.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Operations on general expressions} *)
(*  ====================================================================== *)

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr: t -> [
    | `Bool
    | `Bint of bool * int
    | `Benum of string
    | `Real
  ]
  (** Type of an expression *)

val var : Env.t -> string -> t
  (** Expression representing the litteral var *)

val ite : Env.t -> Bool.t -> t -> t -> t
  (** If-then-else operation *)

val cofactor : t -> Bool.t -> t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val substitute_by_var : Env.t -> t -> (string*string) list -> t
    (** Parallel substitution of variables by variables *)
val substitute : Env.t -> t -> (string*t) list -> t
    (** Parallel substitution of variables by expressions *)

val restrict : t -> Bool.t -> t
val tdrestrict : t -> Bool.t -> t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val permute : t -> int array -> t
  (** Permutation (rather internal) *)

val support : Env.t -> t -> SetteS.t
    (** Return the full support of the expression *)

val eq : Env.t -> t -> t -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
      arithmetic expressions, do not take into account the careset. *)

val support_cond : Env.t -> t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

(** Printing functions *)

val print : Env.t -> Format.formatter -> [<t] -> unit
