(** Finite-type and arithmetical expressions linked to normalized environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
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
  
  type 'a t = ('a, Expr0.t) Env.value
  constraint 'a = ('b,'c,'d) Env.O.t

  type 'a expr = 'a t
    (** Type of general expressions *)
    
  module Bool : sig
    type 'a t = ('a, Expr0.Bool.t) Env.value
    constraint 'a = ('b,'c,'d) Env.O.t

    val of_expr : ('a, [> `Bool of Expr0.Bool.t ]) Env.value -> 'a t
    val to_expr : 'a t -> ('a, [> `Bool of Expr0.Bool.t ]) Env.value

    val extend_environment : 'a t -> 'a -> 'a t

    val dtrue : 'a -> 'a Cond.O.t -> 'a t
    val dfalse : 'a -> 'a Cond.O.t -> 'a t
    val of_bool : 'a -> 'a Cond.O.t -> bool -> 'a t
    val var : 'a -> 'a Cond.O.t -> string -> 'a t

    (** {4 Logical connectors} *)

    val dnot : 'a Cond.O.t -> 'a t -> 'a t
    val dand : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val dor : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

    val xor : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val nand : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val nor : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val nxor : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    (** Exclusive or, not and, nor or and not xor *)

    val eq : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    (** Same as [nxor] *)
    val leq : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    (** Implication *)

    val ite : 'a Cond.O.t -> 'a t -> 'a t -> 'a t -> 'a t
    (** If-then-else *)

    val is_true : 'a Cond.O.t -> 'a t -> bool
    val is_false : 'a Cond.O.t -> 'a t -> bool
    val is_cst : 'a Cond.O.t -> 'a t -> bool
    val is_eq : 'a Cond.O.t -> 'a t -> 'a t -> bool
    val is_leq : 'a Cond.O.t -> 'a t -> 'a t -> bool
    val is_inter_false : 'a Cond.O.t -> 'a t -> 'a t -> bool

    val exist : 'a Cond.O.t -> string list -> 'a t -> 'a t
    val forall : 'a Cond.O.t -> string list -> 'a t -> 'a t

    val cofactor : 'a t -> 'a t -> 'a t
    val restrict : 'a t -> 'a t -> 'a t
    val tdrestrict : 'a t -> 'a t -> 'a t

    val substitute_by_var : 'a Cond.O.t -> 'a t -> (string * string) list -> 'a t
    val substitute : 'a Cond.O.t -> 'a t -> (string * 'a expr) list -> 'a t

    val print : 'a Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Bint : sig
    type 'a t = ('a, Cudd.Man.v Bdd.Int.t) Env.value
    constraint 'a = ('b,'c,'d) Env.O.t

    val of_expr : ('a, [> `Bint of Expr0.Bint.t ]) Env.value -> 'a t
    val to_expr : 'a t -> ('a, [> `Bint of Expr0.Bint.t ]) Env.value
    val extend_environment : 'a t -> 'a -> 'a t

    val of_int :
      'a -> 'a Cond.O.t -> [`Bint of bool * int ] -> int -> 'a t
    val var : 'a -> 'a Cond.O.t -> string -> 'a t

    val neg : 'a Cond.O.t -> 'a t -> 'a t
    val succ : 'a Cond.O.t -> 'a t -> 'a t
    val pred : 'a Cond.O.t -> 'a t -> 'a t
    val add : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val sub : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val mul : 'a Cond.O.t -> 'a t -> 'a t -> 'a t
    val shift_left : 'a Cond.O.t -> int -> 'a t -> 'a t
    val shift_right : 'a Cond.O.t -> int -> 'a t -> 'a t
    val scale : 'a Cond.O.t -> int -> 'a t -> 'a t
    val ite : 'a Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
    val zero : 'a Cond.O.t -> 'a t -> 'a Bool.t
    val eq : 'a Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val supeq : 'a Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val sup : 'a Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val eq_int : 'a Cond.O.t -> 'a t -> int -> 'a Bool.t
    val supeq_int : 'a Cond.O.t -> 'a t -> int -> 'a Bool.t
    val sup_int : 'a Cond.O.t -> 'a t -> int -> 'a Bool.t

    val cofactor : 'a t -> 'a Bool.t -> 'a t
    val restrict : 'a t -> 'a Bool.t -> 'a t
    val tdrestrict : 'a t -> 'a Bool.t -> 'a t

    val substitute_by_var : 'a Cond.O.t -> 'a t -> (string * string) list -> 'a t
    val substitute : 'a Cond.O.t -> 'a t -> (string * 'a expr) list -> 'a t

    val guard_of_int : 'a Cond.O.t -> 'a t -> int -> 'a Bool.t
    (** Return the guard of the integer value. *)
    val guardints : 'a Cond.O.t -> 'a t -> ('a Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

    val print : 'a Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Benum : sig
    type 'a t = ('a, Cudd.Man.v Bdd.Enum.t) Env.value
    constraint 'a = ('b,'c,'d) Env.O.t
    val of_expr : ('a, [> `Benum of Expr0.Benum.t ]) Env.value -> 'a t
    val to_expr : 'a t -> ('a, [> `Benum of Expr0.Benum.t ]) Env.value
    val extend_environment : 'a t -> 'a -> 'a t

    val var : 'a -> 'a Cond.O.t -> string -> 'a t
    val ite : 'a Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
    val eq : 'a Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
    val eq_label : 'a Cond.O.t -> 'a t -> string -> 'a Bool.t

    val cofactor : 'a t -> 'a Bool.t -> 'a t
    val restrict : 'a t -> 'a Bool.t -> 'a t
    val tdrestrict : 'a t -> 'a Bool.t -> 'a t

    val substitute_by_var : 'a Cond.O.t -> 'a t -> (string * string) list -> 'a t
    val substitute : 'a Cond.O.t -> 'a t -> (string * 'a expr) list -> 'a t

    val guard_of_label : 'a Cond.O.t -> 'a t -> string -> 'a Bool.t
    (** Return the guard of the label. *)

    val guardlabels : 'a Cond.O.t -> 'a t -> ('a Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

    val print : 'a Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  module Apron : sig
    type 'a t = ('a, Expr0.Apron.t) Env.value
    constraint 'a = ('b,'c,'d) Env.O.t

    val of_expr :
      ('a, [> `Apron of Expr0.Apron.t ]) Env.value -> 'a t

    val to_expr :
      'a t -> ('a, [> `Apron of Expr0.Apron.t ]) Env.value

    val extend_environment : 'a t -> 'a -> 'a t

    val var : 'a -> 'a Cond.O.t -> string -> 'a t
    val cst : 'a -> 'a Cond.O.t -> Apron.Coeff.t -> 'a t
    val add : 'a Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      'a t -> 'a t -> 'a t
    val mul : 'a Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      'a t -> 'a t -> 'a t
    val sub : 'a Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      'a t -> 'a t -> 'a t
    val div : 'a Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      'a t -> 'a t -> 'a t
    val gmod : 'a Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      'a t -> 'a t -> 'a t

    val negate : 'a Cond.O.t -> 'a t -> 'a t
    val sqrt : 'a Cond.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t
    val cast : 'a Cond.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> 'a t -> 'a t

    val ite : 'a Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

    val condition : 'a Cond.O.t -> Apron.Tcons1.typ -> 'a t -> 'a Bool.t
    val supeq : 'a Cond.O.t -> 'a t -> 'a Bool.t
    val sup : 'a Cond.O.t -> 'a t -> 'a Bool.t
    val eq : 'a Cond.O.t -> 'a t -> 'a Bool.t
    val cofactor : 'a t -> 'a Bool.t -> 'a t
    val restrict : 'a t -> 'a Bool.t -> 'a t
    val tdrestrict : 'a t -> 'a Bool.t -> 'a t

    val substitute_by_var : 'a Cond.O.t -> 'a t -> (string * string) list -> 'a t
    val substitute : 'a Cond.O.t -> 'a t -> (string * 'a expr) list -> 'a t

    val print : 'a Cond.O.t -> Format.formatter -> 'a t -> unit
  end

  val typ_of_expr : 'a t -> Env.typ
  (** Type of an expression *)

  val make : 'a -> Expr0.t -> 'a t
  (** Creation from an expression without environment *)

  val extend_environment : 'a t -> 'a -> 'a t
  val var : 'a -> 'a Cond.O.t -> string -> 'a t
  (** Expression representing the litteral var *)
  val ite : 'a Cond.O.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)
  val eq : 'a Cond.O.t -> 'a t -> 'a t -> 'a Bool.t
  (** Equality operation *)

  val substitute_by_var :
    'a Cond.O.t -> 'a t -> (string * string) list -> 'a t
    (** Variable renaming.
	The new variables should already have been declared *)
  val substitute :
    'a Cond.O.t -> 'a t -> (string * 'a t) list -> 'a t
    (** Parallel substitution of variables by expressions *)

  val support : 'a Cond.O.t -> 'a t -> string PSette.t
    (** Support of the expression *)
  val support_cond : Cudd.Man.vt -> 'a t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

  val cofactor : 'a t -> 'a Bool.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
  val restrict : 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a t -> 'a Bool.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

  val print : 'a Cond.O.t -> Format.formatter -> 'a t -> unit

  val normalize : 
    ?reduce:bool -> ?careset:bool -> 
    ('a Cond.O.t as 'b) * 'a t list ->
    'b * 'a t list

  (*  ====================================================================== *)
  (** {3 List of expressions} *)
  (*  ====================================================================== *)

  module List : sig
    type 'a t = ('a, Expr0.t list) Env.value
    constraint 'a = ('b,'c,'d) Env.O.t
      
    val of_lexpr0 : 'a -> Expr0.t list -> 'a t
    val of_lexpr1 : 'a -> 'a expr list -> 'a t
    val extend_environment : 'a t -> 'a -> 'a t
    val normalize : 
      ?reduce:bool -> ?careset:bool -> 
      ('a Cond.O.t as 'b) * 'a t ->
      'b * 'a t
    val print : 
      ?first:(unit,Format.formatter,unit) format ->
      ?sep:(unit,Format.formatter,unit) format ->
      ?last:(unit,Format.formatter,unit) format ->
      'a Cond.O.t -> Format.formatter -> 'a t -> unit
  end
end
  
(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

type t = (Env.t, Expr0.t) Env.value
type expr = t
  (** Type of general expressions *)

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type t = (Env.t, Expr0.Bool.t) Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr

  val extend_environment : t -> Env.t -> t

  val dtrue : Env.t -> Cond.t -> t
  val dfalse : Env.t -> Cond.t -> t
  val of_bool : Env.t -> Cond.t -> bool -> t
  val var : Env.t -> Cond.t -> string -> t

  (** {4 Logical connectors} *)

  val dnot : Cond.t -> t -> t
  val dand : Cond.t -> t -> t -> t
  val dor : Cond.t -> t -> t -> t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
	keywords) *)

  val xor : Cond.t -> t -> t -> t
  val nand : Cond.t -> t -> t -> t
  val nor : Cond.t -> t -> t -> t
  val nxor : Cond.t -> t -> t -> t
    (** Exclusive or, not and, nor or and not xor *)

  val eq : Cond.t -> t -> t -> t
    (** Same as [nxor] *)
  val leq : Cond.t -> t -> t -> t
    (** Implication *)

  val ite : Cond.t -> t -> t -> t -> t
    (** If-then-else *)

  val is_true : Cond.t -> t -> bool
  val is_false : Cond.t -> t -> bool
  val is_cst : Cond.t -> t -> bool
  val is_eq : Cond.t -> t -> t -> bool
  val is_leq : Cond.t -> t -> t -> bool
  val is_inter_false : Cond.t -> t -> t -> bool

  val exist : Cond.t -> string list -> t -> t
  val forall : Cond.t -> string list -> t -> t

  val cofactor : t -> t -> t
  val restrict : t -> t -> t
  val tdrestrict : t -> t -> t

  val substitute_by_var : Cond.t -> t -> (string * string) list -> t
  val substitute : Cond.t -> t -> (string * expr) list -> t

  val print : Cond.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type t = (Env.t, Cudd.Man.v Bdd.Int.t) Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr
  val extend_environment : t -> Env.t -> t

  val of_int : Env.t -> Cond.t -> [`Bint of bool * int ] -> int -> t
  val var : Env.t -> Cond.t -> string -> t

  val neg : Cond.t -> t -> t
  val succ : Cond.t -> t -> t
  val pred : Cond.t -> t -> t
  val add : Cond.t -> t -> t -> t
  val sub : Cond.t -> t -> t -> t
  val mul : Cond.t -> t -> t -> t
  val shift_left : Cond.t -> int -> t -> t
  val shift_right : Cond.t -> int -> t -> t
  val scale : Cond.t -> int -> t -> t
  val ite : Cond.t -> Bool.t -> t -> t -> t
  val zero : Cond.t -> t -> Bool.t
  val eq : Cond.t -> t -> t -> Bool.t
  val supeq : Cond.t -> t -> t -> Bool.t
  val sup : Cond.t -> t -> t -> Bool.t
  val eq_int : Cond.t -> t -> int -> Bool.t
  val supeq_int : Cond.t -> t -> int -> Bool.t
  val sup_int : Cond.t -> t -> int -> Bool.t

  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : Cond.t -> t -> (string * string) list -> t
  val substitute : Cond.t -> t -> (string * expr) list -> t

  val guard_of_int : Cond.t -> t -> int -> Bool.t
    (** Return the guard of the integer value. *)
  val guardints : Cond.t -> t -> (Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : Cond.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type t = (Env.t, Cudd.Man.v Bdd.Enum.t) Env.value
  val of_expr : expr -> t
  val to_expr : t -> expr
  val extend_environment : t -> Env.t -> t

  val var : Env.t -> Cond.t -> string -> t
  val ite : Cond.t -> Bool.t -> t -> t -> t
  val eq : Cond.t -> t -> t -> Bool.t
  val eq_label : Cond.t -> t -> string -> Bool.t

  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : Cond.t -> t -> (string * string) list -> t
  val substitute : Cond.t -> t -> (string * expr) list -> t

  val guard_of_label : Cond.t -> t -> string -> Bool.t
    (** Return the guard of the label. *)

  val guardlabels : Cond.t -> t -> (Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)

  val print : Cond.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Arithmetic expressions} *)
(*  ====================================================================== *)

module Apron : sig
  type t = (Env.t, Expr0.Apron.t) Env.value

  val of_expr : expr -> t
  val to_expr : t -> expr

  val extend_environment : t -> Env.t -> t

  val var : Env.t -> Cond.t -> string -> t
  val cst : Env.t -> Cond.t -> Apron.Coeff.t -> t
  val add : Cond.t -> 
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : Cond.t -> 
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : Cond.t -> 
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : Cond.t -> 
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : Cond.t -> 
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t

  val negate : Cond.t -> t -> t
  val sqrt : Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t
  val cast : Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round -> t -> t

  val ite : Cond.t -> Bool.t -> t -> t -> t

  val condition : Cond.t -> Apron.Tcons1.typ -> t -> Bool.t
  val supeq : Cond.t -> t -> Bool.t
  val sup : Cond.t -> t -> Bool.t
  val eq : Cond.t -> t -> Bool.t
  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t

  val substitute_by_var : Cond.t -> t -> (string * string) list -> t
  val substitute : Cond.t -> t -> (string * expr) list -> t

  val print : Cond.t -> Format.formatter -> t -> unit
end

(*  ====================================================================== *)
(** {3 Operations on general expressions} *)
(*  ====================================================================== *)

val typ_of_expr : t -> Env.typ
  (** Type of an expression *)


val make : Env.t -> Expr0.t -> t
  (** Creation from an expression without environment *)

val extend_environment : t -> Env.t -> t
val var : Env.t -> Cond.t -> string -> t
  (** Expression representing the litteral var *)
val ite : Cond.t -> Bool.t -> t -> t -> t
  (** If-then-else operation *)
val eq : Cond.t -> t -> t -> Bool.t
  (** Equality operation *)

val substitute_by_var : Cond.t -> t -> (string * string) list -> t
    (** Variable renaming.
	The new variables should already have been declared *)
val substitute : Cond.t -> t -> (string * t) list -> t
    (** Parallel substitution of variables by expressions *)

val support : Cond.t -> t -> string PSette.t
    (** Support of the expression *)
val support_cond : Cudd.Man.vt -> t -> Cudd.Bdd.vt
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)

val cofactor : t -> Bool.t -> t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
val restrict : t -> Bool.t -> t
val tdrestrict : t -> Bool.t -> t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)

val print : Cond.t -> Format.formatter -> t -> unit

val normalize : 
  ?reduce:bool -> ?careset:bool -> 
  Cond.t * t list -> Cond.t * t list 

(*  ====================================================================== *)
(** {3 List of expressions} *)
(*  ====================================================================== *)

module List : sig
  type t = (Env.t, Expr0.t list) Env.value
    
  val of_lexpr0 : Env.t -> Expr0.t list -> t
  val of_lexpr1 : Env.t -> expr list -> t
  val extend_environment : t -> Env.t -> t
  val normalize : 
    ?reduce:bool -> ?careset:bool -> 
    Cond.t * t -> Cond.t * t 
  val print :
    ?first:(unit,Format.formatter,unit) format ->
    ?sep:(unit,Format.formatter,unit) format ->
    ?last:(unit,Format.formatter,unit) format ->
    Cond.t -> Format.formatter -> t -> unit
end
