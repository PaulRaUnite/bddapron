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

  val check_typ2 :
    [< t] -> [< t] -> [> Env.typ]

  module Bool : sig
    type t = Cudd.Man.v Cudd.Bdd.t
    val of_expr : expr -> t
    val to_expr : t -> expr

    val dtrue : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t
    val dfalse : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t
    val of_bool : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> bool -> t
    val var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string -> t
    val ite : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t -> t

    val dnot : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t
    val dand : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val dor : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val xor : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val nand : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val nor : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val nxor : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val leq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t

    val is_true : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> bool
    val is_false : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> bool
    val is_cst : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> bool
    val is_leq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> bool
    val is_eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> bool
    val is_and_false : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> bool
    val exist : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string list -> t -> t
    val forall : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string list -> t -> t

    val cofactor :  t -> t -> t
    val restrict :  t -> t -> t
    val tdrestrict :  t -> t -> t
    val permute : t -> int array -> t

    val substitute_by_var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*string) list -> t
    val substitute : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*expr) list -> t

    val print : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> t -> unit
  end

  module Bint : sig
    type t = Cudd.Man.v Bdd.Int.t
    val of_expr : expr -> t
    val to_expr : t -> expr

    val of_int : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> [> `Tbint of bool * int ] -> int -> t
    val var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string -> t
    val ite : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Bool.t -> t -> t -> t

    val neg : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t
    val succ : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t
    val pred : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t
    val add : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val sub : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val mul : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> t
    val shift_left : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> int -> t -> t
    val shift_right : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> int -> t -> t
    val scale : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> int -> t -> t
    val zero : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> Bool.t
    val eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> Bool.t
    val eq_int : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> int -> Bool.t
    val supeq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> Bool.t
    val supeq_int : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> int -> Bool.t
    val sup : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> Bool.t

    val cofactor :  t -> Bool.t -> t
    val restrict :  t -> Bool.t -> t
    val tdrestrict :  t -> Bool.t -> t
    val permute : t -> int array -> t

    val substitute_by_var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*string) list -> t
    val substitute : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*expr) list -> t

    val print : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> t -> unit
  end

  module Benum : sig
    type t = Cudd.Man.v Bdd.Enum.t
    val of_expr : expr -> t
    val to_expr : t -> expr
    val var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string -> t
    val ite : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Bool.t -> t -> t -> t
    val eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> Bool.t
    val eq_label : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> string -> Bool.t
    val cofactor :  t -> Bool.t -> t
    val restrict :  t -> Bool.t -> t
    val tdrestrict :  t -> Bool.t -> t
    val permute : t -> int array -> t
    val substitute_by_var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*string) list -> t
    val substitute : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*expr) list -> t
    val print : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> t -> unit
  end

  module Apron : sig
    type t = ApronexprDD.t
    val of_expr : [> `Apron of t ] -> t
    val to_expr : t -> [> `Apron of t ]
    val cst : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Apron.Coeff.t -> t
    val var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string -> t
    val add :(('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t -> t
    val sub : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t -> t
    val mul : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t -> t
    val div : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t -> t
    val gmod : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t ->
      ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t -> t
    val negate : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t
    val cast :
      (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t
    val sqrt :
      (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
      t -> t

    val supeq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> Bool.t
    val sup : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> Bool.t
    val eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> Bool.t

    val ite : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Bool.t -> t -> t -> t

    val cofactor :  t -> Bool.t -> t
    val restrict : t -> Bool.t -> t
    val tdrestrict : t -> Bool.t -> t
    val permute : t -> int array -> t

    val substitute_by_var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*string) list -> t
    val substitute : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*expr) list -> t

    val print : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> t -> unit
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
    
  val var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> string -> t
  (** Expression representing the litteral var *)
    
  val ite : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Bool.t -> t -> t -> t
  (** If-then-else operation *)
    
  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t
  val permute_list : t list -> int array -> t list
 
  val substitute_by_var : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*string) list -> t
    (** Parallel substitution of variables by variables *)
  val substitute : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> (string*t) list -> t
    (** Parallel substitution of variables by expressions *)
    
  val support : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> string PSette.t
    (** Return the full support of the expression *)
    
  val eq : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> t -> t -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
	arithmetic expressions, do not take into account the careset. *)
    
  val support_cond : < cudd : Cudd.Man.v Cudd.Man.t; .. > -> t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)
    
  (** Printing functions *)
    
  val print : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> [<t] -> unit
  val print_bdd : (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> Format.formatter -> Cudd.Man.v Cudd.Bdd.t -> unit
  
  val normalize : 
    ?reduce:bool -> ?careset:bool -> 
    ((Cond.cond,'a) #Cond.O.t as 'b) * t list ->
    'b * t list
  
  val compose_of_lvarexpr :
    (('a,'b) #Env.O.t as 'c) -> (Cond.cond,'c) #Cond.O.t -> (string*t) list -> Cudd.Man.v Cudd.Bdd.t array option * (string, t) PMappe.t
    
end
  
(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)
  
(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)
  
module Bool : sig
  type t = Cudd.Man.v Cudd.Bdd.t
  val of_expr : expr -> t
  val to_expr : t -> expr
    
  val dtrue : Env.t -> Cond.t -> t
  val dfalse : Env.t -> Cond.t -> t
  val of_bool : Env.t -> Cond.t -> bool -> t
  val var : Env.t -> Cond.t -> string -> t
  val ite : Env.t -> Cond.t -> t -> t -> t -> t
    
  val dnot : Env.t -> Cond.t -> t -> t
  val dand : Env.t -> Cond.t -> t -> t -> t
  val dor : Env.t -> Cond.t -> t -> t -> t
  val xor : Env.t -> Cond.t -> t -> t -> t
  val nand : Env.t -> Cond.t -> t -> t -> t
  val nor : Env.t -> Cond.t -> t -> t -> t
  val nxor : Env.t -> Cond.t -> t -> t -> t
  val leq : Env.t -> Cond.t -> t -> t -> t
  val eq : Env.t -> Cond.t -> t -> t -> t
    
  val is_true : Env.t -> Cond.t -> t -> bool
  val is_false : Env.t -> Cond.t -> t -> bool
  val is_cst : Env.t -> Cond.t -> t -> bool
  val is_leq : Env.t -> Cond.t -> t -> t -> bool
  val is_eq : Env.t -> Cond.t -> t -> t -> bool
  val is_and_false : Env.t -> Cond.t -> t -> t -> bool
  val exist : Env.t -> Cond.t -> string list -> t -> t
  val forall : Env.t -> Cond.t -> string list -> t -> t
    
  val cofactor : t -> t -> t
  val restrict : t -> t -> t
  val tdrestrict : t -> t -> t
  val permute : t -> int array -> t
    
  val substitute_by_var : Env.t -> Cond.t -> t -> (string*string) list -> t
  val substitute : Env.t -> Cond.t -> t -> (string*expr) list -> t
    
  val print : Env.t -> Cond.t -> Format.formatter -> t -> unit
end
  
(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)
  
module Bint : sig
  type t = Cudd.Man.v Bdd.Int.t
  val of_expr : expr -> t
  val to_expr : t -> expr
    
  val of_int : Env.t -> Cond.t -> [`Tbint of bool * int ] -> int -> t
  val var : Env.t -> Cond.t -> string -> t
  val ite : Env.t -> Cond.t -> Bool.t -> t -> t -> t
    
  val neg : Env.t -> Cond.t -> t -> t
  val succ : Env.t -> Cond.t -> t -> t
  val pred : Env.t -> Cond.t -> t -> t
  val add : Env.t -> Cond.t -> t -> t -> t
  val sub : Env.t -> Cond.t -> t -> t -> t
  val mul : Env.t -> Cond.t -> t -> t -> t
  val shift_left : Env.t -> Cond.t -> int -> t -> t
  val shift_right : Env.t -> Cond.t -> int -> t -> t
  val scale : Env.t -> Cond.t -> int -> t -> t
  val zero : Env.t -> Cond.t -> t -> Bool.t
  val eq : Env.t -> Cond.t -> t -> t -> Bool.t
  val eq_int : Env.t -> Cond.t -> t -> int -> Bool.t
  val supeq : Env.t -> Cond.t -> t -> t -> Bool.t
  val supeq_int : Env.t -> Cond.t -> t -> int -> Bool.t
  val sup : Env.t -> Cond.t -> t -> t -> Bool.t
    
  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t
    
  val substitute_by_var : Env.t -> Cond.t -> t -> (string*string) list -> t
  val substitute : Env.t -> Cond.t -> t -> (string*expr) list -> t
    
  val print : Env.t -> Cond.t -> Format.formatter -> t -> unit
end
  
(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)
  
module Benum : sig
  type t = Cudd.Man.v Bdd.Enum.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val var : Env.t -> Cond.t -> string -> t
  val ite : Env.t -> Cond.t -> Bool.t -> t -> t -> t
  val eq : Env.t -> Cond.t -> t -> t -> Bool.t
  val eq_label : Env.t -> Cond.t -> t -> string -> Bool.t
  val cofactor : t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t
  val substitute_by_var : Env.t -> Cond.t -> t -> (string*string) list -> t
  val substitute : Env.t -> Cond.t -> t -> (string*expr) list -> t
  val print : Env.t -> Cond.t -> Format.formatter -> t -> unit
end
  
(*  ====================================================================== *)
(** {3 Arithmetic expressions} *)
(*  ====================================================================== *)
  
module Apron : sig
  type t = ApronexprDD.t
  val of_expr : expr -> t
  val to_expr : t -> expr
  val cst : Env.t -> Cond.t -> Apron.Coeff.t -> t
  val var : Env.t -> Cond.t -> string -> t
  val add : Env.t -> Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val sub : Env.t -> Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val mul : Env.t -> Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val div : Env.t -> Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val gmod : Env.t -> Cond.t ->
    ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t -> t
  val negate : Env.t -> Cond.t -> t -> t
  val cast :
    Env.t -> Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
  val sqrt :
    Env.t -> Cond.t -> ?typ:Apron.Texpr1.typ -> ?round:Apron.Texpr1.round ->
    t -> t
    
  val supeq : Env.t -> Cond.t -> t -> Bool.t
  val sup : Env.t -> Cond.t -> t -> Bool.t
  val eq : Env.t -> Cond.t -> t -> Bool.t
    
  val ite : Env.t -> Cond.t -> Bool.t -> t -> t -> t
    
  val cofactor :  t -> Bool.t -> t
  val restrict : t -> Bool.t -> t
  val tdrestrict : t -> Bool.t -> t
  val permute : t -> int array -> t
    
  val substitute_by_var : Env.t -> Cond.t -> t -> (string*string) list -> t
  val substitute : Env.t -> Cond.t -> t -> (string*expr) list -> t
    
  val print : Env.t -> Cond.t -> Format.formatter -> t -> unit
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
  
val var : Env.t -> Cond.t -> string -> t
  (** Expression representing the litteral var *)
  
val ite : Env.t -> Cond.t -> Bool.t -> t -> t -> t
  (** If-then-else operation *)
  
val cofactor : t -> Bool.t -> t
    (** Evaluate the expression. The BDD is assumed to be a cube *)
  
val substitute_by_var : Env.t -> Cond.t -> t -> (string*string) list -> t
    (** Parallel substitution of variables by variables *)
val substitute : Env.t -> Cond.t -> t -> (string*t) list -> t
    (** Parallel substitution of variables by expressions *)
  
val restrict : t -> Bool.t -> t
val tdrestrict : t -> Bool.t -> t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
	[cofactor]. *)
  
val permute : t -> int array -> t
  (** Permutation (rather internal) *)
  
val support : Env.t -> Cond.t -> t -> string PSette.t
    (** Return the full support of the expression *)
  
val eq : Env.t -> Cond.t -> t -> t -> Bool.t
    (** Under which condition are the expressions equal ?  In case of
	arithmetic expressions, do not take into account the careset. *)
  
val support_cond : Env.t -> Cond.t -> t -> Cudd.Man.v Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
	identifiers involved in the expression *)
  
(** Printing functions *)
  
val print : Env.t -> Cond.t -> Format.formatter -> [<t] -> unit
  
val normalize : ?reduce:bool -> ?careset:bool -> Cond.t * t list -> Cond.t * t list 
