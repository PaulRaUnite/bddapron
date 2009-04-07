(** Finite-type expressions with BDDs *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** This module allows to manipulate structured BDDs, where variables involved
  in the Boolean formula are not only Boolean variables, but also of bounded
  integer or enumerated type (such types are encoded with several Boolean
  variables).
*)

val make_env : ?boolfirst:bool -> ?relational:bool -> 'a Cudd.Man.t -> 'a Env.t
      (** Create a new database.
      [bddindex] and [bddincr] initalized to 0 and 1. *)

(*  ********************************************************************** *)
(** {2 Expressions} *)
(*  ********************************************************************** *)

type 'a t = [
  | `Bool of 'a Cudd.Bdd.t
      (** Boolean *)
  | `Bint of 'a Int.t
      (** Bounded integer *)
  | `Benum of 'a Enum.t
      (** Enumerated *)
]
type 'a expr = 'a t

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool : sig
  type 'a t = 'a Cudd.Bdd.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val dtrue : 'a Env.t -> 'a t
  val dfalse : 'a Env.t -> 'a t
  val of_bool : 'a Env.t -> bool -> 'a t
  val var : 'a Env.t -> string -> 'a t

  val dnot : 'a Env.t -> 'a t -> 'a t
  val dand : 'a Env.t -> 'a t -> 'a t -> 'a t
  val dor : 'a Env.t -> 'a t -> 'a t -> 'a t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
      keywords) *)

  val xor : 'a Env.t -> 'a t -> 'a t -> 'a t
  val nand : 'a Env.t -> 'a t -> 'a t -> 'a t
  val nor : 'a Env.t -> 'a t -> 'a t -> 'a t
  val nxor : 'a Env.t -> 'a t -> 'a t -> 'a t
    (** Exclusive or, not and, nor or and not xor *)

  val leq : 'a Env.t -> 'a t -> 'a t -> 'a t
    (** Implication *)
  val eq : 'a Env.t -> 'a t -> 'a t -> 'a t
    (** Same as [nxor] *)

  val ite : 'a Env.t -> 'a t -> 'a t -> 'a t -> 'a t
    (** If-then-else *)

  val is_true : 'a Env.t -> 'a t -> bool
  val is_false : 'a Env.t -> 'a t -> bool
  val is_cst : 'a Env.t -> 'a t -> bool
  val is_eq : 'a Env.t -> 'a t -> 'a t -> bool
  val is_leq : 'a Env.t -> 'a t -> 'a t -> bool
  val is_and_false : 'a Env.t -> 'a t -> 'a t -> bool

  val exist : 'a Env.t -> string list -> 'a t -> 'a t
  val forall : 'a Env.t -> string list -> 'a t -> 'a t

  val cofactor : 'a Env.t -> 'a t -> 'a t -> 'a t
  val restrict : 'a Env.t -> 'a t -> 'a t -> 'a t
  val tdrestrict : 'a Env.t -> 'a t -> 'a t -> 'a t
  val permute : 'a t -> int array -> 'a t

  val substitute_by_var : 'a Env.t -> 'a t -> (string * string) list -> 'a t
  val substitute : 'a Env.t -> 'a t -> (string * 'a expr) list -> 'a t

  val print : 'a Env.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint : sig
  type 'a t = 'a Int.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr

  val of_int : 'a Env.t -> [`Tbint of bool * int] -> int -> 'a t
  val var : 'a Env.t -> string -> 'a t
  val ite : 'a Env.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t

  val neg : 'a Env.t -> 'a t -> 'a t
  val succ : 'a Env.t -> 'a t -> 'a t
  val pred : 'a Env.t -> 'a t -> 'a t
  val add : 'a Env.t -> 'a t -> 'a t -> 'a t
  val sub : 'a Env.t -> 'a t -> 'a t -> 'a t
  val mul : 'a Env.t -> 'a t -> 'a t -> 'a t
  val shift_left : 'a Env.t -> int -> 'a t -> 'a t
  val shift_right : 'a Env.t -> int -> 'a t -> 'a t
  val scale : 'a Env.t -> int -> 'a t -> 'a t
  val zero : 'a Env.t -> 'a t -> 'a Bool.t
  val eq : 'a Env.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_int : 'a Env.t -> 'a t -> int -> 'a Bool.t
  val supeq : 'a Env.t -> 'a t -> 'a t -> 'a Bool.t
  val supeq_int : 'a Env.t -> 'a t -> int -> 'a Bool.t
  val sup : 'a Env.t -> 'a t -> 'a t -> 'a Bool.t

  val cofactor : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val permute : 'a t -> int array -> 'a t

  val substitute_by_var : 'a Env.t -> 'a t -> (string * string) list -> 'a t
  val substitute : 'a Env.t -> 'a t -> (string * 'a expr) list -> 'a t

  val guard_of_int: 'a Env.t -> 'a t -> int -> 'a Bool.t
    (** Return the guard of the integer value. *)
  val guardints: 'a Env.t -> 'a t -> ('a Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : 'a Env.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum : sig
  type 'a t = 'a Enum.t
  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val var : 'a Env.t -> string -> 'a t
  val ite : 'a Env.t -> 'a Bool.t -> 'a t -> 'a t -> 'a t
  val eq : 'a Env.t -> 'a t -> 'a t -> 'a Bool.t
  val eq_label : 'a Env.t -> 'a t -> string -> 'a Bool.t
  val cofactor : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val restrict : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val tdrestrict : 'a Env.t -> 'a t -> 'a Bool.t -> 'a t
  val permute : 'a t -> int array -> 'a t
  val substitute_by_var : 'a Env.t -> 'a t -> (string * string) list -> 'a t
  val substitute : 'a Env.t -> 'a t -> (string * 'a expr) list -> 'a t
  val guard_of_label : 'a Env.t -> 'a t -> string -> 'a Bool.t
    (** Return the guard of the label. *)
  val guardlabels : 'a Env.t -> 'a t -> ('a Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)
  val print : 'a Env.t -> Format.formatter -> 'a t -> unit
end

(*  ====================================================================== *)
(** {3 General (typed) expressions} *)
(*  ====================================================================== *)

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr : 'a t -> Env.typ
  (** Type of an expression *)

val var : 'a Env.t -> string -> 'a t
  (** Expression representing the litteral var *)

val ite : 'a Bool.t -> 'a t -> 'a t -> 'a t
  (** If-then-else operation *)

val eq : 'a Env.t -> 'a t -> 'a t -> 'a Bool.t
  (** Equality operation *)

val substitute_by_var : 'a Env.t -> 'a t -> (string * string) list -> 'a t
    (** Variable renaming.
      The new variables should already have been declared *)

val substitute : 'a Env.t -> 'a t -> (string * 'a t) list -> 'a t
    (** Parallel substitution of variables by expressions *)

val cofactor : 'a t -> 'a Cudd.Bdd.t -> 'a t
    (** Evaluate the expression. The BDD is assumed to be a cube *)

val restrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
val tdrestrict : 'a t -> 'a Cudd.Bdd.t -> 'a t
    (** Simplify the expression knowing that the BDD is true.  Generalizes
      [cofactor]. *)

val support : 'a Env.t -> 'a t -> string PSette.t
    (** Support of the expression *)

val support_cond : 'a Env.t -> 'a t -> 'a Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

val vectorsupport_cond : 'a Env.t -> 'a t array -> 'a Cudd.Bdd.t
    (** Return the support of an array of expressions as a conjunction of the
      BDD identifiers involved in the expressions *)

(*  ====================================================================== *)
(** {3 Miscellaneous} *)
(*  ====================================================================== *)

val cube_of_bdd : 'a Env.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
      (** Same as [Cudd.Bdd.cube_of_bdd], but keep only the
	the values of variables having a determinated value.

	Example: the classical [Cudd.Bdd.cube_of_bdd] could return
	[b and (x=1 or x=3)], whereas [cube_of_bdd] will return only [b] in
	such a case. *)

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

val print : 'a Env.t -> Format.formatter -> 'a t -> unit
  (** Print an expression *)

val print_minterm : 'a Env.t -> Format.formatter -> Cudd.Man.tbool array -> unit
  (** Print a minterm *)
val print_bdd : 'a Env.t -> Format.formatter -> 'a Cudd.Bdd.t -> unit
  (** Print a BDD *)
val print_idcondb : 'a Env.t -> Format.formatter -> int*bool -> unit
  (** Print the condition represented by the signed BDD index. *)
val print_idcond : 'a Env.t -> Format.formatter -> int -> unit
  (** Print the condition *)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

val tid_of_var : ('a,'b,'c,'d) #Env.O.t -> string -> int array
val reg_of_expr : 'd expr -> 'd Cudd.Bdd.t array
(*  ====================================================================== *)
(** {3 Expressions} *)
(*  ====================================================================== *)

(*  ---------------------------------------------------------------------- *)
(** {4 Boolean expressions} *)
(*  ---------------------------------------------------------------------- *)

module Bool : sig
  type 'd t = 'd Cudd.Bdd.t
  val of_expr : [>`Bool of 'd t] -> 'd t
  val to_expr : 'd t -> [>`Bool of 'd t]

  val dtrue : ('a,'b,'c,'d) #Env.O.t -> 'd t
  val dfalse : ('a,'b,'c,'d) #Env.O.t -> 'd t
  val of_bool : ('a,'b,'c,'d) #Env.O.t -> bool -> 'd t
  val var : ('a,'b,'c,'d) #Env.O.t -> string -> 'd t

  val dnot : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t
  val dand : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val dor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
    (** [not], [and] and [or] (use of 'd' prefix because of conflict with OCaml
      keywords) *)

  val xor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val nand : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val nor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val nxor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
    (** Exclusive or, not and, nor or and not xor *)

  val leq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
    (** Implication *)
  val eq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
    (** Same as [nxor] *)

  val ite : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t -> 'd t
    (** If-then-else *)

  val is_true : ('a,'b,'c,'d) #Env.O.t -> 'd t -> bool
  val is_false : ('a,'b,'c,'d) #Env.O.t -> 'd t -> bool
  val is_cst : ('a,'b,'c,'d) #Env.O.t -> 'd t -> bool
  val is_eq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> bool
  val is_leq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> bool
  val is_and_false : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> bool

  val exist : ('a,'b,'c,'d) #Env.O.t -> string list -> 'd t -> 'd t
  val forall : ('a,'b,'c,'d) #Env.O.t -> string list -> 'd t -> 'd t

  val cofactor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val restrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val tdrestrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val permute : 'd t -> int array -> 'd t

  val substitute_by_var : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * string) list -> 'd t
  val substitute : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * 'd expr) list -> 'd t

  val print : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'd t -> unit
end

(*  ---------------------------------------------------------------------- *)
(** {4 Bounded integer expressions} *)
(*  ---------------------------------------------------------------------- *)

module Bint : sig
  type 'd t = 'd Int.t
  val of_expr : [> `Bint of 'd t] -> 'd t
  val to_expr : 'd t -> [> `Bint of 'd t]

  val of_int : ('a,'b,'c,'d) #Env.O.t -> [> `Tbint of bool * int ] -> int -> 'd t
  val var : ('a,'b,'c,'d) #Env.O.t -> string -> 'd t
  val ite : ('a,'b,'c,'d) #Env.O.t -> 'd Bool.t -> 'd t -> 'd t -> 'd t

  val neg : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t
  val succ : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t
  val pred : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t
  val add : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val sub : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val mul : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
  val shift_left : ('a,'b,'c,'d) #Env.O.t -> int -> 'd t -> 'd t
  val shift_right : ('a,'b,'c,'d) #Env.O.t -> int -> 'd t -> 'd t
  val scale : ('a,'b,'c,'d) #Env.O.t -> int -> 'd t -> 'd t
  val zero : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t
  val eq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd Bool.t
  val eq_int : ('a,'b,'c,'d) #Env.O.t -> 'd t -> int -> 'd Bool.t
  val supeq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd Bool.t
  val supeq_int : ('a,'b,'c,'d) #Env.O.t -> 'd t -> int -> 'd Bool.t
  val sup : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd Bool.t

  val cofactor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val restrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val tdrestrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val permute : 'd t -> int array -> 'd t

  val substitute_by_var : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * string) list -> 'd t
  val substitute : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * 'd expr) list -> 'd t

  val guard_of_int: ('a,'b,'c,'d) #Env.O.t -> 'd t -> int -> 'd Bool.t
    (** Return the guard of the integer value. *)
  val guardints: ('a,'b,'c,'d) #Env.O.t -> 'd t -> ('d Bool.t * int) list
    (** Return the list [g -> n] of guarded values. *)

  val print : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'd t -> unit
end

(*  ---------------------------------------------------------------------- *)
(** {4 Enumerated expressions} *)
(*  ---------------------------------------------------------------------- *)

module Benum : sig
  type 'd t = 'd Enum.t
  val of_expr : [> `Benum of 'd t] -> 'd t
  val to_expr : 'd t -> [> `Benum of 'd t]
  val var : ('a,'b,'c,'d) #Env.O.t -> string -> 'd t
  val ite : ('a,'b,'c,'d) #Env.O.t -> 'd Bool.t -> 'd t -> 'd t -> 'd t
  val eq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd Bool.t
  val eq_label : ('a,'b,'c,'d) #Env.O.t -> 'd t -> string -> 'd Bool.t
  val cofactor : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val restrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val tdrestrict : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Bool.t -> 'd t
  val permute : 'd t -> int array -> 'd t
  val substitute_by_var : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * string) list -> 'd t
  val substitute : ('a,'b,'c,'d) #Env.O.t -> 'd t -> (string * 'd expr) list -> 'd t
  val guard_of_label : ('a,'b,'c,'d) #Env.O.t -> 'd t -> string -> 'd Bool.t
    (** Return the guard of the label. *)
  val guardlabels : ('a,'b,'c,'d) #Env.O.t -> 'd t -> ('d Bool.t * string) list
    (** Return the list [g -> label] of guarded values. *)
  val print : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'd t -> unit
end

(*  ---------------------------------------------------------------------- *)
(** {4 General (typed) expressions} *)
(*  ---------------------------------------------------------------------- *)

(** The following operations raise a [Failure] exception in case of a typing
  error. *)

val typ_of_expr : 'd expr -> [>Env.typ]
  (** Type of an expression *)

val var : ('a,'b,'c,'d) #Env.O.t -> string -> 'd expr
  (** Expression representing the litteral var *)

val ite : 'd Bool.t -> 'd expr -> 'd expr -> 'd expr
  (** If-then-else operation *)

val eq : ('a,'b,'c,'d) #Env.O.t -> 'd expr -> 'd expr -> 'd Bool.t
  (** Equality operation *)

val substitute_by_var : ('a,'b,'c,'d) #Env.O.t -> 'd expr -> (string * string) list -> 'd expr
    (** Variable renaming.
      The new variables should already have been declared *)

val substitute : ('a,'b,'c,'d) #Env.O.t -> 'd expr -> (string * 'd expr) list -> 'd expr
    (** Parallel substitution of variables by expressions *)

val support : ('a,'b,'c,'d) #Env.O.t -> 'd expr -> string PSette.t
    (** Support of the expression *)

val support_cond : ('a,'b,'c,'d) #Env.O.t -> 'd expr -> 'd Cudd.Bdd.t
    (** Return the support of an expression as a conjunction of the BDD
      identifiers involved in the expression *)

val vectorsupport_cond : ('a,'b,'c,'d) #Env.O.t -> 'd expr array -> 'd Cudd.Bdd.t
    (** Return the support of an array of expressions as a conjunction of the
      BDD identifiers involved in the expressions *)

(*  ---------------------------------------------------------------------- *)
(** {4 Miscellaneous} *)
(*  ---------------------------------------------------------------------- *)

val cube_of_bdd : ('a,'b,'c,'d) #Env.O.t -> 'd Cudd.Bdd.t -> 'd Cudd.Bdd.t
      (** Same as [Cudd.Bdd.cube_of_bdd], but keep only the
	the values of variables having a determinated value.

	Example: the classical [Cudd.Bdd.cube_of_bdd] could return
	[b and (x=1 or x=3)], whereas [cube_of_bdd] will return only [b] in
	such a case. *)

val tbdd_of_texpr : 'd expr array -> 'd Cudd.Bdd.t array
  (** Concatenates in an array the BDDs involved in the expressions *)
val texpr_of_tbdd : 'd expr array -> 'd Cudd.Bdd.t array -> 'd expr array
  (** Inverse operation: rebuild an array of expressions from the old array of
    expressions (for the types) and the array of BDDs. *)


(*  ====================================================================== *)
(** {3 Printing} *)
(*  ====================================================================== *)

val print : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> [<'d expr] -> unit
  (** Print an expression *)

val print_minterm : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> Cudd.Man.tbool array -> unit
  (** Print a minterm *)
val print_bdd : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'd Cudd.Bdd.t -> unit
  (** Print a BDD *)

val print_idcondb : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> int*bool -> unit
  (** Print the condition represented by the signed BDD index. *)
val print_idcond : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> int -> unit
  (** Print the condition *)

(*  ====================================================================== *)
(** {3 Internal functions} *)
(*  ====================================================================== *)

(*  ---------------------------------------------------------------------- *)
(** {4 Permutation and composition} *)
(*  ---------------------------------------------------------------------- *)

val permutation_of_rename :
  ('a,'b,'c,'d) #Env.O.t -> (string * string) list -> int array
val composition_of_substitution :
  ('a,'b,'c,'d) #Env.O.t -> (string * 'd expr) list -> 'd Cudd.Bdd.t array
val bddsupport : ('a,'b,'c,'d) #Env.O.t -> string list -> 'd Cudd.Bdd.t

val permute : 'd expr -> int array -> 'd expr
val compose : 'd expr -> 'd Cudd.Bdd.t array -> 'd expr

(*  ====================================================================== *)
(** {3 Conversion to expressions} *)
(*  ====================================================================== *)

module Expr : sig
  (** Syntax tree for printing *)

  (** Atom *)
  type 'a atom =
    | Tbool of string * bool
	(** variable name and sign *)
    | Tint of string * int list
	(** variable name and list of possible values *)
    | Tenum of string * string list
	(** variable name, possibly primed, and list of possible labels *)
    | Tcond of 'a
	(** condition *)

  (** Basic term *)
  type 'a term =
    | Tatom of 'a atom
	(** *)
    | Texternal of int * bool
	(** Unregistered BDD identifier and a Boolean for possible negation *)
    | Tcst of bool
	(** Boolean constant *)

  (** Conjunction *)
  type 'a conjunction =
    | Conjunction of 'a term list
       (** Conjunction of terms. Empty list means true. *)
    | Cfalse

  (** Disjunction *)
  type 'a disjunction =
    | Disjunction of 'a conjunction list
      (** Disjunction of conjunctions. Empty list means false *)
    | Dtrue
  val term_of_vint : string -> 'd Int.t -> Reg.Minterm.t -> 'a term

  val term_of_venum :
    ('a,'b,'c,'d) #Env.O.t ->
    string -> 'd Enum.t -> Reg.Minterm.t -> 'c term
  val term_of_idcondb :
    ('a,'b,'c,'d) #Env.O.t -> int * bool -> 'c term
  val bool_of_tbool : Cudd.Man.tbool -> bool
  val mand : 'c term list ref -> 'c term -> unit
  val conjunction_of_minterm :
    ('a,'b,'c,'d) #Env.O.t -> Cudd.Man.tbool array -> 'c conjunction
  val disjunction_of_bdd :
    ('a,'b,'c,'d) #Env.O.t -> 'd Cudd.Bdd.t -> 'c disjunction
  val print_term :
    ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'c term -> unit
  val print_conjunction :
    ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'c conjunction -> unit
  val print_disjunction :
    ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'c disjunction -> unit

end
end
