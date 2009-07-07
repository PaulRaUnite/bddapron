
type 'a man = 'a Bdddomain0.man = {
  apron : 'a Apron.Manager.t;
  mutable bdd_restrict : Cudd.Bdd.vt -> Cudd.Bdd.vt -> Cudd.Bdd.vt;
  mutable expr_restrict : Expr0.t -> Cudd.Bdd.vt -> Expr0.t;
  mutable meet_disjoint : bool;
  mutable join_disjoint : bool;
  mutable meet_cond_unique : bool;
  mutable meet_cond_disjoint : bool;
  mutable meet_cond_depth : int;
  mutable assign_unique : bool;
  mutable assign_disjoint : bool;
  mutable substitute_unique : bool;
  mutable substitute_disjoint : bool;
  mutable forget_unique : bool;
  mutable forget_disjoint : bool; 
  mutable change_environment_unique : bool;
  mutable change_environment_disjoint : bool;
}
  (** BDDAPRON Manager. The type parameter ['a] indicates the
      underlying APRON abstract domain, as in type {!'a
      Apron.Abstract1.t} *)

type 'a t = (Env.t, 'a Bdddomain0.t) Env.value
  (** Abstract value. *)

val make_man : 'a Apron.Manager.t -> 'a man
  (** Makes a BDDAPRON manager from an APRON manager, and fills
  options with default values *)

val canonicalize : ?apron:bool -> ?unique:bool -> ?disjoint:bool -> 'a man -> 'a t -> unit
  (** Canonicalize an abstract value by ensuring uniqueness and
      disjointness properties. If [apron] is true, then also
      normalize APRON abstract values. By default: [apron=false,
      unique=disjoint=true]. *)

val size : 'a man -> 'a t -> int
val print : Format.formatter -> 'c t -> unit
  (** Printing function *)

(*  ********************************************************************** *)
(** {2 Constructors, accessors, tests and property extraction} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Basic constructor} *)
(*  ====================================================================== *)

val bottom : 'a man -> Env.t -> 'a t
val top : 'a man -> Env.t -> 'a t
val of_apron : 'a man -> Env.t -> 'a Apron.Abstract1.t -> 'a t

(*  ====================================================================== *)
(** {3 Tests} *)
(*  ====================================================================== *)

val is_bottom : 'a -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool

(*  ====================================================================== *)
(** {3 Extraction of properties} *)
(*  ====================================================================== *)

val to_bddapron : 'a man -> 'a t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list

(*  ********************************************************************** *)
(** {2 Operations} *)
(*  ********************************************************************** *)

val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet_condition : 'a man -> Cond.t -> 'a t -> Expr1.Bool.t -> 'a t
val meet_condition2 : 'a man -> 'a t -> Expr2.Bool.t -> 'a t
 
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  'a man -> Cond.t ->
  'a t -> string list -> Expr1.t list -> 'a t option -> 'a t
val assign_listexpr2 :
  ?relational:bool -> ?nodependency:bool ->
  'a man ->
  'a t -> string list -> Expr2.List.t -> 'a t option ->
  'a t
val substitute_lexpr :
  'a man -> Cond.t ->
  'a t -> string list -> Expr1.t list -> 'a t option -> 'a t
val substitute_listexpr2 :
  'a man ->
  'a t -> string list -> Expr2.List.t -> 'a t option -> 'a t
  (** Parallel assignement/substitution of a list of variables by
      a list of expressions *)
val forget_list :
  'a man -> 'a t -> string list -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t

(*  ====================================================================== *)
(** {3 Change of environments and renaming} *)
(*  ====================================================================== *)

val change_environment : 'a man -> 'a t -> Env.t -> 'a t
  (** Change the environement (eliminates (forget) variables not
      belonging to the new environment, and introduces new variables)
  *)

val rename : 'a man -> 'a t -> (string*string) list -> 'a t
  (** Rename a list of variables (thus changing the
      environment). *)

val unify : 'a man -> 'a t -> 'a t -> 'a t
  (** Unifies two abstract values on their least common
      environment (lce, that should exist, which implies that no variable
      is defined with different types in the two initial environments).

      This is equivalent to change the environment to the lce, and
      to perform meet.  *)
