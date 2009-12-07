(** Combined Boolean/Numerical domain, with lists of BDDs and APRON values *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type ('a,'b) man = {
  apron : 'b Apron.Manager.t;
  mutable bdd_restrict : Cudd.Bdd.vt -> Cudd.Bdd.vt -> Cudd.Bdd.vt;
  mutable expr_restrict : 'a Expr0.t -> Cudd.Bdd.vt -> 'a Expr0.t;
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
  (** BDDAPRON Manager. The type parameter ['b] indicates the
      underlying APRON abstract domain, as in type {!'b
      Apron.Abstract1.t}, and ['a] is the type of symbols. *)

type 'b elt = (Cudd.Man.v, 'b Apron.Abstract0.t) Bddleaf.elt

type 'b t = {
  mutable list : 'b elt list;
  bottom : 'b elt;
  mutable unique : bool;
  mutable disjoint : bool;
}
  (** Abstract value. *)

val make_man : 'b Apron.Manager.t -> ('a,'b) man
  (** Makes a BDDAPRON manager from an APRON manager, and fills
      options with default values *)

val canonicalize : ?apron:bool -> ?unique:bool -> ?disjoint:bool -> ('a,'b) man -> 'b t -> unit
  (** Canonicalize an abstract value by ensuring uniqueness and
      disjointness properties. If [apron] is true, then also
      normalize APRON abstract values. By default: [apron=false,
      unique=disjoint=true]. *)

val size : ('a,'b) man -> 'b t -> int
  (** Size of an abstract value in terms of number of nodes of the MTBDD. *)
val print : 'a Env.t -> Format.formatter -> 'b t -> unit
  (** Printing function *)

(*  ********************************************************************** *)
(** {2 Constructors, accessors, tests and property extraction} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Basic constructor} *)
(*  ====================================================================== *)

val bottom : ('a,'b) man -> 'a Env.t -> 'b t
val top : ('a,'b) man -> 'a Env.t -> 'b t
val of_apron : ('a,'b) man -> 'a Env.t -> 'b Apron.Abstract0.t -> 'b t

(*  ====================================================================== *)
(** {3 Tests} *)
(*  ====================================================================== *)

val is_bottom : ('a,'b) man -> 'b t -> bool
val is_top : ('a,'b) man -> 'b t -> bool
  (** Emtpiness and Universality tests *)

val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  (** Inclusion and equality tests *)

(*  ====================================================================== *)
(** {3 Extraction of properties} *)
(*  ====================================================================== *)

val to_bddapron : ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  (** Conversion to a disjunction of a conjunction of pair of a
      purely Boolean formula (without numerical constraints) and an
      APRON abstract value *)

(*  ********************************************************************** *)
(** {2 Operations} *)
(*  ********************************************************************** *)

val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  (** Meet and join *)

val meet_condition : ('a,'b) man -> 'a Env.t -> 'a Cond.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  (** Intersection with a Boolean expression (that may involve
      numerical constraints) *)

val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
  'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
val substitute_lexpr :
  ('a,'b) man -> 'a Env.t -> 'a Cond.t ->
  'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  (** Parallel assignement/substitution of a list of variables by
      a list of expressions *)

val forget_list :
  ('a,'b) man -> 'a Env.t -> 'b t -> 'a list -> 'b t
  (** Forget (existential quantification) a list of variables *)

val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
  (** Widening *)

val apply_change :
  bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
val apply_permutation :
  ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O : sig
  val check_wellformed : ('a,'b) man -> 'b t -> bool
  val canonicalize : ?apron:bool -> ?unique:bool -> ?disjoint:bool -> ('a,'b) man -> 'b t -> unit

  val size : ('a,'b) man -> 'b t -> int
  val print : ('a,'c,'d,'e) Env.O.t -> Format.formatter -> 'b t -> unit
  val bottom : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t
  val top : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t
  val of_apron : ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b Apron.Abstract0.t -> 'b t
  val is_bottom : ('a,'b) man -> 'b t -> bool
  val is_top : ('a,'b) man -> 'b t -> bool
  val is_leq : ('a,'b) man -> 'b t -> 'b t -> bool
  val is_eq : ('a,'b) man -> 'b t -> 'b t -> bool
  val to_bddapron : ('a,'b) man -> 'b t -> ('a Expr0.Bool.t * 'b Apron.Abstract0.t) list
  val meet : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val join : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val meet_condition : ('a,'b) man -> 'c -> ('a,'c) Cond.O.t -> 'b t -> 'a Expr0.Bool.t -> 'b t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    ('a,'b) man -> 'c -> ('a,'c) Cond.O.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t
  val substitute_lexpr :
    ('a,'b) man -> 'c -> ('a,'c) Cond.O.t ->
    'b t -> 'a list -> 'a Expr0.t list -> 'b t option -> 'b t

  val forget_list :
    ('a,'b) man -> ('a,'c,'d,'e) Env.O.t -> 'b t -> 'a list -> 'b t
  val widening : ('a,'b) man -> 'b t -> 'b t -> 'b t
  val apply_change :
    bottom:'b t -> ('a,'b) man -> 'b t -> Env.change -> 'b t
  val apply_permutation :
    ('a,'b) man -> 'b t -> int array option * Apron.Dim.perm option -> 'b t
end
