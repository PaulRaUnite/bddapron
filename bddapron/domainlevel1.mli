(** Functor to transform an abstract domain interface from level 0
    to level 1 (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

module type Level0 = sig
  type 'a man
    (** BDDAPRON Manager. The type parameter ['a] indicates the
	underlying APRON abstract domain, as in type {!'a
	Apron.Abstract0.t}. *)
  type 'a t
    (** BDDAPRON Abstract value. The type parameter ['a] indicates
	the underlying APRON abstract domain, as in type {!'a
	Apron.Abstract0.t}. *)
    
  val size : 'a man -> 'a t -> int
    (** Size of an abstract value in terms of number of nodes of the MTBDD. *)
  val print : Env.t -> Format.formatter -> 'c t -> unit
    (** Printing function *)

  val bottom : 'a man -> Env.t -> 'a t
  val top : 'a man -> Env.t -> 'a t
  val of_apron : 'a man -> Env.t -> 'a Apron.Abstract0.t -> 'a t

  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val to_bddapron : 
    'a man -> 'a t -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val meet_condition : 
    'a man -> Env.t -> Cond.t -> 'a t -> Expr0.Bool.t -> 'a t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    'a man -> Env.t -> Cond.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val substitute_lexpr :
    'a man -> Env.t -> Cond.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val forget_list : 'a man -> Env.t -> 'a t -> string list -> 'a t
  val widening : 'a man -> 'a t -> 'a t -> 'a t
  val apply_change :
    bottom:'a t -> 'a man -> 'a t -> Env.change -> 'a t
  val apply_permutation :
    'a man -> 'a t -> int array option * Apron.Dim.perm option -> 'a t
end

(*  ********************************************************************** *)
(** {2 Abstract domain of level 1} *)
(*  ********************************************************************** *)

module type Level1 = sig
  type 'a man
    (** BDDAPRON Manager. The type parameter ['a] indicates the
	underlying APRON abstract domain, as in type {!'a
	Apron.Abstract0.t}. *)
  type 'a t0
    (** Level 0 abstract value. *)
  type 'a t = (Env.t, 'a t0) Env.value
    (** Level 1 abstract value *)
    
  val size : 'a man -> 'a t -> int
    (** Size of an abstract value. *)
  val print : Format.formatter -> 'c t -> unit
    (** Printing function *)

  (** {3 Basic constructor} *)
  val bottom : 'a man -> Env.t -> 'a t
  val top : 'a man -> Env.t -> 'a t
  val of_apron : 'a man -> Env.t -> 'a Apron.Abstract1.t -> 'a t

  (** {3 Tests} *)

  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
    (** Emtpiness and Universality tests *)

  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
    (** Inclusion and equality tests *)

  (** {3 Extraction of properties} *)
  val to_bddapron : 'a man -> 'a t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
    (** Conversion to a disjunction of a conjunction of pair of a
        purely Boolean formula (without numerical constraints) and
        an APRON abstract value *)

  (** {3 Operations} *)

  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
    (** Meet and join *)

  val meet_condition : 'a man -> Cond.t -> 'a t -> Expr1.Bool.t -> 'a t
  val meet_condition2 : 'a man -> 'a t -> Expr2.Bool.t -> 'a t
    (** Intersection with a Boolean expression (that may involve
        numerical constraints) *)
    
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
    (** Parallel assignement/substitution of a list of variables
        by a list of expressions *)

  val forget_list : 'a man -> 'a t -> string list -> 'a t
    (** Forget (existential quantification) a list of variables *)
  val widening : 'a man -> 'a t -> 'a t -> 'a t
    (** Widening *)

  (** {3 Change of environments and renaming} *)
  val change_environment : 'a man -> 'a t -> Env.t -> 'a t
    (** Change the environment (eliminate (forget) variables not
        belonging to the new environment, and introduce new
        variables) *)
  val rename : 'a man -> 'a t -> (string*string) list -> 'a t
    (** Rename a list of variables (thus changing the
        environment). *)
  val unify : 'a man -> 'a t -> 'a t -> 'a t
    (** Unify two abstract values on their least common
        environment (lce, that should exist, which implies that no
        variable is defined with different types in the two
        initial environments).

	This is equivalent to change the environment to the lce, and
	to perform meet.  *)
end

module Make(Level0:Level0) : 
  (Level1 with type 'a man = 'a Level0.man
	  and type 'a t0 = 'a Level0.t)
