(** Boolean (abstract) domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Environments} *)
(*  ********************************************************************** *)

val make_env : ?boolfirst:bool -> ?relational:bool -> 'a Cudd.Man.t -> 'a Env.t
    (** Create an environment with a CUDD manager *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr0.Bool.t
  (** Abstract value *)

type 'a expr = 'a Expr0.expr
  (** Finite-type expression *)

val size : 'a t -> int
  (** Size of an abstract value (number of nodes) *)
val print : 'a Env.t -> Format.formatter -> 'a t -> unit

val bottom : 'a Env.t -> 'a t
val top : 'a Env.t -> 'a t
  (** Constructors *)

val is_bottom : 'a Env.t -> 'a t -> bool
val is_top : 'a Env.t -> 'a t -> bool
val is_leq : 'a Env.t -> 'a t -> 'a t -> bool
val is_eq : 'a Env.t -> 'a t -> 'a t -> bool
val is_variable_unconstrained : 'a Env.t -> 'a t -> string -> bool
  (** Tests *)

val meet : 'a Env.t -> 'a t -> 'a t -> 'a t
val join : 'a Env.t -> 'a t -> 'a t -> 'a t
val meet_cond : 'a Env.t -> 'a t -> 'a Expr0.Bool.t -> 'a t
  (** Lattice operations *)

val assign_list :
  ?relational:bool ->
  ?nodependency:bool ->
  'a Env.t ->
  'a t -> (string * 'a Expr0.expr) list -> 'a t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [relational=true], it is assumed that [Env.t#bddincr=2] (checked),
    starting from a pair index. It is also advised to have paired variables in
    groups.

    [rel=true] is most probably much better for assignements of a few
    variables. *)
val substitute_list :
  'a Env.t ->
  'a t -> (string * 'a Expr0.expr) list -> 'a t
  (** Substitution *)

val forget_list : 'a Env.t -> 'a t -> string list -> 'a t
  (** Eliminating variables *)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

val print : ('a,'b,'c,'d) #Env.O.t -> Format.formatter -> 'd t -> unit

val bottom :
  ('a,'b,'c,'d) #Env.O.t -> 'd t
val top :
  ('a,'b,'c,'d) #Env.O.t -> 'd t
  (** Constructors *)

val is_bottom : ('a,'b,'c,'d) #Env.O.t -> 'd t -> bool
val is_top : ('a,'b,'c,'d) #Env.O.t -> 'd t -> bool
val is_leq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> bool
val is_eq : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> bool
val is_variable_unconstrained : ('a,'b,'c,'d) #Env.O.t -> 'd t -> string -> bool
  (** Tests *)

val meet : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
val join : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd t -> 'd t
val meet_cond : ('a,'b,'c,'d) #Env.O.t -> 'd t -> 'd Expr0.Bool.t -> 'd t
  (** Lattice operations *)

val assign_list :
  ?relational:bool ->
  ?nodependency:bool -> 
  ('a,'b,'c,'d) #Env.O.t ->
  'd t -> (string * 'd Expr0.expr) list -> 'd t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [rel=true], it is assumed that [Env.t#bddincr=2] (checked), starting from
    a pair index. It is also advised to have paired variables in groups.

    [rel=true] is most probably much better for assignements of a few
    variables.  *)
val substitute_list :
  ('a,'b,'c,'d) #Env.O.t ->
  'd t -> (string * 'd Expr0.expr) list -> 'd t
  (** Substitution *)

val forget_list : ('a,'b,'c,'d) #Env.O.t -> 'd t -> string list -> 'd t
  (** Eliminating variables *)

module Asssub : sig
  val sort : int array -> 'a Cudd.Bdd.t array -> int array * 'a Cudd.Bdd.t array
  val is_equal : 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t array -> bool
  val post : 'a Cudd.Bdd.t -> int array -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
  val postcondition : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
end
val relation_supp_compose_of_lvarexpr :
  ('a,'b,'c,'d) #Env.O.t ->
  (string * ('d Expr0.expr)) list ->
  'd Cudd.Bdd.t * 'd Cudd.Bdd.t * 'd Cudd.Bdd.t array

end
