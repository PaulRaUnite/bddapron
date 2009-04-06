(** Boolean (abstract) domain with integrated environment *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Environments} *)
(*  ********************************************************************** *)

val make_env : ?boolfirst:bool -> ?relational:bool -> 'a Cudd.Man.t -> 'a Env.t
    (** Create an Env.tironment with a CUDD manager *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr1.Bool.t
  (** Abstract value *)

type 'a expr = 'a Expr1.expr
  (** Finite-type expression *)

val size : 'a t -> int
  (** Size of an abstract value (number of nodes) *)
val print : Format.formatter -> 'a t -> unit

val bottom : 'a Env.t -> 'a t
val top : 'a Env.t -> 'a t
  (** Constructors *)

val is_bottom : 'a t -> bool
val is_top : 'a t -> bool
val is_leq : 'a t -> 'a t -> bool
val is_eq : 'a t -> 'a t -> bool
val is_variable_unconstrained : 'a t -> string -> bool
  (** Tests *)

val meet : 'a t -> 'a t -> 'a t
val join : 'a t -> 'a t -> 'a t
val meet_cond : 'a t -> 'a Expr1.Bool.t -> 'a t
  (** Lattice operations *)

val assign_list : ?relational:bool -> ?nodependency:bool -> 'a t -> (string * 'a expr) list -> 'a t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [rel=true], it is assumed that [Env.t#bddincr=2] (checked), starting from
    a pair index. It is also advised to have paired variables in groups.

    [rel=true] is most probably much better for assignements of a few
    variables.  *)
val substitute_list : 'a t -> (string * 'a expr) list -> 'a t
  (** Substitution *)

val forget_list : 'a t -> string list -> 'a t
  (** Eliminating variables *)

val change_environment : 'a t -> 'a Env.t -> 'a t
val rename :'a t -> (string*string) list -> 'a t
  (** Change of environments *)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

type ('a,'b) t = ('a,'b) Expr1.O.Bool.t
type ('a,'b) expr = ('a,'b) Expr1.O.expr

val size : ('a,'b) t -> int
val print : Format.formatter -> ('a,'b) t -> unit

val bottom : 'a -> ('a,'b) t
val top : 'a -> ('a,'b) t
  (** Constructors *)

val is_bottom : ('a,'b) t -> bool
val is_top : ('a,'b) t -> bool
val is_leq : ('a,'b) t -> ('a,'b) t -> bool
val is_eq : ('a,'b) t -> ('a,'b) t -> bool
val is_variable_unconstrained : ('a,'b) t -> string -> bool
  (** Tests *)

val meet : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val join : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val meet_cond : ('a,'b) t -> ('a,'b) Expr1.O.Bool.t -> ('a,'b) t
  (** Lattice operations *)

val assign_list : ?relational:bool -> ?nodependency:bool -> ('a,'b) t -> (string * ('a,'b) expr) list -> ('a,'b) t
  (** Assignement

    If [rel=true], it is assumed that [Env.t#bddincr=2] (checked), starting from
    a pair index. It is also advised to have paired variables in groups.

    [rel=true] is most probably much better for assignements of a few
    variables.  *)
val substitute_list : ('a,'b) t -> (string * ('a,'b) expr) list -> ('a,'b) t
  (** Substitution *)

val forget_list : ('a,'b) t -> string list -> ('a,'b) t
  (** Eliminating variables *)

type 'a change_environment = {
  intro : int array option;
  remove : ('a Cudd.Bdd.t * int array) option;
}
val compute_change_environment :
  (('a,'b,'c,'d) #Env.O.t as 'e) ->
  'e -> 'd change_environment
val apply_change_environment :
  'a Cudd.Bdd.t -> 'a change_environment -> 'a Cudd.Bdd.t
val change_environment : ('a,'b) t -> 'a -> ('a,'b) t
val rename :('a,'b) t -> (string*string) list -> ('a,'b) t
  (** Change of environments *)

val check2 :
  (('a,'b,'c,'d) #Env.O.t as 'e, 'f) Env.value ->
  ('e, 'f) Env.value -> unit
val check_expr :
  ('e -> int array -> 'e) ->
  ('a,'b,'c,'d) #Env.O.t ->
  (('a,'b,'c,'d) #Env.O.t, 'e) Env.value ->
  'e
    (** Internal functions *)
end
