(** Boolean (abstract) domain *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr0.Bool.t
  (** Abstract value *)

val size : 'a t -> int
  (** Size of an abstract value (number of nodes) *)
val print :     
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    'a Env.t -> Format.formatter -> 'a t -> unit

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
val meet_condition : 'a Env.t -> 'a t -> 'a Expr0.Bool.t -> 'a t
  (** Lattice operations *)

val assign_lexpr :
  ?relational:bool ->
  ?nodependency:bool ->
  'a Env.t ->
  'a t -> string list -> 'a Expr0.expr list -> 'a t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [relational=true], it is assumed that [env#bddincr=2] (checked),
    starting from a pair index. It is also advised to have paired variables in
    groups.

    [rel=true] is most probably much better for assignements of a few
    variables. *)
val substitute_lexpr :
  'a Env.t ->
  'a t -> string list -> 'a Expr0.expr list -> 'a t
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

val print :     
    ?print_external_idcondb:(Format.formatter -> int * bool -> unit) ->
    ('a,'b,'c,'d) Env.O.t -> Format.formatter -> 'c t -> unit

val bottom :
  ('a,'b,'c,'d) Env.O.t -> 'c t
val top :
  ('a,'b,'c,'d) Env.O.t -> 'c t
  (** Constructors *)

val is_bottom : ('a,'b,'c,'d) Env.O.t -> 'c t -> bool
val is_top : ('a,'b,'c,'d) Env.O.t -> 'c t -> bool
val is_leq : ('a,'b,'c,'d) Env.O.t -> 'c t -> 'c t -> bool
val is_eq : ('a,'b,'c,'d) Env.O.t -> 'c t -> 'c t -> bool
val is_variable_unconstrained : ('a,'b,'c,'d) Env.O.t -> 'c t -> string -> bool
  (** Tests *)

val meet : ('a,'b,'c,'d) Env.O.t -> 'c t -> 'c t -> 'c t
val join : ('a,'b,'c,'d) Env.O.t -> 'c t -> 'c t -> 'c t
val meet_condition : ('a,'b,'c,'d) Env.O.t -> 'c t -> 'c Expr0.Bool.t -> 'c t
  (** Lattice operations *)

val assign_lexpr :
  ?relational:bool ->
  ?nodependency:bool -> 
  ('a,'b,'c,'d) Env.O.t ->
  'c t -> string list -> 'c Expr0.expr list -> 'c t
  (** Assignement

    If [nodependency=true], which means that no expression depends on the
    assigned variables, it uses an optimized algorithm.

    If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
    a pair index. It is also advised to have paired variables in groups.

    [rel=true] is most probably much better for assignements of a few
    variables.  *)
val substitute_lexpr :
  ('a,'b,'c,'d) Env.O.t ->
  'c t -> string list -> 'c Expr0.expr list -> 'c t
  (** Substitution *)

val forget_list : ('a,'b,'c,'d) Env.O.t -> 'c t -> string list -> 'c t
  (** Eliminating variables *)

module Asssub : sig
  val sort : int array -> 'a Cudd.Bdd.t array -> int array * 'a Cudd.Bdd.t array
  val is_equal : 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t array -> bool
  val post : 'a Cudd.Bdd.t -> int array -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
  val postcondition : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t array -> 'a Cudd.Bdd.t
end
val relation_supp_compose_of_lvarlexpr :
  ('a,'b,'c,'d) Env.O.t ->
  string list -> 'c Expr0.expr list ->
  'c Cudd.Bdd.t * 'c Cudd.Bdd.t * 'c Cudd.Bdd.t array

val apply_change : 'a t -> 'a Env.change -> 'a t
end
