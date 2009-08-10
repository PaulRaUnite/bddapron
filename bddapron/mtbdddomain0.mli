(** Boolean/Numerical domain, with MTBDDs over APRON values *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type 'a man =
  'a ApronDD.man = {
  apron : 'a Apron.Manager.t;
  table : 'a ApronDD.table;
  oglobal : 'a ApronDD.global option;
}
  (** BDDAPRON Manager. The type parameter ['a] indicates the
      underlying APRON abstract domain, as in type {!'a
      Apron.Abstract1.t} *)

type 'a t = 'a ApronDD.t
  (** BDDAPRON Abstract value. *)

val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man
  (** Makes a BDDAPRON manager from an APRON manager.
      If [global=true] (default: [false]), uses a global (persistent)
      BDD cache for the operations [is_leq], [join], [meet]
      and [exist] (internal).
  *)

val size : 'a man -> 'a t -> int
  (** Size of an abstract value in terms of number of nodes of the MTBDD. *)
val print : Env.t -> Format.formatter -> 'c t -> unit
  (** Printing function *)

(*  ********************************************************************** *)
(** {2 Constructors, accessors, tests and property extraction} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Basic constructor} *)
(*  ====================================================================== *)

val bottom : 'a man -> Env.t -> 'a t
val top : 'a man -> Env.t -> 'a t
val of_apron : 'a man -> Env.t -> 'a Apron.Abstract0.t -> 'a t

(*  ====================================================================== *)
(** {3 Tests} *)
(*  ====================================================================== *)

val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
  (** Emtpiness and Universality tests *)

val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
  (** Inclusion and equality tests *)

(*  ====================================================================== *)
(** {3 Extraction of properties} *)
(*  ====================================================================== *)

val to_bddapron : 'a man -> 'a t -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
  (** Conversion to a disjunction of a conjunction of pair of a
      purely Boolean formula (without numerical constraints) and an
      APRON abstract value *)

(*  ********************************************************************** *)
(** {2 Operations} *)
(*  ********************************************************************** *)

val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
  (** Meet and join *)

val meet_condition : 'a man -> Env.t -> Cond.t -> 'a t -> Expr0.Bool.t -> 'a t
  (** Intersection with a Boolean expression (that may involve
      numerical constraints) *)

val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  'a man -> Env.t -> Cond.t ->
  'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
val substitute_lexpr :
  'a man -> Env.t -> Cond.t ->
  'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  (** Parallel assignement/substitution of a list of variables by
      a list of expressions *)

val forget_list :
  'a man -> Env.t -> 'a t -> string list -> 'a t
  (** Forget (existential quantification) a list of variables *)

val widening : 'a man -> 'a t -> 'a t -> 'a t
  (** Widening *)

val apply_change :
  bottom:'a t -> 'a man -> 'a t -> Env.change -> 'a t
val apply_permutation :
  'a man -> 'a t -> int array option * Apron.Dim.perm option -> 'a t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows extensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig
  val meet_idcondb :
    'a man -> 'b -> 'b Cond.O.t -> 'a t -> int * bool -> 'a t

  val size : 'a man -> 'a t -> int
  val print : ('b,'c,'d) Env.O.t -> Format.formatter -> 'a t -> unit
  val bottom : 'a man -> ('b,'c,'d) Env.O.t -> 'a t
  val top : 'a man -> ('b,'c,'d) Env.O.t -> 'a t
  val of_apron : 'a man -> ('b,'c,'d) Env.O.t -> 'a Apron.Abstract0.t -> 'a t
  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val to_bddapron : 'a man -> 'a t -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val meet_condition : 'a man -> 'b -> 'b Cond.O.t -> 'a t -> Expr0.Bool.t -> 'a t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    'a man -> 'b -> 'b Cond.O.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val substitute_lexpr :
    'a man -> 'b -> 'b Cond.O.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val forget_list :
    'a man -> ('b,'c,'d) Env.O.t -> 'a t -> string list -> 'a t
  val widening : 'a man -> 'a t -> 'a t -> 'a t
  val apply_change :
    bottom:'a t -> 'a man -> 'a t -> Env.change -> 'a t
  val apply_permutation :
    'a man -> 'a t -> int array option * Apron.Dim.perm option -> 'a t

end
