(** Combined Boolean/Numerical domain with environment *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a man = 'a Mtbdddomain0.man = {
  apron: 'a Apron.Manager.t;
  table : 'a ApronDD.table;
  oglobal : 'a ApronDD.global option;
}
  (** BDDAPRON Manager. The type parameter ['a] indicates the
      underlying APRON abstract domain, as in type {!'a
      Apron.Abstract0.t} *)

type 'a t = (Env.t, 'a Mtbdddomain0.t) Env.value
  (** BDDAPRON Abstract value. *)

val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man
  (** Makes a BDDAPRON manager from an APRON manager.
      If [global=true] (default: [false]), uses a global (persistent)
      BDD cache for the operations [is_leq], [join], [meet] 
      and [exist] (internal).
  *)

val size : 'a man -> 'a t -> int
  (** Size of an abstract value in terms of number of nodes of the MTBDD. *)
val print : Format.formatter -> 'a t -> unit
  (** Printing function *)


(*  ====================================================================== *)
(** {3 Constructors, accessors, tests and property extraction} *)
(*  ====================================================================== *)

(*  ---------------------------------------------------------------------- *)
(** {4 Basic constructor} *)
(*  ---------------------------------------------------------------------- *)

val bottom : 'a man -> Env.t -> 'a t
val top: 'a man -> Env.t -> 'a t
val of_apron : 'a man -> Env.t -> 'a Apron.Abstract1.t -> 'a t

(*  ---------------------------------------------------------------------- *)
(** {4 Tests} *)
(*  ---------------------------------------------------------------------- *)

val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
  (** Emtpiness and Universality tests *)

val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
  (** Inclusion and equality tests *)

(*  ---------------------------------------------------------------------- *)
(** {4 Extraction of properties} *)
(*  ---------------------------------------------------------------------- *)

val to_bddapron : 
  'a man -> 'a t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
  (** Conversion to a disjunction of a conjunction of pair of a
      purely Boolean formula (without numerical constraints) and an
      APRON abstract value *)

(*  ====================================================================== *)
(** {3 Operations} *)
(*  ====================================================================== *)

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
  'a t -> string list -> Expr1.t list -> 'a t option ->
  'a t
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
  (** Forget (existential quantification) a list of variables *)
val widening : 'a man -> 'a t -> 'a t -> 'a t
  (** Widening *)

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

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

  type ('a,'b) t = ('a,'b Mtbdddomain0.t) Bdd.Env.value
  constraint 'a = ('c,'d) #Env.O.t

  val size : 'b man -> ('a,'b) t -> int
  val print : Format.formatter -> ('a,'b) t -> unit
  val bottom : 'b man -> 'a -> ('a,'b) t
  val top : 'b man -> 'a -> ('a,'b) t
  val of_apron : 'b man -> 'a -> 'b Apron.Abstract1.t -> ('a,'b) t
  val is_bottom : 'b man -> ('a,'b) t -> bool
  val is_top : 'b man -> ('a,'b) t -> bool
  val is_leq : 'b man -> ('a,'b) t -> ('a,'b) t -> bool
  val is_eq : 'b man -> ('a,'b) t -> ('a,'b) t -> bool
  val to_bddapron : 'b man -> ('a,'b) t -> ('a Expr1.O.Bool.t * 'b Apron.Abstract1.t) list
  val meet : 'b man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val join : 'b man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t

  val meet_condition :
    'b man -> (Cond.cond,'a) #Cond.O.t ->
    ('a,'b) t -> 'a Expr1.O.Bool.t -> ('a,'b) t
  val meet_condition2 :
    'b man -> 
    ('a,'b) t -> ('c,'a) Expr2.O.Bool.t -> ('a,'b) t

  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    'b man -> (Cond.cond,'a) #Cond.O.t ->
    ('a,'b) t -> string list -> 'a Expr1.O.t list -> ('a,'b) t option ->
    ('a,'b) t
  val assign_listexpr2 :
    ?relational:bool -> ?nodependency:bool ->
    'b man ->
    ('a,'b) t -> string list -> ('c,'a) Expr2.O.List.t -> ('a,'b) t option ->
    ('a,'b) t
  val substitute_lexpr :
    'b man -> (Cond.cond,'a) #Cond.O.t ->
    ('a,'b) t -> string list -> 'a Expr1.O.t list -> ('a,'b) t option ->
    ('a,'b) t
  val substitute_listexpr2 :
    'b man ->
    ('a,'b) t -> string list -> ('c,'a) Expr2.O.List.t -> ('a,'b) t option ->
    ('a,'b) t

  val forget_list :
    'b man -> ('a,'b) t -> string list -> ('a,'b) t

  val change_environment : 'a man -> (Env.t,'a) t -> Env.t -> (Env.t,'a) t
  val rename :
    'b man -> ('a,'b) t -> (string*string) list -> ('a,'b) t
  val widening : 'b man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val unify : 'a man -> (Env.t,'a) t -> (Env.t,'a) t -> (Env.t,'a) t

end
