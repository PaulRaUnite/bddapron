(** Combined Boolean/Numerical domain with environment *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Environments} *)
(*  ********************************************************************** *)

type env = Bddapronexpr.env
val make_env : ?boolfirst:bool -> Manager.t -> env
val print_env : Format.formatter -> env -> unit

val add_typ : env -> string -> Bddapronexpr.typdef -> env
    (** Declaration of a new type *)
val add_vars : env -> (string * Bddapronexpr.typ) list -> env
    (** Add (typed) variables *)
val remove_vars : env -> string list -> env
    (** Remove variables *)
val rename_vars : env -> (string*string) list -> env
    (** Rename variables *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a manager = 'a Bddaprondomain.manager
type 'a t = (Bddapronexpr.env, 'a Bddaprondomain.t) Bddenv.value

val make_manager : 'a Apron.Manager.t -> 'a manager

val size : 'a manager -> 'a t -> int
val print : Format.formatter -> 'a t -> unit
val bottom : 'a manager -> env -> 'a t
val top: 'a manager -> env -> 'a t
val is_bottom : 'a manager -> 'a t -> bool
val is_top : 'a manager -> 'a t -> bool
val is_leq : 'a manager -> 'a t -> 'a t -> bool
val is_eq : 'a manager -> 'a t -> 'a t -> bool

val meet : 'a manager -> 'a t -> 'a t -> 'a t
val join : 'a manager -> 'a t -> 'a t -> 'a t
val meet_cond : 'a manager -> 'a t -> BddapronexprE.Bool.t -> 'a t
val assign_list :
  'a manager -> 'a t ->
  (string * BddapronexprE.expr) list ->
  'a t option ->
  'a t
val substitute_list :
  'a manager -> 'a t ->
  (string * BddapronexprE.expr) list ->
  'a t option ->
  'a t
val forget_list :
  'a manager -> 'a t -> string list -> 'a t

val change_environment : 'a manager -> 'a t -> env -> 'a t
val rename : 'a manager -> 'a t -> (string*string) list -> 'a t
val widening : 'a manager -> 'a t -> 'a t -> 'a t
val unify : 'a manager -> 'a t -> 'a t -> 'a t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

(*  ====================================================================== *)
(** {3 Abstract domain} *)
(*  ====================================================================== *)

type ('a,'b) t = ('a,'b Bddaprondomain.t) Bddenv.value
constraint 'a = ('c,'d,'e) #Bddapronexpr.O.env

val size : 'b manager -> ('a,'b) t -> int
val print : Format.formatter -> ('a,'b) t -> unit
val bottom : 'b manager -> (('c,'d,'e) #Bddapronexpr.O.env as 'a) -> ('a,'b) t
val top : 'b manager -> (('c,'d,'e) #Bddapronexpr.O.env as 'a) -> ('a,'b) t
val is_bottom : 'b manager -> ('a,'b) t -> bool
val is_top : 'b manager -> ('a,'b) t -> bool
val is_leq : 'b manager -> ('a,'b) t -> ('a,'b) t -> bool
val is_eq : 'b manager -> ('a,'b) t -> ('a,'b) t -> bool

val meet : 'b manager -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val join : 'b manager -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val meet_cond :
  'b manager ->
  (('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a,'b) t ->
  ('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env BddapronexprE.O.Bool.t ->
  ('a,'b) t

val assign_list :
  'b manager ->
  (('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a,'b) t ->
  (string * ('c, 'd, Bddapronexpr.cond) #Bddapronexpr.O.env  BddapronexprE.O.expr) list ->
  ('a,'b) t option ->
  ('a,'b) t
val substitute_list :
  'b manager ->
  (('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a,'b) t ->
  (string * ('c, 'd, Bddapronexpr.cond) #Bddapronexpr.O.env BddapronexprE.O.expr) list ->
  ('a,'b) t option ->
  ('a,'b) t
val forget_list :
  'b manager -> ('a,'b) t -> string list -> ('a,'b) t

val change_environment : 'b manager -> ('a,'b) t -> 'a -> ('a,'b) t
val rename : 'b manager -> ('a,'b) t -> (string*string) list -> ('a,'b) t
val widening : 'b manager -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val unify : 'b manager -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t

end
