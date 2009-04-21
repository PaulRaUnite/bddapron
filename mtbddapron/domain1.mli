(** Combined Boolean/Numerical domain with environment *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a man = 'a Domain0.man
type 'a t = (Env.t, 'a Domain0.t) Env.value

val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man

val size : 'a man -> 'a t -> int
val print : Format.formatter -> 'a t -> unit
val bottom : 'a man -> Env.t -> 'a t
val top: 'a man -> Env.t -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool

val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t

val meet_condition : 'a man -> Cond.t -> 'a t -> Expr1.Bool.t -> 'a t
val meet_condition2 : 'a man -> 'a t -> Expr2.Bool.t -> 'a t

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
val forget_list :
  'a man -> 'a t -> string list -> 'a t

val change_environment : 'a man -> 'a t -> Env.t -> 'a t
val rename : 'a man -> 'a t -> (string*string) list -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val unify : 'a man -> 'a t -> 'a t -> 'a t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows etxensions). The functions above are axtually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

  type ('a,'b) t = ('a,'b Domain0.t) Bdd.Env.value
  constraint 'a = ('c,'d) #Env.O.t

  val size : 'b man -> ('a,'b) t -> int
  val print : Format.formatter -> ('a,'b) t -> unit
  val bottom : 'b man -> 'a -> ('a,'b) t
  val top : 'b man -> 'a -> ('a,'b) t
  val is_bottom : 'b man -> ('a,'b) t -> bool
  val is_top : 'b man -> ('a,'b) t -> bool
  val is_leq : 'b man -> ('a,'b) t -> ('a,'b) t -> bool
  val is_eq : 'b man -> ('a,'b) t -> ('a,'b) t -> bool

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

  val change_environment : 'b man -> ('a,'b) t -> 'a -> ('a,'b) t
  val rename :
    'b man -> ('a,'b) t -> (string*string) list -> ('a,'b) t
  val widening : 'b man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val unify : 'b man -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t

end
