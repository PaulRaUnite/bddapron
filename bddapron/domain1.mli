(** Boolean/Numerical domain linked to environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

type ('a, 'b, 'c, 'd) man = ('a, 'b, 'c, 'd) Domain0.man
type 'd t = (Env.t, 'd) Env.value

val canonicalize : ?apron:bool -> ('a, 'b, 'c, 'd) man -> 'd t -> unit
val print : ('a, 'b, 'c, 'd) man -> Format.formatter -> 'd t -> unit

val size : ('a, 'b, 'c, 'd) man -> 'd t -> int
val bottom : ('a, 'b, 'c, 'd) man -> Env.t -> 'd t
val top : ('a, 'b, 'c, 'd) man -> Env.t -> 'd t
val of_apron : ('a, 'b, 'c, 'd) man -> Env.t -> 'a Apron.Abstract1.t -> 'd t
val is_bottom : ('a, 'b, 'c, 'd) man -> 'd t -> bool
val is_top : ('a, 'b, 'c, 'd) man -> 'd t -> bool
val is_leq : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> bool
val is_eq : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> bool
val to_bddapron :
  ('a, 'b, 'c, 'd) man -> 'd t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
val meet : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> 'd t
val join : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> 'd t
val meet_condition :
  ('a, 'b, 'c, 'd) man -> Cond.t ->
  'd t -> Expr1.Bool.t -> 'd t
val meet_condition2 :
  ('a, 'b, 'c, 'd) man ->
  'd t -> Expr2.Bool.t -> 'd t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a, 'b, 'c, 'd) man -> Cond.t ->
  'd t -> string list -> Expr1.t list -> 'd t option -> 'd t
val assign_listexpr2 :
  ?relational:bool ->  ?nodependency:bool ->
  ('a, 'b, 'c, 'd) man ->
  'd t -> string list -> Expr2.List.t -> 'd t option -> 'd t
val substitute_lexpr :
  ('a, 'b, 'c, 'd) man -> Cond.t ->
  'd t -> string list -> Expr1.t list -> 'd t option -> 'd t
val substitute_listexpr2 :
  ('a, 'b, 'c, 'd) man ->
  'd t -> string list -> Expr2.List.t -> 'd t option -> 'd t
val forget_list : ('a, 'b, 'c, 'd) man -> 'd t -> string list -> 'd t
val widening : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> 'd t
val change_environment :
  ('a, 'b, 'c, 'd) man -> 'd t -> Env.t -> 'd t
val unify : ('a, 'b, 'c, 'd) man -> 'd t -> 'd t -> 'd t
val rename : ('a, 'b, 'c, 'd) man -> 'd t -> (string * string) list -> 'd t

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

val make_mtbdd : ?global:bool -> 'a Apron.Manager.t -> 'a Domain0.mtbdd
val to_mtbdd : ('a, [> `Mtbdd of 'a Mtbdddomain0.man], 'c, 'd)  Domain0.man -> 'a Mtbdddomain0.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

val make_bdd : 'a Apron.Manager.t -> 'a Domain0.bdd
val to_bdd : ('a, [> `Bdd of 'a Bdddomain0.man], 'c, 'd) Domain0.man -> 'a Bdddomain0.man
