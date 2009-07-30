(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

(** {3 Generic manager (normally a private type)} *)

type ('a,'b,'c,'d) man = {
  typ : 'b;
  man : 'c;
  size : 'c -> 'd -> int;
  print : Format.formatter -> 'd -> unit;
  bottom : 'c -> Env.t -> 'd;
  top : 'c -> Env.t -> 'd;
  of_apron : 'c -> Env.t -> 'a Apron.Abstract1.t -> 'd;
  is_bottom : 'c -> 'd -> bool;
  is_top : 'c -> 'd -> bool;
  is_leq : 'c -> 'd -> 'd -> bool;
  is_eq : 'c -> 'd -> 'd -> bool;
  to_bddapron : 'c -> 'd -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list;
  meet : 'c -> 'd -> 'd -> 'd;
  join : 'c -> 'd -> 'd -> 'd;
  meet_condition : 'c -> Cond.t -> 'd -> Expr1.Bool.t -> 'd;
  meet_condition2 : 'c -> 'd -> Expr2.Bool.t -> 'd;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'c ->  Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd;
  substitute_lexpr : 'c -> Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd;
  assign_listexpr2 : ?relational:bool -> ?nodependency:bool -> 'c -> 'd -> string list -> Expr2.List.t -> 'd option -> 'd;
  substitute_listexpr2 : 'c -> 'd -> string list -> Expr2.List.t -> 'd option -> 'd;
  forget_list : 'c -> 'd -> string list -> 'd;
  widening : 'c -> 'd -> 'd -> 'd;
}

(** {3 Functions} *)

val size : ('a, 'b, 'c, 'd) man -> 'd -> int
val print : ('a, 'b, 'c, 'd) man -> Format.formatter -> 'd -> unit
val bottom : ('a, 'b, 'c, 'd) man -> Env.t -> 'd
val top : ('a, 'b, 'c, 'd) man -> Env.t -> 'd
val of_apron : ('a, 'b, 'c, 'd) man -> Env.t -> 'a Apron.Abstract1.t -> 'd
val is_bottom : ('a, 'b, 'c, 'd) man -> 'd -> bool
val is_top : ('a, 'b, 'c, 'd) man -> 'd -> bool
val is_leq : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> bool
val is_eq : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> bool
val to_bddapron :
  ('a, 'b, 'c, 'd) man -> 'd -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
val meet : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd
val join : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd
val meet_condition :
  ('a, 'b, 'c, 'd) man -> Cond.t -> 'd -> Expr1.Bool.t -> 'd
val meet_condition2 :
  ('a, 'b, 'c, 'd) man -> 'd -> Expr2.Bool.t -> 'd
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool -> 
  ('a, 'b, 'c, 'd) man ->
  Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd
val substitute_lexpr :
  ('a, 'b, 'c, 'd) man ->
  Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd
val assign_listexpr2 :
  ?relational:bool -> ?nodependency:bool -> 
  ('a, 'b, 'c, 'd) man ->
  'd -> string list -> Expr2.List.t -> 'd option -> 'd
val substitute_listexpr2 :
  ('a, 'b, 'c, 'd) man ->
  'd -> string list -> Expr2.List.t -> 'd option -> 'd
val forget_list : ('a, 'b, 'c, 'd) man -> 'd -> string list -> 'd
val widening : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    [`Mtbdd of 'a Mtbdddomain1.man],
    'a Mtbdddomain1.man,
    'a Mtbdddomain1.t
  ) man

val make_mtbdd : ?global:bool -> 'a Apron.Manager.t -> 'a mtbdd
val to_mtbdd : ('a, [> `Mtbdd of 'a Mtbdddomain1.man], 'c, 'd) man -> 'a Mtbdddomain1.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    [`Bdd of 'a Bdddomain1.man],
    'a Bdddomain1.man,
    'a Bdddomain1.t
  ) man

val make_bdd : 'a Apron.Manager.t -> 'a bdd
val to_bdd : ('a, [> `Bdd of 'a Bdddomain1.man], 'c, 'd) man -> 'a Bdddomain1.man
