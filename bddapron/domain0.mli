(** Boolean/Numerical domain: generic interface *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

(** {3 Generic manager (normally a private type)} *)

type ('a,'b,'c,'d) man = {
  typ : 'b;
  man : 'c;
  canonicalize : ?apron:bool -> 'c -> 'd -> unit;
  size : 'c -> 'd -> int;
  print : Env.t -> Format.formatter -> 'd -> unit;
  bottom : 'c -> Env.t -> 'd;
  top : 'c -> Env.t -> 'd;
  of_apron : 'c -> Env.t -> 'a Apron.Abstract0.t -> 'd;
  is_bottom : 'c -> 'd -> bool;
  is_top : 'c -> 'd -> bool;
  is_leq : 'c -> 'd -> 'd -> bool;
  is_eq : 'c -> 'd -> 'd -> bool;
  to_bddapron : 'c -> 'd -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list;
  meet : 'c -> 'd -> 'd -> 'd;
  join : 'c -> 'd -> 'd -> 'd;
  meet_condition : 'c -> Env.t -> Cond.t -> 'd -> Expr0.Bool.t -> 'd;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'c ->  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd;
  substitute_lexpr : 'c ->  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd;
  forget_list : 'c -> Env.t -> 'd -> string list -> 'd;
  widening : 'c -> 'd -> 'd -> 'd;
  apply_change : bottom:'d -> 'c -> 'd -> Env.change -> 'd;
  apply_permutation : 'c -> 'd -> int array option * Apron.Dim.perm option -> 'd;
}

(** {3 Functions} *)

val canonicalize : ?apron:bool -> ('a, 'b, 'c, 'd) man -> 'd -> unit
val size : ('a, 'b, 'c, 'd) man -> 'd -> int
val print : ('a, 'b, 'c, 'd) man -> Env.t -> Format.formatter -> 'd -> unit
val bottom : ('a, 'b, 'c, 'd) man -> Env.t -> 'd
val top : ('a, 'b, 'c, 'd) man -> Env.t -> 'd
val of_apron : ('a, 'b, 'c, 'd) man -> Env.t -> 'a Apron.Abstract0.t -> 'd
val is_bottom : ('a, 'b, 'c, 'd) man -> 'd -> bool
val is_top : ('a, 'b, 'c, 'd) man -> 'd -> bool
val is_leq : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> bool
val is_eq : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> bool
val to_bddapron :
  ('a, 'b, 'c, 'd) man -> 'd -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
val meet : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd
val join : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd
val meet_condition :
  ('a, 'b, 'c, 'd) man -> Env.t -> Cond.t -> 'd -> Expr0.Bool.t -> 'd
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool -> 
  ('a, 'b, 'c, 'd) man ->
  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd
val substitute_lexpr :
  ('a, 'b, 'c, 'd) man ->
  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd
val forget_list : ('a, 'b, 'c, 'd) man -> Env.t -> 'd -> string list -> 'd
val widening : ('a, 'b, 'c, 'd) man -> 'd -> 'd -> 'd
val apply_change : bottom:'d -> ('a, 'b, 'c, 'd) man -> 'd -> Env.change -> 'd
val apply_permutation : ('a, 'b, 'c, 'd) man -> 'd -> int array option * Apron.Dim.perm option -> 'd

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    [`Mtbdd of 'a Mtbdddomain0.man],
    'a Mtbdddomain0.man,
    'a Mtbdddomain0.t
  ) man

val make_mtbdd : ?global:bool -> 'a Apron.Manager.t -> 'a mtbdd
val to_mtbdd : ('a, [> `Mtbdd of 'a Mtbdddomain0.man], 'c, 'd) man -> 'a Mtbdddomain0.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    [`Bdd of 'a Bdddomain0.man],
    'a Bdddomain0.man,
    'a Bdddomain0.t
  ) man

val make_bdd : 'a Apron.Manager.t -> 'a bdd
val to_bdd : ('a, [> `Bdd of 'a Bdddomain0.man], 'c, 'd) man -> 'a Bdddomain0.man
