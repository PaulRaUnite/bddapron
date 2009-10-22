(** Boolean/Numerical domain: generic interface *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Types} *)
(*  ====================================================================== *)

type ('a,'b,'c) man = private {
  typ : string;
  man : 'b;
  canonicalize : ?apron:bool -> 'b -> 'c -> unit;
  size : 'b -> 'c -> int;
  print : Env.t -> Format.formatter -> 'c -> unit;
  bottom : 'b -> Env.t -> 'c;
  top : 'b -> Env.t -> 'c;
  of_apron : 'b -> Env.t -> 'a Apron.Abstract0.t -> 'c;
  is_bottom : 'b -> 'c -> bool;
  is_top : 'b -> 'c -> bool;
  is_leq : 'b -> 'c -> 'c -> bool;
  is_eq : 'b -> 'c -> 'c -> bool;
  to_bddapron : 'b -> 'c -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list;
  meet : 'b -> 'c -> 'c -> 'c;
  join : 'b -> 'c -> 'c -> 'c;
  meet_condition : 'b -> Env.t -> Cond.t -> 'c -> Expr0.Bool.t -> 'c;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'b ->  Env.t -> Cond.t -> 'c -> string list -> Expr0.t list -> 'c option -> 'c;
  substitute_lexpr : 'b ->  Env.t -> Cond.t -> 'c -> string list -> Expr0.t list -> 'c option -> 'c;
  forget_list : 'b -> Env.t -> 'c -> string list -> 'c;
  widening : 'b -> 'c -> 'c -> 'c;
  apply_change :  bottom:'c -> 'b -> 'c -> Env.change -> 'c;
  apply_permutation : 'b -> 'c -> int array option * Apron.Dim.perm option -> 'c;
}
(** Type of generic managers 

    - ['a]: as in ['a Apron.Manager.t]
	    ([Box.t], [Polka.strict Polka.t], etc);
    - ['b]: type of the underlying manager;
    - ['c]: type of the underlying abstract values of level 0.
*)

type 'c t = 'c
(** Type of generic abstract values *)

(*  ====================================================================== *)
(** {3 Functions} *)
(*  ====================================================================== *)

val canonicalize : ?apron:bool -> ('a, 'b, 'c) man -> 'c t -> unit
val size : ('a, 'b, 'c) man -> 'c t -> int
val print : ('a, 'b, 'c) man -> Env.t -> Format.formatter -> 'c t -> unit
val bottom : ('a, 'b, 'c) man -> Env.t -> 'c t
val top : ('a, 'b, 'c) man -> Env.t -> 'c t
val of_apron : ('a, 'b, 'c) man -> Env.t -> 'a Apron.Abstract0.t -> 'c t
val is_bottom : ('a, 'b, 'c) man -> 'c t -> bool
val is_top : ('a, 'b, 'c) man -> 'c t -> bool
val is_leq : ('a, 'b, 'c) man -> 'c t -> 'c t -> bool
val is_eq : ('a, 'b, 'c) man -> 'c t -> 'c t -> bool
val to_bddapron :
  ('a, 'b, 'c) man -> 'c t -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
val meet : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val join : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val meet_condition :
  ('a, 'b, 'c) man -> Env.t -> Cond.t -> 'c t -> Expr0.Bool.t -> 'c t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool -> 
  ('a, 'b, 'c) man ->
  Env.t -> Cond.t -> 'c t -> string list -> Expr0.t list -> 'c t option -> 'c t
val substitute_lexpr :
  ('a, 'b, 'c) man ->
  Env.t -> Cond.t -> 'c t -> string list -> Expr0.t list -> 'c t option -> 'c t
val forget_list : ('a, 'b, 'c) man -> Env.t -> 'c t -> string list -> 'c t
val widening : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val apply_change : bottom:'c t -> ('a, 'b, 'c) man -> 'c t -> Env.change -> 'c t
val apply_permutation : ('a, 'b, 'c) man -> 'c t -> int array option * Apron.Dim.perm option -> 'c t

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    'a Mtbdddomain0.man,
    'a Mtbdddomain0.t
  ) man

val make_mtbdd : ?global:bool -> 'a Apron.Manager.t -> 'a mtbdd
  (** Make a mtbdd manager *)

(*  ====================================================================== *)
(** {3 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_mtbdd : ('a, 'b, 'c) man -> bool
  (** Return [true] iff the argument manager is a mtbdd manager *)
val man_of_mtbdd : 'a mtbdd -> ('a, 'b, 'c) man
  (** Makes a mtbdd manager generic *)
val man_to_mtbdd : ('a, 'b, 'c) man -> 'a mtbdd
  (** Instanciate the type of a mtbdd manager.
      Raises [Failure] if the argument manager is not a mtbdd manager *)

val of_mtbdd : 'a mtbdd * 'a Mtbdddomain0.t t -> ('a, 'b, 'c) man * 'c t
  (** Makes a pair (mtbdd manager,mtbdd abstract value) generic *)
val to_mtbdd : ('a, 'b, 'c) man * 'c t -> 'a mtbdd * 'a Mtbdddomain0.t t
  (** Instanciate the type of a pair (mtbdd manager,mtbdd abstract value).
      Raises [Failure] if the argument manager is not a mtbdd manager *)

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    'a Bdddomain0.man,
    'a Bdddomain0.t
  ) man

val make_bdd : 'a Apron.Manager.t -> 'a bdd
  (** Make a bdd manager *)

(*  ====================================================================== *)
(** {3 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_bdd : ('a, 'b, 'c) man -> bool
  (** Return [true] iff the argument manager is a bdd manager *)
val man_of_bdd : 'a bdd -> ('a, 'b, 'c) man
  (** Makes a bdd manager generic *)
val man_to_bdd : ('a, 'b, 'c) man -> 'a bdd
  (** Instanciate the type of a bdd manager.
      Raises [Failure] if the argument manager is not a bdd manager *)

val of_bdd : 'a bdd * 'a Bdddomain0.t t -> ('a, 'b, 'c) man * 'c t
  (** Makes a pair (bdd manager,bdd abstract value) generic *)
val to_bdd : ('a, 'b, 'c) man * 'c t -> 'a bdd * 'a Bdddomain0.t t
  (** Instanciate the type of a pair (bdd manager,bdd abstract value).
      Raises [Failure] if the argument manager is not a bdd manager *)

