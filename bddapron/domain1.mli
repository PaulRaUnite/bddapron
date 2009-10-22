(** Boolean/Numerical domain linked to environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Types} *)
(*  ====================================================================== *)

type ('a, 'b, 'c) man = ('a, 'b, 'c) Domain0.man
(**  Type of generic managers.
 
    - ['a]: as in ['a Apron.Manager.t] 
            ([Box.t], [Polka.strict Polka.t], etc);
    - ['b]: type of the underlying manager;
    - ['c]: type of the underlying abstract values of level 0.
*)

type 'c t = (Env.t, 'c) Env.value
(** Type of generic abstract values *)

val canonicalize : ?apron:bool -> ('a, 'b, 'c) man -> 'c t -> unit
val print : ('a, 'b, 'c) man -> Format.formatter -> 'c t -> unit

val size : ('a, 'b, 'c) man -> 'c t -> int
val bottom : ('a, 'b, 'c) man -> Env.t -> 'c t
val top : ('a, 'b, 'c) man -> Env.t -> 'c t
val of_apron : ('a, 'b, 'c) man -> Env.t -> 'a Apron.Abstract1.t -> 'c t
val is_bottom : ('a, 'b, 'c) man -> 'c t -> bool
val is_top : ('a, 'b, 'c) man -> 'c t -> bool
val is_leq : ('a, 'b, 'c) man -> 'c t -> 'c t -> bool
val is_eq : ('a, 'b, 'c) man -> 'c t -> 'c t -> bool
val to_bddapron :
  ('a, 'b, 'c) man -> 'c t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
val meet : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val join : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val meet_condition :
  ('a, 'b, 'c) man -> Cond.t ->
  'c t -> Expr1.Bool.t -> 'c t
val meet_condition2 :
  ('a, 'b, 'c) man ->
  'c t -> Expr2.Bool.t -> 'c t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  ('a, 'b, 'c) man -> Cond.t ->
  'c t -> string list -> Expr1.t list -> 'c t option -> 'c t
val assign_listexpr2 :
  ?relational:bool ->  ?nodependency:bool ->
  ('a, 'b, 'c) man ->
  'c t -> string list -> Expr2.List.t -> 'c t option -> 'c t
val substitute_lexpr :
  ('a, 'b, 'c) man -> Cond.t ->
  'c t -> string list -> Expr1.t list -> 'c t option -> 'c t
val substitute_listexpr2 :
  ('a, 'b, 'c) man ->
  'c t -> string list -> Expr2.List.t -> 'c t option -> 'c t
val forget_list : ('a, 'b, 'c) man -> 'c t -> string list -> 'c t
val widening : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val change_environment :
  ('a, 'b, 'c) man -> 'c t -> Env.t -> 'c t
val unify : ('a, 'b, 'c) man -> 'c t -> 'c t -> 'c t
val rename : ('a, 'b, 'c) man -> 'c t -> (string * string) list -> 'c t

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

val make_mtbdd : ?global:bool -> 'a Apron.Manager.t -> 'a Domain0.mtbdd
  (** Make a mtbdd manager *)

(*  ====================================================================== *)
(** {3 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_mtbdd : ('a, 'b, 'c) man -> bool
  (** Return [true] iff the argument manager is a mtbdd manager *)
val man_of_mtbdd : 'a Domain0.mtbdd -> ('a, 'b, 'c) man
  (** Makes a mtbdd manager generic *)
val man_to_mtbdd : ('a, 'b, 'c) man -> 'a Domain0.mtbdd
  (** Instanciate the type of a mtbdd manager.
      Raises [Failure] if the argument manager is not a mtbdd manager *)

val of_mtbdd : 
  'a Domain0.mtbdd * 'a Mtbdddomain0.t t -> 
  ('a, 'b, 'c) man * 'c t
  (** Makes a pair (mtbdd manager,mtbdd abstract value) generic *)
val to_mtbdd : 
  ('a, 'b, 'c) man * 'c t ->
  'a Domain0.mtbdd * 'a Mtbdddomain0.t t
  (** Instanciate the type of a pair (mtbdd manager,mtbdd abstract value).
      Raises [Failure] if the argument manager is not a mtbdd manager *)

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

val make_bdd : 'a Apron.Manager.t -> 'a Domain0.bdd

(*  ====================================================================== *)
(** {3 Type conversion functions} *)
(*  ====================================================================== *)

val man_is_bdd : ('a, 'b, 'c) man -> bool
  (** Return [true] iff the argument manager is a bdd manager *)
val man_of_bdd : 'a Domain0.bdd -> ('a, 'b, 'c) man
  (** Makes a bdd manager generic *)
val man_to_bdd : ('a, 'b, 'c) man -> 'a Domain0.bdd
  (** Instanciate the type of a bdd manager.
      Raises [Failure] if the argument manager is not a bdd manager *)

val of_bdd : 
  'a Domain0.bdd * 'a Bdddomain0.t t -> 
  ('a, 'b, 'c) man * 'c t
  (** Makes a pair (bdd manager,bdd abstract value) generic *)
val to_bdd : 
  ('a, 'b, 'c) man * 'c t -> 
  'a Domain0.bdd * 'a Bdddomain0.t t
  (** Instanciate the type of a pair (bdd manager,bdd abstract value).
      Raises [Failure] if the argument manager is not a bdd manager *)

