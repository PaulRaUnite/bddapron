(** Normalized variable managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** Types *)
type typ = [
  | Bdd.Env.typ
  | Apronexpr.typ
]

(** Type definitions *)
type typdef = Bdd.Env.typdef

(** Environment *)
type 'a ext = {
  mutable eapron : Apron.Environment.t;
  mutable aext : 'a;
}
type ('a,'b,'c) t0 = ('a,'b,Cudd.Man.v,'c ext) Bdd.Env.t0

(** {3 Opened signature} *)
module O : sig
  type ('a,'b,'c) t = ('a,'b,'c) t0
  constraint 'a = [>typ]
  constraint 'b = [>typdef]

  val make : 
    ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> 
    Cudd.Man.vt -> 'c -> ('c -> 'c) -> ('a,'b,'c) t
      (** Create a new database.  Default values for
	  [bddindex0,bddsize,relational] are [0,100,false].
	  [bddincr] is initialized to 1 if [relational=false], 2
	  otherwise. *)
  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    Format.formatter -> ('a,'b,'c) t -> unit
      (** Print an environment *)
end

type t = (typ,typdef,unit) O.t

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)
val print_typ : Format.formatter -> [> typ] -> unit
  (** Print a type *)
val print_typdef : Format.formatter -> [> typdef] -> unit
  (** Print a type definition *)

val print_idcondb : ('a,'b,'c) O.t -> Format.formatter -> int * bool -> unit
val print_order : ('a,'b,'c) O.t -> Format.formatter -> unit
    (** Print the BDD variable ordering *)

val print : Format.formatter -> ('a,'b,'c) O.t -> unit
    (** Print an environment *)

(*  ********************************************************************** *)
(** {2 Constructors} *)
(*  ********************************************************************** *)


val make :
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> Cudd.Man.vt -> t
      (** Same as [O.make], but constrained signature. *)

val copy : ('a,'b,'c) O.t -> ('a,'b,'c) O.t 
      (** Copy *)

(*  ********************************************************************** *)
(** {2 Accessors} *)
(*  ********************************************************************** *)

val mem_typ : ('a,'b,'c) O.t -> string -> bool
    (** Is the type defined in the database ? *)
val mem_var : ('a,'b,'c) O.t -> string -> bool
    (** Is the label/var defined in the database ? *)
val mem_label : ('a,'b,'c) O.t -> string -> bool
    (** Is the label a label defined in the database ? *)

val typdef_of_typ : ('a,'b,'c) O.t -> string -> 'b
    (** Return the definition of the type *)

val typ_of_var : ('a,'b,'c) O.t -> string -> 'a
    (** Return the type of the label/variable *)

val vars : ('a,'b,'c) O.t -> string PSette.t
    (** Return the list of variables (not labels) *)
val labels : ('a,'b,'c) O.t -> String.t PSette.t
    (** Return the list of labels (not variables) *)

(*  ********************************************************************** *)
(** {2 Adding types and variables} *)
(*  ********************************************************************** *)

val add_typ_with : ('a,'b,'c) O.t -> string -> 'b -> unit
    (** Declaration of a new type *)

val add_vars_with :
  ('a,'b,'c) O.t -> (string * 'a) list -> 
  int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
val remove_vars_with : 
  ('a,'b,'c) O.t -> string list -> 
  int array option
    (** Remove the set of variables, as well as all constraints,
	and possibly normalize the environment and return the
	applied permutation. *)
val rename_vars_with : 
  ('a,'b,'c) O.t -> (string * string) list -> 
  int array option * Apron.Dim.perm option
    (** Rename the variables, and remove all constraints,possibly
	normalize the environment and return the applied
	permutation. *)

val add_typ : ('a,'b,'c) O.t -> string -> 'b -> ('a,'b,'c) O.t 
val add_vars : ('a,'b,'c) O.t -> (string * 'a) list -> ('a,'b,'c) O.t
val remove_vars : ('a,'b,'c) O.t -> string list -> ('a,'b,'c) O.t
val rename_vars : ('a,'b,'c) O.t -> (string * string) list -> ('a,'b,'c) O.t
  (** Functional versions of the previous functions *)


(* ********************************************************************** *)
(** {2 Operations} *)
(* ********************************************************************** *)

val is_leq : ('a,'b,'c) O.t -> ('a,'b,'c) O.t -> bool
    (** Test inclusion of environments in terms of types and
	variables (but not in term of indexes) *)
val is_eq : ('a,'b,'c) O.t -> ('a,'b,'c) O.t -> bool
    (** Test equality of environments in terms of types and
	variables (but not in term of indexes) *)
val lce : ('a,'b,'c) O.t -> ('a,'b,'c) O.t -> ('a,'b,'c) O.t
    (** Least common environment *)

(*  ********************************************************************** *)
(** {2 Precomputing change of environments} *)
(*  ********************************************************************** *)

type change = {
  cbdd : Cudd.Man.v Bdd.Env.change;
  capron : Apron.Dim.change2;
}

val compute_change : ('a,'b,'c) O.t -> ('a,'b,'c) O.t -> change

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a, 'b) value = ('a, 'b) Bdd.Env.value = { env : 'a; val0 : 'b; }

val make_value :
  ('a,'b,'c) O.t ->
  'd -> (('a,'b,'c) O.t, 'd) value
  (** Constructor *)

val check_var : ('a,'b,'c) O.t -> string -> unit
val check_lvar : ('a,'b,'c) O.t -> string list -> unit
val check_value :
  ('a,'b,'c) O.t -> 
  (('a,'b,'c) O.t, 'd) value -> unit
val check_value2 :
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value -> unit
val check_value3 :
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value -> 
  (('a,'b,'c) O.t, 'f) value -> unit

val check_lvarvalue :
  ('a,'b,'c) O.t ->
  (string * (('a,'b,'c) O.t, 'd) value) list -> (string * 'd) list
val check_lvalue :
  ('a,'b,'c) O.t ->
  (('a,'b,'c) O.t, 'd) value list -> 'd list
val check_ovalue :
  ('a,'b,'c) O.t ->
  (('a,'b,'c) O.t, 'd) value option -> 'd option

val mapunop :
  ('d -> 'e) ->
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value

val mapbinop :
  ('d -> 'e -> 'f) ->
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value ->
  (('a,'b,'c) O.t, 'f) value
val mapbinope :
  (('a,'b,'c) O.t -> 'd -> 'e -> 'f) ->
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value -> (('a,'b,'c) O.t, 'f) value
val mapterop :
  ('d -> 'e -> 'f -> 'g) ->
  (('a,'b,'c) O.t, 'd) value ->
  (('a,'b,'c) O.t, 'e) value ->
  (('a,'b,'c) O.t, 'f) value ->
  (('a,'b,'c) O.t, 'g) value

