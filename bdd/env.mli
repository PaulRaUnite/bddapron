(** Normalized managers/environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

exception Bddindex

(** Type defintion *)
type typdef = [
  | `Benum of string array
]

(** Types *)
type typ = [
  | `Bool
  | `Bint of (bool * int)
  | `Benum of string
]

(** (Internal type:) Expressions *)
type 'a expr = [
  | `Bool of 'a Cudd.Bdd.t
      (** Boolean *)
  | `Bint of 'a Int.t
      (** Bounded integer *)
  | `Benum of 'a Enum.t
      (** Enumerated *)
]

(** Environment *)
type ('a,'b,'c,'d) t0 = ('a,'b,'c,'d) Enum.env0 = {
  mutable cudd : 'c Cudd.Man.t;
    (** CUDD manager *)
  mutable typdef : (string, 'b) PMappe.t;
    (** Named types definitions *)
  mutable vartyp : (string, 'a) PMappe.t;
    (** Associate to a var/label its type *)
  mutable bddindex0 : int;
    (** First index for finite-type variables *)
  mutable bddsize : int;
    (** Number of indices dedicated to finite-type variables *)
  mutable bddindex : int;
    (** Next free index in BDDs used by [self#add_var]. *)
  mutable bddincr : int;
    (** Increment used by {!add_var} for incrementing
	[bddindex] *)
  mutable idcondvar : (int, string) PMappe.t;
    (** Associates to a BDD index the variable involved by it *)
  mutable vartid : (string, int array) PMappe.t;
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
  mutable varset : (string, 'c Cudd.Bdd.t) PMappe.t;
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
  mutable print_external_idcondb : Format.formatter -> int*bool -> unit;
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)
  mutable ext : 'd;
  copy_ext : 'd -> 'd;
}

(** {3 Opened signature} *)
module O : sig
  type ('a,'b,'c,'d) t = ('a,'b,'c,'d) t0
  constraint 'a = [>typ]
  constraint 'b = [>typdef]

  val make : 
    ?bddindex0:int -> ?bddsize:int -> ?relational:bool -> 
    'c Cudd.Man.t -> 'd -> ('d -> 'd) ->('a,'b,'c,'d) t
      (** Create a new database.  Default values for
	  [bddindex0,bddsize,relational] are [0,100,false].
	  [bddincr] is initialized to 1 if [relational=false], 2
	  otherwise. *)

  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    Format.formatter -> ('a,'b,'c,'d) t -> unit
      (** Print an environment *)
end

type 'c t = (typ,typdef,'c,unit) O.t

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)
val print_typ : Format.formatter -> [> typ] -> unit
  (** Print a type *)
val print_typdef : Format.formatter -> [> typdef] -> unit
  (** Print a type definition *)

val print_tid : Format.formatter -> int array -> unit
val print_idcondb : ('a,'b,'c,'d) O.t -> Format.formatter -> int * bool -> unit
val print_order : ('a,'b,'c,'d) O.t -> Format.formatter -> unit
    (** Print the BDD variable ordering *)

val print : Format.formatter -> ('a,'b,'c,'d) O.t -> unit
    (** Print an environment *)

(*  ********************************************************************** *)
(** {2 Constructors} *)
(*  ********************************************************************** *)


val make :
  ?bddindex0:int ->
  ?bddsize:int -> ?relational:bool -> 'c Cudd.Man.t -> 'c t
      (** Same as [O.make], but constrained signature. *)

val copy : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t 
      (** Copy *)

(*  ********************************************************************** *)
(** {2 Accessors} *)
(*  ********************************************************************** *)

val mem_typ : ('a,'b,'c,'d) O.t -> string -> bool
    (** Is the type defined in the database ? *)
val mem_var : ('a,'b,'c,'d) O.t -> string -> bool
    (** Is the label/var defined in the database ? *)
val mem_label : ('a,'b,'c,'d) O.t -> string -> bool
    (** Is the label a label defined in the database ? *)

val typdef_of_typ : ('a,'b,'c,'d) O.t -> string -> 'b
    (** Return the definition of the type *)

val typ_of_var : ('a,'b,'c,'d) O.t -> string -> 'a
    (** Return the type of the label/variable *)

val vars : ('a,'b,'c,'d) O.t -> string PSette.t
    (** Return the list of variables (not labels) *)
val labels : ('a,'b,'c,'d) O.t -> String.t PSette.t
    (** Return the list of labels (not variables) *)

(*  ********************************************************************** *)
(** {2 Adding types and variables} *)
(*  ********************************************************************** *)

val add_typ_with : ('a,'b,'c,'d) O.t -> string -> 'b -> unit
    (** Declaration of a new type *)

val add_vars_with : ('a,'b,'c,'d) O.t -> (string * 'a) list -> int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
val remove_vars_with : ('a,'b,'c,'d) O.t -> string list -> int array option
    (** Remove the set of variables, as well as all constraints,
	and possibly normalize the environment and return the
	applied permutation. *)
val rename_vars_with : ('a,'b,'c,'d) O.t -> (string * string) list -> int array option
    (** Rename the variables, and remove all constraints,possibly
	normalize the environment and return the applied
	permutation. *)

val add_typ : ('a,'b,'c,'d) O.t -> string -> 'b -> ('a,'b,'c,'d) O.t 
val add_vars : ('a,'b,'c,'d) O.t -> (string * 'a) list -> ('a,'b,'c,'d) O.t
val remove_vars : ('a,'b,'c,'d) O.t -> string list -> ('a,'b,'c,'d) O.t
val rename_vars : ('a,'b,'c,'d) O.t -> (string * string) list -> ('a,'b,'c,'d) O.t
  (** Functional versions of the previous functions *)

val add_var_with : ('a,'b,'c,'d) O.t -> string -> 'a -> unit
    (** Addition without normalization (internal) *)

(* ********************************************************************** *)
(** {2 Operations} *)
(* ********************************************************************** *)

val iter_ordered :
  ('a,'b,'c,'d) O.t -> (string -> int array -> unit) -> unit
    (** Iter on all finite-state variables declared in the database *)

val is_leq : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> bool
    (** Test inclusion of environments in terms of types and
	variables (but not in term of indexes) *)
val is_eq : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> bool
    (** Test equality of environments in terms of types and
	variables (but not in term of indexes) *)
val shift : ('a,'b,'c,'d) O.t -> int -> ('a,'b,'c,'d) O.t
    (** Shift all the indices by the offset *) 
val lce : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t
    (** Least common environment *)
val permutation12 : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> int array
    (** Permutation for going from a subenvironment to a superenvironment *)
val permutation21 : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> int array
    (** Permutation from a superenvironment to a subenvironment *)

(*  ********************************************************************** *)
(** {2 Precomputing change of environments} *)
(*  ********************************************************************** *)

(** Contain the computed information to switch from one
    environment to another one. *)
type 'a change = {
  intro : int array option;
    (** Permutation to apply for making space for new BDD
	variables *)
  remove : ('a Cudd.Bdd.t * int array) option;
    (** BDD variables to existentially quantify out, and
	permutation to apply *)
}
val compute_change : ('a,'b,'c,'d) O.t -> ('a,'b,'c,'d) O.t -> 'c change

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

val notfound : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Type of pairs [(environment, value)] *)
type ('a, 'b) value = { env : 'a; val0 : 'b; }

val make_value :
  ('a,'b,'c,'d) O.t ->
  'e -> (('a,'b,'c,'d) O.t, 'e) value
  (** Constructor *)

val extend_environment :
  ('e -> int array -> 'e) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  ('a,'b,'c,'d) O.t -> (('a,'b,'c,'d) O.t, 'e) value
    (** [extend_environment permute value env] embed [value] in
	the new (super)environment [env], by computing the
	permutation transformation and using [permute] to apply it
	to the value. *)

(*  ********************************************************************** *)
(** {2 Internal functions} *)
(*  ********************************************************************** *)

val compare_idb : int*bool -> int*bool -> int
  (** Comparison *)

(* ====================================================================== *)
(** {3 Normalisation} *)
(* ====================================================================== *)

val permutation : ('a,'b,'c,'d) O.t -> int array
    (** Compute the permutation for normalizing the environment *)
val permute_with : ('a,'b,'c,'d) O.t -> int array -> unit
    (** Apply the given permutation to the environment *)
val normalize_with : ('a,'b,'c,'d) O.t -> int array
    (** Combine the two previous functions, and return the permutation *)
val check_normalized : ('a,'b,'c,'d) O.t -> bool
    (** Prints error message and returns [false] if not normalized *)

(* ====================================================================== *)
(** {3 Permutations} *)
(* ====================================================================== *)

val permute_expr : 'a expr -> int array -> 'a expr
val compose_permutation : int array -> int array -> int array
val compose_opermutation :
  int array option -> int array option -> int array option
val permutation_of_offset : int -> int -> int array

(* ====================================================================== *)
(** {3 Used by level1 APIs} *)
(* ====================================================================== *)

val check_var : ('a,'b,'c,'d) O.t -> string -> unit
val check_lvar : ('a,'b,'c,'d) O.t -> string list -> unit
val check_value :
  ('a,'b,'c,'d) O.t -> 
  (('a,'b,'c,'d) O.t, 'e) value -> unit
val check_value2 :
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value -> unit
val check_value3 :
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value -> 
  (('a,'b,'c,'d) O.t, 'g) value -> unit

val check_lvarvalue :
  ('a,'b,'c,'d) O.t ->
  (string * (('a,'b,'c,'d) O.t, 'e) value) list -> (string * 'e) list
val check_lvalue :
  ('a,'b,'c,'d) O.t ->
  (('a,'b,'c,'d) O.t, 'e) value list -> 'e list
val check_ovalue :
  ('a,'b,'c,'d) O.t ->
  (('a,'b,'c,'d) O.t, 'e) value option -> 'e option

val mapunop :
  ('e -> 'f) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value

val mapbinop :
  ('e -> 'f -> 'g) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value ->
  (('a,'b,'c,'d) O.t, 'g) value
val mapbinope :
  (('a,'b,'c,'d) O.t -> 'e -> 'f -> 'g) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value -> (('a,'b,'c,'d) O.t, 'g) value
val mapterop :
  ('e -> 'f -> 'g -> 'h) ->
  (('a,'b,'c,'d) O.t, 'e) value ->
  (('a,'b,'c,'d) O.t, 'f) value ->
  (('a,'b,'c,'d) O.t, 'g) value ->
  (('a,'b,'c,'d) O.t, 'h) value

