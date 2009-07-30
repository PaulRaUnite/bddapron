(** Normalized managers/environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
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

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O : sig
  class type ['a,'b,'c] t = object('d)
    constraint 'a = [> typ]
    constraint 'b = [> typdef]

    method add_typ : string -> 'b -> unit
    (** Declaration of a new type *)
    method mem_typ : string -> bool
    (** Is the type defined in the database ? *)
    method typdef_of_typ : string -> 'b
    (** Return the definition of the type *)

    method add_vars : (string * 'a) list -> int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
    method remove_vars : string list -> int array option
    (** Remove the set of variables, as well as all constraints,
	and possibly normalize the environment and return the
	applied permutation. *)
    method rename_vars : (string * string) list -> int array option
    (** Rename the variables, and remove all constraints,possibly
	normalize the environment and return the applied
	permutation. *)
    method mem_var : string -> bool
    (** Is the label/var defined in the database ? *)
    method mem_label : string -> bool
    (** Is the label a label defined in the database ? *)
    method typ_of_var : string -> 'a
    (** Return the type of the label/variable *)
    method vars : string PSette.t
    (** Return the list of variables (not labels) *)
    method labels : string PSette.t
    (** Return the list of labels (not variables) *)

    method permutation : int array
    (** Compute the permutation for normalizing the environment *)
    method permute : int array -> unit
    (** Apply the given permutation to the environment *)
    method normalize : int array
    (** Combine the two previous functions, and return the permutation *)
(*
    method pair : unit
    (** Assuming [bddindex] is even and [bddincr=2]), if [v_paired=false],
	groups variable by pair *)
*)
    method add_var : string -> 'a -> unit
    (** Private function *)

    method print_idcondb : Format.formatter -> int*bool -> unit
    method print_order : Format.formatter -> unit
    (** Print the BDD variable ordering *)

    val v_cudd : 'c Cudd.Man.t
    (** CUDD manager *)
    val mutable v_paired : bool
    (** Are pair groups active (default false) ? *)
    val mutable v_typdef : (string, 'b) PMappe.t
    (** Named types definitions *)
    val mutable v_vartyp : (string, 'a) PMappe.t
    (** Associate to a var/label its type *)
    val mutable v_bddindex0 : int
    (** First index for finite-type variables *)
    val mutable v_bddsize : int
    (** Number of indices dedicated to finite-type variables *)
    val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
    val v_bddincr : int
    (** Increment used by [self#add_var] for incrementing
	[self#_v_bddindex] *)
    val mutable v_idcondvar : (int, string) PMappe.t
    (** Associates to a BDD index the variable involved by it *)
    val mutable v_vartid : (string, int array) PMappe.t
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
    val mutable v_varset : (string, 'c Cudd.Bdd.t) PMappe.t
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
    val mutable v_print_external_idcondb : Format.formatter -> int*bool -> unit
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)

    method cudd : 'c Cudd.Man.t
    method typdef : (string, 'b) PMappe.t
    method vartyp : (string, 'a) PMappe.t
    method bddindex0 : int
    method bddsize : int
    method bddindex : int
    method bddincr : int
    method idcondvar : (int, string) PMappe.t
    method vartid : (string, int array) PMappe.t
    method varset : (string, 'c Cudd.Bdd.t) PMappe.t
    method print_external_idcondb : Format.formatter -> int*bool -> unit
    method set_bddindex0 : int -> unit
    method set_bddsize : int -> unit
    method set_bddindex : int -> unit
    method set_idcondvar : (int, string) PMappe.t -> unit
    method set_vartid : (string, int array) PMappe.t -> unit
    method set_varset : (string, 'c Cudd.Bdd.t) PMappe.t -> unit
    method set_typdef : (string, 'b) PMappe.t -> unit
    method set_vartyp : (string, 'a) PMappe.t -> unit
    method set_print_external_idcondb :
      (Format.formatter -> int*bool -> unit) -> unit
  end

  class ['a,'b,'c] make:
    ?bddindex0:int ->
    ?bddsize:int -> 
    ?relational:bool ->
    'c Cudd.Man.t ->
    ['a,'b,'c] t
    (** Creates an environment from scratch *)

  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a,'b,'c) #t -> unit
    (** Print an environment *)

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type ['a] t = [typ, typdef, 'a] O.t
class ['a] make :
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool ->
  'a Cudd.Man.t -> ['a] t

val make :
  ?bddindex0:int -> ?bddsize:int -> ?relational:bool ->
  'a Cudd.Man.t -> 'a t
      (** Create a new database.
	  [bddindex] and [bddincr] initalized to 0 and 1. *)

val print : Format.formatter -> ([>typ ], [>typdef ], 'a) #O.t -> unit

(** {3 Functional version of some methods} *)

val add_typ : (('a,'b,'c) #O.t as 'd) -> string -> 'b -> 'd
val add_vars : (('a,'b,'c) #O.t as 'd) -> (string * 'a) list -> 'd
val remove_vars : (('a,'b,'c) #O.t as 'd) -> string list -> 'd
val rename_vars : (('a,'b,'c) #O.t as 'd) -> (string * string) list -> 'd

(** {3 Functions} *)

val is_leq : ('a,'b,'c) #O.t -> ('a,'b,'c) #O.t -> bool
    (** Test inclusion of environments in terms of types and
	variables (but not in term of indexes) *)
val is_eq : ('a,'b,'c) #O.t -> ('a,'b,'c) #O.t -> bool
    (** Test equality of environments in terms of types and
	variables (but not in term of indexes) *)
val shift : (('a,'b,'c) #O.t as 'd) -> int -> 'd
    (** Shift all the indices by the offset *) 
val lce : (('a,'b,'c) #O.t as 'd) -> 'd -> 'd
    (** Least common environment *)
(*
val lce2 :
  (('a,'b,'c) #O.t as 'd) ->
  ('a,'b,'c) #O.t -> 'd
    (** Least common environment, special version *)
*)
  
val compose_permutation : int array -> int array -> int array
val compose_opermutation : int array option -> int array option -> int array option
val permutation_of_offset : int -> int -> int array

val check_normalized : ('a,'b,'c) #O.t -> bool
    (** Prints error message and returns [false] if not normalized *)
val permutation12 : ('a,'b,'c) #O.t -> ('a,'b,'c) #O.t -> int array
    (** Permutation for going from a subenvironment to a superenvironment *)
val permutation21 : ('a,'b,'c) #O.t -> ('a,'b,'c) #O.t -> int array
    (** Permutation from a superenvironment to a subenvironment *)
  
val iter_ordered : ('a,'b,'c) #O.t -> (string -> int array -> unit) -> unit
  (** Iter on all finite-state variables declared in the database *)
  
val compare_idb : int*bool -> int*bool -> int
  (** Comparison *)
  
val print_tid : Format.formatter -> int array -> unit
  
val print_typ : Format.formatter -> [>typ] -> unit
  (** Print a type *)
val print_typdef : Format.formatter -> [>typdef] -> unit
  (** Print a type definition *)
val print :
  Format.formatter -> ('a,'b,'c) #O.t -> unit
    (** Print an environment *)

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
val compute_change : (('a,'b,'c) #O.t as 'd) -> 'd -> 'c change

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a,'b) value = {
  env : 'a;
  val0 : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
  (** Constructor *)

val extend_environment :
  ('a -> int array -> 'a) ->
  (('b,'c,'d) #O.t as 'env, 'a) value -> 'env -> ('env, 'a) value
    (** [extend_environment permute value env] embed [value] in the new
      (super)environment [env], by computing the permutation transformation and
      using [permute] to apply it to the value. *)


val check_var :
  ([> typ ], [> typdef ], 'a) #O.t -> string -> unit
val check_lvar :
  ([> typ ], [> typdef ], 'a) #O.t ->
  string list -> unit
val mapunop :
  ('a -> 'b) -> ('c, 'a) value -> ('c, 'b) value
val check_value :
  ([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t ->
  (('a, 'b, 'c) #O.t, 'd) value -> unit
val check_value2 :
  (([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t, 'd)
  value -> (('a, 'b, 'c) #O.t, 'e) value -> unit
val mapbinop :
  ('a -> 'b -> 'c) ->
  (([> typ ] as 'e, [> typdef ] as 'f, 'g) #O.t as 'd,
  'a) value ->
  (('e, 'f, 'g) #O.t, 'b) value -> ('d, 'c) value
val mapbinope :
  ((([> typ ] as 'b, [> typdef ] as 'c, 'd) #O.t as 'a) ->
    'e -> 'f -> 'g) ->
  (('b, 'c, 'd) #O.t as 'h, 'e) value ->
  ('a, 'f) value -> ('h, 'g) value
val check_value3 :
  (([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t, 'd)
  value ->
  (('a, 'b, 'c) #O.t, 'e) value ->
  (('a, 'b, 'c) #O.t, 'f) value -> unit
val mapterop :
  ('a -> 'b -> 'c -> 'd) ->
  (([> typ ] as 'f, [> typdef ] as 'g, 'h) #O.t as 'e,
  'a)
    value ->
  (('f, 'g, 'h) #O.t, 'b) value ->
  (('f, 'g, 'h) #O.t, 'c) value -> ('e, 'd) value
val check_lvarvalue :
  ([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t ->
  (string * (('a, 'b, 'c) #O.t, 'd) value) list ->
  (string * 'd) list
val check_lvalue :
  ([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t ->
  (('a, 'b, 'c) #O.t, 'd) value list -> 'd list
val check_ovalue :
  ([> typ ] as 'a, [> typdef ] as 'b, 'c) #O.t ->
  (('a, 'b, 'c) #O.t, 'd) value option -> 'd option
  
