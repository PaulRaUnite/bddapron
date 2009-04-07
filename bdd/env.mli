(** Normalized managers/environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

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
  class type ['a,'b,'c,'d] t = object('e)
    constraint 'a = [> typ]
    constraint 'b = [> typdef]
    constraint 'c = [> ]

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
    (** Remove the set of variables, possibly normalize the
	environment and return the applied permutation. *)
    method rename_vars : (string * string) list -> int array option
    (** Rename the variables, possibly normalize the environment
	and return the applied permutation. *)
    method mem_var : string -> bool
    (** Is the label/var defined in the database ? *)
    method typ_of_var : string -> 'a
    (** Return the type of the label/variable *)
    method vars : string PSette.t
    (** Return the list of variables *)

    method permutation : int array
    (** Compute the permutation fornormalizing the environment *)
    method permute : int array -> unit
    (** Apply the given permutation to the environment *)
    method normalize : int array
    (** Combine the two previous functions, and return the permutation *)

    method compute_careset : normalized:bool -> unit
    (** Computes the careset, using [self#compare_cond] *)
    method clear_cond : int array option
    (** Remove the condition of an environment *)
    method pair : unit
    (** Assuming [bddindex] is odd and [bddincr=2]), if [v_paired=false],
	groups variable by pair *)

    method add_var : string -> 'a -> unit
    (** Private function *)

    method cond_of_idb : int*bool -> 'c
    method idb_of_cond : 'c -> int*bool

    method print_idcondb : Format.formatter -> int*bool -> unit
    method print_order : Format.formatter -> unit
    (** Print the BDD variable ordering *)

    val v_cudd : 'd Cudd.Man.t
    (** CUDD manager *)
    val mutable v_paired : bool
    (** Are pair groups active (default false) ? *)
    val mutable v_typdef : (string, 'b) PMappe.t
    (** Named types definitions *)
    val mutable v_vartyp : (string, 'a) PMappe.t
    (** Associate to a var/label its type *)
    val mutable v_boolfirst : bool
    (** If true, when normalizing,
	Boolean variables are putabove conditions, otherwise below *)
    val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
    val mutable v_bddincr : int
    (** Increment used by [self#add_var] for incrementing
	[self#_v_bddindex] *)
    val mutable v_idcondvar : (int, string) PMappe.t
    (** Associates to a BDD index the variable involved by it *)
    val mutable v_vartid : (string, int array) PMappe.t
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
    val mutable v_varset : (string, 'd Cudd.Bdd.t) PMappe.t
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
    val v_compare_cond : 'c -> 'c -> int
    val v_negate_cond : ('a,'b,'c,'d) t -> 'c -> 'c
    val v_support_cond : ('a,'b,'c,'d) t -> 'c -> string PSette.t
    val mutable v_print_cond : ('a,'b,'c,'d) t -> Format.formatter -> 'c -> unit
    val mutable v_cond : ('c,int*bool) PDMappe.t
    (** Two-way association between a condition and a pair of a BDD index and a polarity *)
    val mutable v_cond_supp : 'd Cudd.Bdd.t
    (** Support of conditions *)
    val mutable v_careset : 'd Cudd.Bdd.t
    (** Boolean formula indicating which logical
	combination known as true could be exploited for simplification.
	For instance, [x>=1 => x>=0]. *)
    val mutable v_print_external_idcondb : Format.formatter -> int*bool -> unit
    (** Printing conditions not managed by [Bddvar].
	By default, [pp_print_int]. *)

    method cudd : 'd Cudd.Man.t
    method typdef : (string, 'b) PMappe.t
    method vartyp : (string, 'a) PMappe.t
    method boolfirst : bool
    method bddindex : int
    method bddincr : int
    method idcondvar : (int, string) PMappe.t
    method vartid : (string, int array) PMappe.t
    method varset : (string, 'd Cudd.Bdd.t) PMappe.t
    method compare_cond : 'c -> 'c -> int
    method negate_cond : 'c -> 'c
    method support_cond : 'c -> string PSette.t
    method print_cond : Format.formatter -> 'c -> unit
    method cond : ('c,int*bool) PDMappe.t
    method cond_supp : 'd Cudd.Bdd.t
    method careset : 'd Cudd.Bdd.t
    method print_external_idcondb : Format.formatter -> int*bool -> unit
    method set_bddindex : int -> unit
    method set_bddincr : int -> unit
    method set_boolfirst : bool -> unit
    method set_idcondvar : (int, string) PMappe.t -> unit
    method set_vartid : (string, int array) PMappe.t -> unit
    method set_varset : (string, 'd Cudd.Bdd.t) PMappe.t -> unit
    method set_typdef : (string, 'b) PMappe.t -> unit
    method set_vartyp : (string, 'a) PMappe.t -> unit
    method set_cond : ('c,int*bool) PDMappe.t -> unit
    method set_cond_supp : 'd Cudd.Bdd.t -> unit
    method set_careset : 'd Cudd.Bdd.t -> unit
    method set_print_external_idcondb :
      (Format.formatter -> int*bool -> unit) -> unit
  end

  class ['a,'b,'c,'d] make:
    'd Cudd.Man.t ->
    ?boolfirst:bool ->
    ?relational:bool ->
    compare_cond:('c -> 'c -> int) ->
    negate_cond:(('a,'b,'c,'d) t -> 'c -> 'c) ->
    support_cond:(('a,'b,'c,'d) t -> 'c -> string PSette.t) ->
    print_cond:(('a,'b,'c,'d) t -> Format.formatter -> 'c -> unit) ->
    ['a,'b,'c,'d] t
    (** Creates an environment from scratch *)

  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a,'b,'c,'d) #t -> unit
    (** Print an environment *)
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type ['a] t = [typ, typdef, [`Unit], 'a] O.t
class ['a] make :
  ?boolfirst:bool -> ?relational:bool ->
  'a Cudd.Man.t -> ['a] t

val make :
  ?boolfirst:bool -> ?relational:bool ->
  'a Cudd.Man.t -> 'a t
      (** Create a new database.
	  [bddindex] and [bddincr] initalized to 0 and 1. *)

(** {3 Functional version of some methods} *)

val add_typ : (('a,'b,'c,'d) #O.t as 'e) -> string -> 'b -> 'e
val add_vars : (('a,'b,'c,'d) #O.t as 'e) -> (string * 'a) list -> 'e
val remove_vars : (('a,'b,'c,'d) #O.t as 'e) -> string list -> 'e
val rename_vars : (('a,'b,'c,'d) #O.t as 'e) -> (string * string) list -> 'e

(** {3 Functions} *)

val is_leq : ('a,'b,'c,'d) #O.t -> ('a,'b,'c,'d) #O.t -> bool
    (** Test inclusion of environments *)
val is_eq : ('a,'b,'c,'d) #O.t -> ('a,'b,'c,'d) #O.t -> bool
    (** Test equality of environments *)
val lce : (('a,'b,'c,'d) #O.t as 'e) -> 'e -> 'e
    (** Least common environment *)
val lce2 :
  (('a,'b,'c,'d) #O.t as 'e) ->
  ('a, 'b, 'c, 'd) #O.t -> 'e
    (** Least common environment, special version *)
  
val compose_permutation : int array -> int array -> int array
val compose_opermutation : int array option -> int array option -> int array option
  
val check_normalized : ('a,'b,'c,'d) #O.t -> bool
    (** Prints error message and returns [false] if not normalized *)
val permutation12 : ('a,'b,'c,'d) #O.t -> ('a,'b,'c,'d) #O.t -> int array
    (** Permutation for going from a subenvironment to a superenvironment *)
val permutation21 : ('a,'b,'c,'d) #O.t -> ('a,'b,'c,'d) #O.t -> int array
    (** Permutation from a superenvironment to a subenvironment *)
  
val iter_ordered : ('a,'b,'c,'d) #O.t -> (string -> int array -> unit) -> unit
  (** Iter on all finite-state variables declared in the database *)
  
val compare_idb : int*bool -> int*bool -> int
  (** Comparison *)
  
val print_tid : Format.formatter -> int array -> unit
  
val print_typ : Format.formatter -> [>typ] -> unit
  (** Print a type *)
val print_typdef : Format.formatter -> [>typdef] -> unit
  (** Print a type definition *)
val print :
  Format.formatter -> ('a,'b,'c,'d) #O.t -> unit
    (** Print an environment *)

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Type of pairs [(environment, value)] *)
type ('a,'b) value = {
  env : 'a;
  value : 'b
}

val make_value : 'a -> 'b -> ('a,'b) value
  (** Constructor *)

val extend_environment :
  ('a -> int array -> 'a) ->
  (('b,'c,'d,'e) #O.t as 'env, 'a) value -> 'env -> ('env, 'a) value
    (** [extend_environment permute value env] embed [value] in the new
      (super)environment [env], by computing the permutation transformation and
      using [permute] to apply it to the value. *)
