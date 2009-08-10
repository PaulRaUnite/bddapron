(** Finite-type and arithmetical expressions linked to variable
    and condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O : sig

  (*  ==================================================================== *)
  (** {3 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool : sig
    type 'a t = ('a Cond.O.t, 'a Expr1.O.Bool.t) Bdd.Cond.value

    val of_expr :
      ('a Cond.O.t, ('a, [> `Bool of Expr0.Bool.t ]) Env.value) Bdd.Cond.value ->
      'a t
    val to_expr :
      'a t ->
      ('a Cond.O.t, ('a, [> `Bool of Expr0.Bool.t ]) Env.value) Bdd.Cond.value 
   val of_expr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'a -> 'a Cond.O.t -> Expr0.Bool.t -> 'a t
   val of_expr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'a Cond.O.t -> 'a Expr1.O.Bool.t -> 'a t

   val extend_environment : 'a t -> 'a -> 'a t
    
   val is_false : 'a t -> bool
   val is_true : 'a t -> bool

   val print : Format.formatter -> 'a t -> unit
  end

  (*  ==================================================================== *)
  (** {3 General expressions} *)
  (*  ==================================================================== *)

  type 'a t = ('a Cond.O.t, 'a Expr1.O.t) Bdd.Cond.value

  type 'a expr = 'a t

  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a -> 'a Cond.O.t -> Expr0.t -> 'a t
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Cond.O.t -> 'a Expr1.O.t -> 'a t
  val extend_environment : 'a t -> 'a -> 'a t  
  val print : Format.formatter -> 'a t -> unit

  (*  ==================================================================== *)
  (** {3 List of expressions} *)
  (*  ==================================================================== *)

  module List : sig
    type 'a t = ('a Cond.O.t, 'a Expr1.O.List.t) Bdd.Cond.value

   val of_lexpr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'a -> 'a Cond.O.t -> Expr0.t list -> 'a t
   val of_lexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'a -> 'a Cond.O.t -> 'a Expr1.O.t list -> 'a t

   val of_listexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'a Cond.O.t -> 'a Expr1.O.List.t -> 'a t

   val extend_environment : 'a t -> 'a -> 'a t
    
   val print : 
     ?first:(unit,Format.formatter,unit) format ->
     ?sep:(unit,Format.formatter,unit) format ->
     ?last:(unit,Format.formatter,unit) format ->
     Format.formatter -> 'a t -> unit
  end
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type t = (Cond.t, Expr1.t) Bdd.Cond.value
type expr = t

val of_expr0 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  Env.t -> Cond.t -> Expr0.t -> t
val of_expr1 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  Cond.t -> Expr1.t -> t
val extend_environment : t -> Env.t -> t
    
val print : Format.formatter -> t -> unit

module Bool : sig
  type t = (Cond.t, Expr1.Bool.t) Bdd.Cond.value

  val of_expr : expr -> t
  val to_expr : t -> expr
  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    Env.t -> Cond.t -> Expr0.Bool.t -> t
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    Cond.t -> Expr1.Bool.t -> t
  val extend_environment : t -> Env.t -> t

  val is_false : t -> bool
  val is_true : t -> bool

  val print : Format.formatter -> t -> unit
end

module List : sig
  type t = (Cond.t, Expr1.List.t) Bdd.Cond.value

  val of_lexpr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    Env.t -> Cond.t -> Expr0.t list -> t
  val of_lexpr1 : 
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    Env.t -> Cond.t -> Expr1.t list -> t
  val of_listexpr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    Cond.t -> Expr1.List.t -> t
  val extend_environment : t -> Env.t -> t
  val print :    
    ?first:(unit,Format.formatter,unit) format ->
    ?sep:(unit,Format.formatter,unit) format ->
    ?last:(unit,Format.formatter,unit) format ->
     Format.formatter -> t -> unit
end
