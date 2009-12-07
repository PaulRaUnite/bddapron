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
    type ('a,'b)  t = (('a,'b)  Cond.O.t, ('a,'b)  Expr1.O.Bool.t) Bdd.Cond.value

    val of_expr :
      (('a,'b) Cond.O.t, ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value) Bdd.Cond.value ->
      ('a,'b)  t
    val to_expr :
      ('a,'b)  t ->
      (('a,'b) Cond.O.t, ('b, [> `Bool of 'a Expr0.Bool.t ]) Env.value) Bdd.Cond.value
   val of_expr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> 'a Expr0.Bool.t -> ('a,'b) t
   val of_expr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.Bool.t -> ('a,'b) t

   val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

   val is_false : ('a,'b) t -> bool
   val is_true : ('a,'b) t -> bool

   val print : Format.formatter -> ('a,'b) t -> unit
  end

  (*  ==================================================================== *)
  (** {3 General expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.t) Bdd.Cond.value

  type ('a,'b) expr = ('a,'b) t

  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'b -> ('a,'b) Cond.O.t -> 'a Expr0.t -> ('a,'b) t
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.t -> ('a,'b) t
  val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t
  val print : Format.formatter -> ('a,'b) t -> unit

  (*  ==================================================================== *)
  (** {3 List of expressions} *)
  (*  ==================================================================== *)

  module List : sig
    type ('a,'b) t = (('a,'b) Cond.O.t, ('a,'b) Expr1.O.List.t) Bdd.Cond.value

   val of_lexpr0 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> 'a Expr0.t list -> ('a,'b) t
   val of_lexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     'b -> ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.t list -> ('a,'b) t

   val of_listexpr1 :
     ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
     ('a,'b) Cond.O.t -> ('a,'b) Expr1.O.List.t -> ('a,'b) t

   val extend_environment : ('a,'b) t -> 'b -> ('a,'b) t

   val print :
     ?first:(unit,Format.formatter,unit) format ->
     ?sep:(unit,Format.formatter,unit) format ->
     ?last:(unit,Format.formatter,unit) format ->
     Format.formatter -> ('a,'b) t -> unit
  end
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type 'a t = ('a Cond.t, 'a Expr1.t) Bdd.Cond.value
type 'a expr = 'a t

val of_expr0 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Env.t -> 'a Cond.t -> 'a Expr0.t -> 'a t
val of_expr1 :
  ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
  'a Cond.t -> 'a Expr1.t -> 'a t
val extend_environment : 'a t -> 'a Env.t -> 'a t

val print : Format.formatter -> 'a t -> unit

module Bool : sig
  type 'a t = ('a Cond.t, 'a Expr1.Bool.t) Bdd.Cond.value

  val of_expr : 'a expr -> 'a t
  val to_expr : 'a t -> 'a expr
  val of_expr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr0.Bool.t -> 'a t
  val of_expr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Cond.t -> 'a Expr1.Bool.t -> 'a t
  val extend_environment : 'a t -> 'a Env.t -> 'a t

  val is_false : 'a t -> bool
  val is_true : 'a t -> bool

  val print : Format.formatter -> 'a t -> unit
end

module List : sig
  type 'a t = ('a Cond.t, 'a Expr1.List.t) Bdd.Cond.value

  val of_lexpr0 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr0.t list -> 'a t
  val of_lexpr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Env.t -> 'a Cond.t -> 'a Expr1.t list -> 'a t
  val of_listexpr1 :
    ?normalize:bool -> ?reduce:bool -> ?careset:bool ->
    'a Cond.t -> 'a Expr1.List.t -> 'a t
  val extend_environment : 'a t -> 'a Env.t -> 'a t
  val print :
    ?first:(unit,Format.formatter,unit) format ->
    ?sep:(unit,Format.formatter,unit) format ->
    ?last:(unit,Format.formatter,unit) format ->
     Format.formatter -> 'a t -> unit
end
