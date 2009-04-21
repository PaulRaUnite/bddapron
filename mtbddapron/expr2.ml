(** Finite-type and arithmetical expressions paired with condition
    environment *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env
open Cond

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {3 General expressions} *)
  (*  ==================================================================== *)

  type ('a,'b) t = ('a, 'b Expr1.O.t) Cond.value
  constraint 'a = (Cond.cond,'b) #Cond.O.t

  type ('a,'b) expr = ('a,'b) t

  let print fmt (expr:('a,'b) t) =
    Expr1.O.print expr.Cond.cond fmt expr.Cond.val1

  let of_expr0
      ?(normalize=false) ?reduce ?careset
      (env:'b) (cond:'a) (expr0:Expr0.t)
      :
      ('a,'b) t
      =
    let (cond,expr0) =
      if normalize then
	let (cond,lexpr0) = Expr0.O.normalize ?reduce ?careset (cond,[expr0]) in
	(cond, List.hd lexpr0)
      else
	(cond,expr0)
    in
    Cond.make_value cond (Env.make_value env expr0)

  let of_expr1
      ?(normalize=false) ?reduce ?careset
      (cond:'a) (expr1:'b Expr1.O.t)
      :
      ('a,'b) t
      =
    if normalize then
      of_expr0 ~normalize ?reduce ?careset expr1.Env.env cond expr1.Env.val0
    else
      Cond.make_value cond expr1

  let extend_environment e nenv =
    Cond.make_value
      e.cond
      (Expr1.O.extend_environment e.val1 nenv)

  (*  ==================================================================== *)
  (** {3 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool = struct
    type ('a,'b) t = ('a, 'b Expr1.O.Bool.t) Cond.value
    constraint 'a = (Cond.cond,'b) #Cond.O.t

    let print fmt (expr:('a,'b) t) =
      Expr1.O.Bool.print expr.Cond.cond fmt expr.Cond.val1

    let of_expr e =
      let cond = e.cond and expr1 = e.val1 in
      Cond.make_value cond (Expr1.O.Bool.of_expr expr1)
    let to_expr e =
      let cond = e.cond and bexpr1 = e.val1 in
      Cond.make_value cond (Expr1.O.Bool.to_expr bexpr1)

    let of_expr0
	?normalize ?reduce ?careset
	(env:'b) (cond:'a) (bexpr0:Expr0.Bool.t)
	:
	('a,'b) t
	=
      let expr0 = ((Expr0.O.Bool.to_expr bexpr0):> Expr0.t) in
      let expr = of_expr0 ?normalize ?reduce ?careset env cond expr0 in
      of_expr expr

    let of_expr1
	?normalize ?reduce ?careset
	(cond:'a) (bexpr1:'b Expr1.O.Bool.t)
	:
	('a,'b) t
	=
      let expr1 = ((Expr1.O.Bool.to_expr bexpr1):> 'b Expr1.O.t) in
      let expr = of_expr1 ?normalize ?reduce ?careset cond expr1 in
      of_expr expr
    let extend_environment e nenv =
      Cond.make_value
	e.cond
	(Expr1.O.Bool.extend_environment e.val1 nenv)

    let is_false e = Expr1.O.Bool.is_false e.cond e.val1
    let is_true e = Expr1.O.Bool.is_true e.cond e.val1

  end

  (*  ==================================================================== *)
  (** {3 List of expressions} *)
  (*  ==================================================================== *)

  module List = struct
    type ('a,'b) t = ('a, 'b Expr1.O.List.t) Cond.value
    constraint 'a = (Cond.cond,'b) #Cond.O.t

    let print ?first ?sep ?last fmt (listexpr:('a,'b) t) =
      Expr1.O.List.print ?first ?sep ?last
	listexpr.Cond.cond fmt listexpr.Cond.val1

    let of_lexpr0
	?(normalize=false) ?reduce ?careset
	(env:'b) (cond:'a) (lexpr0:Expr0.t list)
	:
	('a,'b) t
	=
      let (cond,lexpr0) =
	if normalize
	then Expr0.O.normalize ?reduce ?careset (cond,lexpr0)
	else (cond,lexpr0)
      in
      Cond.make_value cond (Env.make_value env lexpr0)

    let of_listexpr1
	?(normalize=false) ?reduce ?careset
	(cond:'a) (listexpr1:'b Expr1.O.List.t)
	:
	('a,'b) t
	=
      if normalize then
	of_lexpr0 ~normalize ?reduce ?careset listexpr1.Env.env cond listexpr1.Env.val0
      else
	Cond.make_value cond listexpr1

    let of_lexpr1
	?normalize ?reduce ?careset
	(env:'b) (cond:'a) (lexpr1:'b Expr1.O.t list)
	:
	('a,'b) t
	=
      let listexpr1 = Expr1.O.List.of_lexpr1 env lexpr1 in
      of_listexpr1 ?normalize ?reduce ?careset cond listexpr1

    let extend_environment e nenv =
      Cond.make_value
	e.cond
	(Expr1.O.List.extend_environment e.val1 nenv)

  end
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type t = (Cond.t, Expr1.t) Cond.value
type expr = t

let of_expr0 = O.of_expr0
let of_expr1 = O.of_expr1
let extend_environment = O.extend_environment
let print = O.print

module Bool = struct

  type t = (Cond.t, Expr1.Bool.t) Cond.value
  let of_expr = O.Bool.of_expr
  let to_expr = O.Bool.to_expr
  let of_expr0 = O.Bool.of_expr0
  let of_expr1 = O.Bool.of_expr1
  let extend_environment = O.Bool.extend_environment
  let is_false = O.Bool.is_false
  let is_true = O.Bool.is_true
  let print = O.Bool.print
end

module List = struct
  type t = (Cond.t, Expr1.List.t) Cond.value
  let of_lexpr0 = O.List.of_lexpr0
  let of_lexpr1 = O.List.of_lexpr1
  let of_listexpr1 = O.List.of_listexpr1
  let extend_environment = O.List.extend_environment
  let print = O.List.print
end
