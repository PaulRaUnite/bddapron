(** Finite-type and arithmetical expressions linked to normalized environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env


(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

let make_env = Env.O.make

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

type 'a t = ('a, Expr0.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t
type 'a expr = 'a t

let make permute env e =
  let perm = env#normalize in
  make_value env (permute e perm)

let substitute (e:'a t) (substitution:(string * 'a t) list) : 'a t
  =
  let (nenv,e,lvarexpr) =
    Bdd.Expr1.O.check_lvarvalue Expr0.permute Expr0.permute
      e substitution
  in
  make_value nenv (Expr0.O.substitute nenv e lvarexpr)

let var env (var:string) : 'a t
  =
  make_value env (Expr0.O.var env var)

let substitute_by_var e (substitution:(string*string) list) =
  let nenv = Oo.copy e.env in
  let res = Expr0.O.substitute_by_var nenv e.value substitution in
  let env = if Bdd.Env.is_eq nenv e.env then e.env else nenv in
  make_value env res

let ddsubstitute = substitute
let ddsubstitute_by_var = substitute_by_var

(*  ====================================================================== *)
(** {3 Bdd expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type 'a t = ('a, Cudd.Man.v Cudd.Bdd.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  let of_expr = Bdd.Expr1.O.Bool.of_expr
  let to_expr = Bdd.Expr1.O.Bool.to_expr
  let extend_environment = Bdd.Expr1.O.Bool.extend_environment
  let dtrue = Bdd.Expr1.O.Bool.dtrue
  let dfalse = Bdd.Expr1.O.Bool.dfalse
  let of_bool = Bdd.Expr1.O.Bool.of_bool
  let var = Bdd.Expr1.O.Bool.var
  let dnot = Bdd.Expr1.O.Bool.dnot
  let dand = Bdd.Expr1.O.Bool.dand
  let dor = Bdd.Expr1.O.Bool.dor
  let xor = Bdd.Expr1.O.Bool.xor
  let nand = Bdd.Expr1.O.Bool.nand
  let nor = Bdd.Expr1.O.Bool.nor
  let nxor = Bdd.Expr1.O.Bool.nxor
  let eq = Bdd.Expr1.O.Bool.eq
  let leq = Bdd.Expr1.O.Bool.leq
  let ite = Bdd.Expr1.O.Bool.ite
  let is_true = Bdd.Expr1.O.Bool.is_true
  let is_false = Bdd.Expr1.O.Bool.is_false
  let is_cst = Bdd.Expr1.O.Bool.is_cst
  let is_eq = Bdd.Expr1.O.Bool.is_eq
  let is_leq = Bdd.Expr1.O.Bool.is_leq
  let is_inter_false = Bdd.Expr1.O.Bool.is_inter_false
  let exist = Bdd.Expr1.O.Bool.exist
  let forall = Bdd.Expr1.O.Bool.forall
  let cofactor = Bdd.Expr1.O.Bool.cofactor
  let restrict = Bdd.Expr1.O.Bool.restrict
  let tdrestrict = Bdd.Expr1.O.Bool.tdrestrict
  let print = Bdd.Expr1.O.Bool.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end
module Bint = struct
  type 'a t = ('a, Cudd.Man.v Bdd.Int.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  let of_expr = Bdd.Expr1.O.Bint.of_expr
  let to_expr = Bdd.Expr1.O.Bint.to_expr
  let extend_environment = Bdd.Expr1.O.Bint.extend_environment

  let of_int = Bdd.Expr1.O.Bint.of_int
  let var = Bdd.Expr1.O.Bint.var
  let neg = Bdd.Expr1.O.Bint.neg
  let succ = Bdd.Expr1.O.Bint.succ
  let pred = Bdd.Expr1.O.Bint.pred
  let add = Bdd.Expr1.O.Bint.add
  let sub = Bdd.Expr1.O.Bint.sub
  let mul = Bdd.Expr1.O.Bint.mul
  let shift_left = Bdd.Expr1.O.Bint.shift_left
  let shift_right = Bdd.Expr1.O.Bint.shift_right
  let scale = Bdd.Expr1.O.Bint.scale
  let ite = Bdd.Expr1.O.Bint.ite

  let zero = Bdd.Expr1.O.Bint.zero
  let eq = Bdd.Expr1.O.Bint.eq
  let eq_int = Bdd.Expr1.O.Bint.eq_int
  let supeq = Bdd.Expr1.O.Bint.supeq
  let supeq_int = Bdd.Expr1.O.Bint.supeq_int
  let sup = Bdd.Expr1.O.Bint.sup

  let cofactor = Bdd.Expr1.O.Bint.cofactor
  let restrict = Bdd.Expr1.O.Bint.restrict
  let tdrestrict = Bdd.Expr1.O.Bint.tdrestrict

  let guard_of_int = Bdd.Expr1.O.Bint.guard_of_int
  let guardints = Bdd.Expr1.O.Bint.guardints

  let print = Bdd.Expr1.O.Bint.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)

end
module Benum = struct
  type 'a t = ('a, Cudd.Man.v Bdd.Enum.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t
  let of_expr = Bdd.Expr1.O.Benum.of_expr
  let to_expr = Bdd.Expr1.O.Benum.to_expr
  let extend_environment = Bdd.Expr1.O.Benum.extend_environment

  let var = Bdd.Expr1.O.Benum.var
  let ite = Bdd.Expr1.O.Benum.ite
  let eq = Bdd.Expr1.O.Benum.eq
  let eq_label = Bdd.Expr1.O.Benum.eq_label
  let cofactor = Bdd.Expr1.O.Benum.cofactor
  let restrict = Bdd.Expr1.O.Benum.restrict
  let tdrestrict = Bdd.Expr1.O.Benum.tdrestrict
  let guard_of_label = Bdd.Expr1.O.Benum.guard_of_label
  let guardlabels = Bdd.Expr1.O.Benum.guardlabels
  let print = Bdd.Expr1.O.Benum.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 Arith expressions} *)
(*  ====================================================================== *)

module Apron = struct
  type 'a t = ('a, ApronexprDD.t) Bdd.Env.value
    constraint 'a = ('b,'c,'d) #Env.O.t

  let of_expr e : 'a t =
    match e.value with
    | `Apron x -> make_value e.env x
    | _ -> failwith "Apron.of_expr: arithmetic expression expected"

  let to_expr (e:'a t) =
    make_value e.env (`Apron e.value)

  let extend_environment e nenv =
    if Bdd.Env.is_eq e.env nenv then
      e
    else begin
      if not (Bdd.Env.is_leq e.env nenv) then
	failwith "Apron.extend_environment: the given environment is not a superenvironment "
      ;
      let perm = Bdd.Env.permutation12 e.env nenv in
      make_value nenv (Cudd.Mtbdd.permute e.value perm)
    end

  let var env name = make_value env (Expr0.O.Apron.var env name)
  let cst env cst = make_value env (Expr0.O.Apron.cst env cst)
  let add ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (ApronexprDD.add ~typ ~round) e1 e2
  let mul ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (ApronexprDD.mul ~typ ~round) e1 e2
  let sub ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (ApronexprDD.sub ~typ ~round) e1 e2
  let div ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (ApronexprDD.div ~typ ~round) e1 e2
  let gmod ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (ApronexprDD.gmod ~typ ~round) e1 e2
  let negate e = Bdd.Expr1.O.mapunop ApronexprDD.negate e
  let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
    Bdd.Expr1.O.mapunop (ApronexprDD.sqrt ~typ ~round) e
  let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
    Bdd.Expr1.O.mapunop (ApronexprDD.cast ~typ ~round) e

  let ite e1 e2 e3 =
    let (nenv,value1,value2,value3) =
      Bdd.Expr1.O.check_value3 Cudd.Bdd.permute Cudd.Mtbdd.permute Cudd.Mtbdd.permute e1 e2 e3
    in
    make_value nenv (Cudd.Mtbdd.ite value1 value2 value3)

  let condition typ e =
    let env = Oo.copy e.env in
    let res = ApronexprDD.Condition.make env typ e.value in
    if Bdd.Env.is_eq env e.env then
      make_value e.env res
    else
      make Expr0.Bool.permute env res

  let supeq expr = condition Apron.Tcons1.SUPEQ expr
  let sup expr = condition Apron.Tcons1.SUP expr
  let eq expr = 
    condition Apron.Tcons1.EQ expr

  let cofactor e1 e2 = Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Bdd.permute Cudd.Mtbdd.cofactor e1 e2
  let restrict e1 e2 = Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Bdd.permute Cudd.Mtbdd.restrict e1 e2
  let tdrestrict e1 e2 = Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Bdd.permute Cudd.Mtbdd.tdrestrict e1 e2

  let print fmt (x:'a t) =
    ApronexprDD.print (Bdd.Expr0.O.print_bdd x.env) fmt x.value

  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end

(*  ********************************************************************** *)
(** {2 General expressions} *)
(*  ********************************************************************** *)

let typ_of_expr e = Expr0.typ_of_expr e.value

let permute (e:Expr0.t) (tab:int array) : Expr0.t = match e with
  | #Bdd.Expr0.t as e ->
      let res = Bdd.Expr0.O.permute e tab in
      (res:>Expr0.t)
  | `Apron e -> `Apron (Cudd.Mtbdd.permute e tab)

let extend_environment abs nenv =
  if Bdd.Env.is_eq abs.env nenv then
    abs
  else begin
    if not (Bdd.Env.is_leq abs.env nenv) then
      failwith "BddapronexprE.O.extend_environment: the given environment is not a superenvironment"
    ;
    let perm = Bdd.Env.permutation12 abs.env nenv in
    make_value nenv (permute abs.value perm)
  end

let ite e1 e2 e3 =
  let (nenv,value1,value2,value3) =
    Bdd.Expr1.O.check_value3 Cudd.Bdd.permute permute permute e1 e2 e3
  in
  make_value nenv (Expr0.O.ite nenv value1 value2 value3)

let cofactor e1 e2 = Bdd.Expr1.O.mapbinop permute Cudd.Bdd.permute Expr0.cofactor e1 e2
let restrict e1 e2 = Bdd.Expr1.O.mapbinop permute Cudd.Bdd.permute Expr0.restrict e1 e2
let tdrestrict e1 e2 = Bdd.Expr1.O.mapbinop permute Cudd.Bdd.permute Expr0.tdrestrict e1 e2

let eq e1 e2 =
  let t = Expr0.O.check_typ2 e1.value e2.value in
  match t with
  | `Bool ->
      Bool.eq (Bool.of_expr e1) (Bool.of_expr e2)
  | `Bint _ ->
      Bint.eq (Bint.of_expr e1) (Bint.of_expr e2)
  | `Benum _ ->
      Benum.eq (Benum.of_expr e1) (Benum.of_expr e2)
  | `Real ->
      let diff = Apron.sub
	(Apron.of_expr e1)
	(Apron.of_expr e2)
      in
      Apron.eq diff
  | _ -> failwith ""

let support (e:'a t) = Expr0.O.support e.env e.value
let support_cond (e:'a t) = Expr0.O.support_cond e.env e.value

let print fmt (e:'a t) : unit =
  Expr0.O.print e.env fmt e.value

let make env x = make Expr0.permute env x
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

let make_env = Env.make

(*  ====================================================================== *)
(** {3 Operations on general expressions} *)
(*  ====================================================================== *)

type t = (Env.t, Expr0.t) Bdd.Env.value
type expr = t
let typ_of_expr = O.typ_of_expr
let make = O.make
let extend_environment = O.extend_environment
let var = O.var
let ite = O.ite
let eq = O.eq
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let support = O.support
let support_cond = O.support_cond
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let print = O.print

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type t = (Env.t, Cudd.Man.v Cudd.Bdd.t) Bdd.Env.value
  let of_expr = O.Bool.of_expr
  let to_expr = O.Bool.to_expr
  let extend_environment = O.Bool.extend_environment
  let dtrue = O.Bool.dtrue
  let dfalse = O.Bool.dfalse
  let of_bool = O.Bool.of_bool
  let var = O.Bool.var
  let dnot = O.Bool.dnot
  let dand = O.Bool.dand
  let dor = O.Bool.dor
  let xor = O.Bool.xor
  let nand = O.Bool.nand
  let nor = O.Bool.nor
  let nxor = O.Bool.nxor
  let eq = O.Bool.eq
  let leq = O.Bool.leq
  let ite = O.Bool.ite
  let is_true = O.Bool.is_true
  let is_false = O.Bool.is_false
  let is_cst = O.Bool.is_cst
  let is_eq = O.Bool.is_eq
  let is_leq = O.Bool.is_leq
  let is_inter_false = O.Bool.is_inter_false
  let exist = O.Bool.exist
  let forall = O.Bool.forall
  let cofactor = O.Bool.cofactor
  let restrict = O.Bool.restrict
  let tdrestrict = O.Bool.tdrestrict
  let substitute_by_var = O.Bool.substitute_by_var
  let substitute = O.Bool.substitute
  let print = O.Bool.print
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint = struct
  type t = (Env.t, Cudd.Man.v Bdd.Int.t) Bdd.Env.value
  let of_expr = O.Bint.of_expr
  let to_expr = O.Bint.to_expr
  let extend_environment = O.Bint.extend_environment
  let of_int = O.Bint.of_int
  let var = O.Bint.var
  let neg = O.Bint.neg
  let succ = O.Bint.succ
  let pred = O.Bint.pred
  let add = O.Bint.add
  let sub = O.Bint.sub
  let mul = O.Bint.mul
  let shift_left = O.Bint.shift_left
  let shift_right = O.Bint.shift_right
  let scale = O.Bint.scale
  let ite = O.Bint.ite
  let zero = O.Bint.zero
  let eq = O.Bint.eq
  let supeq = O.Bint.supeq
  let sup = O.Bint.sup
  let eq_int = O.Bint.eq_int
  let supeq_int = O.Bint.supeq_int
  let cofactor = O.Bint.cofactor
  let restrict = O.Bint.restrict
  let tdrestrict = O.Bint.tdrestrict
  let substitute_by_var = O.Bint.substitute_by_var
  let substitute = O.Bint.substitute
  let guard_of_int= O.Bint.guard_of_int
  let guardints= O.Bint.guardints
  let print = O.Bint.print
end

(*  ====================================================================== *)
(** {3 Enumerated expressions} *)
(*  ====================================================================== *)

module Benum = struct
  type t = (Env.t, Cudd.Man.v Bdd.Enum.t) Bdd.Env.value
  let of_expr = O.Benum.of_expr
  let to_expr = O.Benum.to_expr
  let extend_environment = O.Benum.extend_environment
  let var = O.Benum.var
  let ite = O.Benum.ite
  let eq = O.Benum.eq
  let eq_label = O.Benum.eq_label
  let cofactor = O.Benum.cofactor
  let restrict = O.Benum.restrict
  let tdrestrict = O.Benum.tdrestrict
  let substitute_by_var = O.Benum.substitute_by_var
  let substitute = O.Benum.substitute
  let guard_of_label = O.Benum.guard_of_label
  let guardlabels = O.Benum.guardlabels
  let print = O.Benum.print
end

(*  ====================================================================== *)
(** {3 Arithmetic expressions} *)
(*  ====================================================================== *)
module Apron = struct
  type t = (Env.t, ApronexprDD.t) Bdd.Env.value
  let of_expr = O.Apron.of_expr
  let to_expr = O.Apron.to_expr
  let extend_environment = O.Apron.extend_environment
  let var = O.Apron.var
  let cst = O.Apron.cst
  let add = O.Apron.add
  let mul = O.Apron.mul
  let sub = O.Apron.sub
  let div = O.Apron.div
  let gmod = O.Apron.gmod
  let negate = O.Apron.negate
  let sqrt = O.Apron.sqrt
  let cast = O.Apron.cast
  let ite = O.Apron.ite
  let condition = O.Apron.condition
  let supeq = O.Apron.supeq
  let sup = O.Apron.sup
  let eq = O.Apron.eq
  let cofactor = O.Apron.cofactor
  let restrict = O.Apron.restrict
  let tdrestrict = O.Apron.tdrestrict
  let substitute_by_var = O.Apron.substitute_by_var
  let substitute = O.Apron.substitute
  let print = O.Apron.print
end
