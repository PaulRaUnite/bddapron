(** Finite-type and arithmetical expressions linked to normalized environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bddenv

let print_typ = Bddapronexpr.print_typ
let print_typdef = Bddapronexpr.print_typdef

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

(*  ====================================================================== *)
(** {3 Environments} *)
(*  ====================================================================== *)

type ('a,'b,'c) env = ('a,'b,'c) Bddapronexpr.O.env

let make_env = Bddapronexpr.O.make_env
let add_typ = Bddapronexpr.O.add_typ
let add_vars = Bddapronexpr.O.add_vars
let remove_vars = Bddapronexpr.O.remove_vars
let print_cond = Bddapronexpr.O.print_cond
let print_env = Bddapronexpr.O.print_env

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

type 'a expr = ('a, Bddapronexpr.expr) Bddenv.value
    constraint 'a = ('b,'c,'d) #Bddapronexpr.O.env

let make permute env e =
  let perm = Bddenv.permutation env in
  Bddenv.permute env perm;
  make_value env (permute e perm)

let substitute (e:'a expr) (substitution:(string * 'a expr) list) : 'a expr
  =
  let (nenv,e,lvarexpr) =
    BddexprE.O.check_lvarvalue Bddapronexpr.permute Bddapronexpr.permute
      e substitution
  in
  make_value nenv (Bddapronexpr.O.substitute nenv e lvarexpr)

let var env (var:string) : 'a expr
  =
  make_value env (Bddapronexpr.O.var env var)

let substitute_by_var e (substitution:(string*string) list) =
  let nenv = Oo.copy e.env in
  let res = Bddapronexpr.O.substitute_by_var nenv e.value substitution in
  let env = if Bddenv.is_eq nenv e.env then e.env else nenv in
  make_value env res

let ddsubstitute = substitute
let ddsubstitute_by_var = substitute_by_var

(*  ====================================================================== *)
(** {3 Bdd expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type 'a t = ('a, Bdd.t) Bddenv.value
    constraint 'a = ('b,'c,'d) #Bddapronexpr.O.env

  let of_expr = BddexprE.O.Bool.of_expr
  let to_expr = BddexprE.O.Bool.to_expr
  let extend_environment = BddexprE.O.Bool.extend_environment
  let dtrue = BddexprE.O.Bool.dtrue
  let dfalse = BddexprE.O.Bool.dfalse
  let of_bool = BddexprE.O.Bool.of_bool
  let var = BddexprE.O.Bool.var
  let dnot = BddexprE.O.Bool.dnot
  let dand = BddexprE.O.Bool.dand
  let dor = BddexprE.O.Bool.dor
  let xor = BddexprE.O.Bool.xor
  let nand = BddexprE.O.Bool.nand
  let nor = BddexprE.O.Bool.nor
  let nxor = BddexprE.O.Bool.nxor
  let eq = BddexprE.O.Bool.eq
  let leq = BddexprE.O.Bool.leq
  let ite = BddexprE.O.Bool.ite
  let is_true = BddexprE.O.Bool.is_true
  let is_false = BddexprE.O.Bool.is_false
  let is_cst = BddexprE.O.Bool.is_cst
  let is_eq = BddexprE.O.Bool.is_eq
  let is_leq = BddexprE.O.Bool.is_leq
  let is_inter_false = BddexprE.O.Bool.is_inter_false
  let exist = BddexprE.O.Bool.exist
  let forall = BddexprE.O.Bool.forall
  let cofactor = BddexprE.O.Bool.cofactor
  let restrict = BddexprE.O.Bool.restrict
  let tdrestrict = BddexprE.O.Bool.tdrestrict
  let print = BddexprE.O.Bool.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end
module Bint = struct
  type 'a t = ('a, Bddint.t) Bddenv.value
    constraint 'a = ('b,'c,'d) #Bddapronexpr.O.env

  let of_expr = BddexprE.O.Bint.of_expr
  let to_expr = BddexprE.O.Bint.to_expr
  let extend_environment = BddexprE.O.Bint.extend_environment

  let of_int = BddexprE.O.Bint.of_int
  let var = BddexprE.O.Bint.var
  let neg = BddexprE.O.Bint.neg
  let succ = BddexprE.O.Bint.succ
  let pred = BddexprE.O.Bint.pred
  let add = BddexprE.O.Bint.add
  let sub = BddexprE.O.Bint.sub
  let shift_left = BddexprE.O.Bint.shift_left
  let shift_right = BddexprE.O.Bint.shift_right
  let scale = BddexprE.O.Bint.scale
  let ite = BddexprE.O.Bint.ite

  let zero = BddexprE.O.Bint.zero
  let eq = BddexprE.O.Bint.eq
  let eq_int = BddexprE.O.Bint.eq_int
  let supeq = BddexprE.O.Bint.supeq
  let supeq_int = BddexprE.O.Bint.supeq_int
  let sup = BddexprE.O.Bint.sup

  let cofactor = BddexprE.O.Bint.cofactor
  let restrict = BddexprE.O.Bint.restrict
  let tdrestrict = BddexprE.O.Bint.tdrestrict

  let guard_of_int = BddexprE.O.Bint.guard_of_int
  let guardints = BddexprE.O.Bint.guardints

  let print = BddexprE.O.Bint.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)

end
module Benum = struct
  type 'a t = ('a, Bddenum.t) Bddenv.value
    constraint 'a = ('b,'c,'d) #Bddapronexpr.O.env
  let of_expr = BddexprE.O.Benum.of_expr
  let to_expr = BddexprE.O.Benum.to_expr
  let extend_environment = BddexprE.O.Benum.extend_environment

  let var = BddexprE.O.Benum.var
  let ite = BddexprE.O.Benum.ite
  let eq = BddexprE.O.Benum.eq
  let eq_label = BddexprE.O.Benum.eq_label
  let cofactor = BddexprE.O.Benum.cofactor
  let restrict = BddexprE.O.Benum.restrict
  let tdrestrict = BddexprE.O.Benum.tdrestrict
  let guard_of_label = BddexprE.O.Benum.guard_of_label
  let guardlabels = BddexprE.O.Benum.guardlabels
  let print = BddexprE.O.Benum.print
  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 Arith expressions} *)
(*  ====================================================================== *)

module Apron = struct
  type 'a t = ('a, ApronexprDD.expr) Bddenv.value
    constraint 'a = ('b,'c,'d) #Bddapronexpr.O.env

  let of_expr e : 'a t =
    match e.value with
    | `Apron x -> make_value e.env x
    | _ -> failwith "Apron.of_expr: arithmetic expression expected"

  let to_expr (e:'a t) =
    make_value e.env (`Apron e.value)

  let extend_environment e nenv =
    if Bddenv.is_eq e.env nenv then
      e
    else begin
      if not (Bddenv.is_leq e.env nenv) then
	failwith "Apron.extend_environment: the given environment is not a superenvironment "
      ;
      let perm = Bddenv.permutation12 e.env nenv in
      make_value nenv (ApronexprDD.permute e.value perm)
    end

  let var env name = make_value env (Bddapronexpr.O.Apron.var env name)
  let cst env cst = make_value env (Bddapronexpr.O.Apron.cst env cst)
  let add ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    BddexprE.O.mapbinop ApronexprDD.permute ApronexprDD.permute
      (ApronexprDD.add ~typ ~round) e1 e2
  let mul ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    BddexprE.O.mapbinop ApronexprDD.permute ApronexprDD.permute
      (ApronexprDD.mul ~typ ~round) e1 e2
  let sub ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    BddexprE.O.mapbinop ApronexprDD.permute ApronexprDD.permute
      (ApronexprDD.sub ~typ ~round) e1 e2
  let div ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    BddexprE.O.mapbinop ApronexprDD.permute ApronexprDD.permute
      (ApronexprDD.div ~typ ~round) e1 e2
  let gmod ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
    BddexprE.O.mapbinop ApronexprDD.permute ApronexprDD.permute
      (ApronexprDD.gmod ~typ ~round) e1 e2
  let negate e = BddexprE.O.mapunop ApronexprDD.negate e
  let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
    BddexprE.O.mapunop (ApronexprDD.sqrt ~typ ~round) e
  let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
    BddexprE.O.mapunop (ApronexprDD.cast ~typ ~round) e

  let ite e1 e2 e3 =
    let (nenv,value1,value2,value3) =
      BddexprE.O.check_value3 Bdd.permute ApronexprDD.permute ApronexprDD.permute e1 e2 e3
    in
    make_value nenv (ApronexprDD.ite value1 value2 value3)

  let condition typ e =
    let env = Oo.copy e.env in
    let res = ApronexprDD.Condition.make env typ e.value in
    if Bddenv.is_eq env e.env then
      make_value e.env res
    else
      make Bddapronexpr.Bool.permute env res

  let supeq expr = condition Apron.Tcons1.SUPEQ expr
  let sup expr = condition Apron.Tcons1.SUP expr
  let eq expr = condition Apron.Tcons1.EQ expr

  let cofactor e1 e2 = BddexprE.O.mapbinop ApronexprDD.permute Bdd.permute ApronexprDD.cofactor e1 e2
  let restrict e1 e2 = BddexprE.O.mapbinop ApronexprDD.permute Bdd.permute ApronexprDD.restrict e1 e2
  let tdrestrict e1 e2 = BddexprE.O.mapbinop ApronexprDD.permute Bdd.permute ApronexprDD.tdrestrict e1 e2

  let print fmt (x:'a t) =
    ApronexprDD.print (Bddexpr.O.print_bdd x.env) fmt x.value

  let substitute_by_var e sub =
    of_expr (ddsubstitute_by_var (to_expr e) sub)
  let substitute e sub =
    of_expr (ddsubstitute (to_expr e) sub)
end

(*  ********************************************************************** *)
(** {2 General expressions} *)
(*  ********************************************************************** *)

let typ_of_expr e = Bddapronexpr.typ_of_expr e.value

let permute (e:Bddapronexpr.expr) (tab:int array) : Bddapronexpr.expr = match e with
  | #Bddexpr.expr as e ->
      let res = Bddexpr.O.permute e tab in
      (res:>Bddapronexpr.expr)
  | `Apron e -> `Apron (ApronexprDD.permute e tab)

let extend_environment abs nenv =
  if Bddenv.is_eq abs.env nenv then
    abs
  else begin
    if not (Bddenv.is_leq abs.env nenv) then
      failwith "BddapronexprE.O.extend_environment: the given environment is not a superenvironment"
    ;
    let perm = Bddenv.permutation12 abs.env nenv in
    make_value nenv (permute abs.value perm)
  end

let ite e1 e2 e3 =
  let (nenv,value1,value2,value3) =
    BddexprE.O.check_value3 Bdd.permute permute permute e1 e2 e3
  in
  make_value nenv (Bddapronexpr.O.ite nenv value1 value2 value3)

let cofactor e1 e2 = BddexprE.O.mapbinop permute Bdd.permute Bddapronexpr.cofactor e1 e2
let restrict e1 e2 = BddexprE.O.mapbinop permute Bdd.permute Bddapronexpr.restrict e1 e2
let tdrestrict e1 e2 = BddexprE.O.mapbinop permute Bdd.permute Bddapronexpr.tdrestrict e1 e2

let eq e1 e2 =
  BddexprE.O.mapbinope permute permute Bddapronexpr.O.eq e1 e2

let support (e:'a expr) = Bddapronexpr.O.support e.env e.value
let support_cond (e:'a expr) = Bddapronexpr.O.support_cond e.env e.value
let vectorsupport_cond (te:'a expr array) =
  Bddapronexpr.O.vectorsupport_cond te.(0).env
    (Array.map (fun e -> e.value) te)

let print_expr fmt (e:'a expr) : unit =
  Bddapronexpr.O.print_expr e.env fmt e.value

let make env x = make Bddapronexpr.permute env x
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)
type env = Bddapronexpr.env
let make_env =  Bddapronexpr.make_env
let add_typ = O.add_typ
let add_vars = O.add_vars
let remove_vars = O.remove_vars
let print_cond = O.print_cond
let print_env = O.print_env

(*  ====================================================================== *)
(** {3 Operations on general expressions} *)
(*  ====================================================================== *)

type expr = (Bddapronexpr.env, Bddapronexpr.expr) Bddenv.value
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
let vectorsupport_cond = O.vectorsupport_cond
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let print_expr = O.print_expr

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type t = (Bddapronexpr.env, Bdd.t) Bddenv.value
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
  type t = (Bddapronexpr.env, Bddint.t) Bddenv.value
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
  type t = (Bddapronexpr.env, Bddenum.t) Bddenv.value
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
  type t = (Bddapronexpr.env, ApronexprDD.expr) Bddenv.value
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
