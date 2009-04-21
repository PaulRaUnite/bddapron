(** *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  type ('a,'b) t = ('a,'b) Expr1.O.Bool.t
  constraint 'a = ('c,'d,'b) #Env.O.t

  let size t = Domain0.size t.val0
  let print fmt t = Domain0.O.print t.env fmt t.val0

  let bottom = Expr1.O.Bool.dfalse
  let top = Expr1.O.Bool.dtrue

  let is_bottom t = Domain0.O.is_bottom t.env t.val0
  let is_top t = Domain0.O.is_top t.env t.val0
  let is_leq = Expr1.O.Bool.is_leq
  let is_eq = Expr1.O.Bool.is_eq

  let is_variable_unconstrained t var =
    Domain0.O.is_variable_unconstrained t.env t.val0 var

  let meet = Expr1.O.Bool.dand
  let join = Expr1.O.Bool.dor

  let check_value permute t nenv =
    if Env.is_eq t.env nenv &&
      t.env#bddindex0 = nenv#bddindex0  then
      t.val0
    else if Env.is_leq t.env nenv &&
      t.env#bddindex0 = nenv#bddindex0 then
	permute t.val0 (Env.permutation12 t.env nenv)
    else
      failwith (Print.sprintf "Bdd.Domain1: the environment of the argument is not a subenvironment of the expected environment@. t.env=%a@.nenv=%a@." Expr1.O.print_env t.env Expr1.O.print_env nenv)
	
  let check_lvalue permute lt nenv =
    List.map (fun t -> check_value permute t nenv) lt

  let meet_condition t bexpr =
    let bexpr0 = check_value Expr0.O.Bool.permute bexpr t.env in
    make_value t.env (Expr0.O.Bool.dand t.env t.val0 bexpr0)

  let assign_lexpr ?relational ?nodependency (t:('a,'b) t) lvar lexpr =
    if lvar=[] && lexpr=[] then t
    else begin
      Expr1.O.check_lvar t.env lvar;
      let lexpr0 = check_lvalue Expr0.O.permute lexpr t.env in
      make_value t.env (Domain0.O.assign_lexpr ?relational ?nodependency t.env t.val0 lvar lexpr0)
    end

  let substitute_lexpr (t:('a,'b) t) lvar lexpr =
    if lvar=[] && lexpr=[] then t
    else begin
      Expr1.O.check_lvar t.env lvar;
      let lexpr0 = check_lvalue Expr0.O.permute lexpr t.env in
      make_value t.env (Domain0.O.substitute_lexpr t.env t.val0 lvar lexpr0)
    end
      
  let assign_listexpr ?relational ?nodependency (t:('a,'b) t) lvar lexpr =
    let lexpr0 = check_value Expr0.O.permute_list lexpr t.env in
    if lvar=[] && lexpr0=[] then t
    else begin
      Expr1.O.check_lvar t.env lvar;
      make_value t.env (Domain0.O.assign_lexpr ?relational ?nodependency t.env t.val0 lvar lexpr0)
    end

  let substitute_listexpr (t:('a,'b) t) lvar lexpr =
    let lexpr0 = check_value Expr0.O.permute_list lexpr t.env in
    if lvar=[] && lexpr.val0=[] then t
    else begin
      Expr1.O.check_lvar t.env lvar;
      make_value t.env (Domain0.O.substitute_lexpr t.env t.val0 lvar lexpr0)
    end

  let forget_list t lvar =
    Expr1.O.check_lvar t.env lvar;
    make_value t.env (Domain0.O.forget_list t.env t.val0 lvar)

  let rename t lvarvar =
    if lvarvar=[] then t else
      let nenv = Oo.copy t.env in
      let operm = nenv#rename_vars lvarvar in
      make_value nenv
	(match operm with
	| None -> t.val0
	| Some perm -> Cudd.Bdd.permute t.val0 perm
	)

  type 'a change_environment = {
    intro : int array option;
    remove : ('a Cudd.Bdd.t * int array) option;
  }

  let compute_change_environment env nenv =
    let lce = Env.lce env nenv in

    let intro =
      if Env.is_eq env lce
      then None
      else Some (Env.permutation12 env lce)
    in
    let remove =
      if Env.is_eq nenv lce
      then None
      else
	let setvar =
	  PSette.diff
	    (PMappe.maptoset lce#vartid)
	    (PMappe.maptoset nenv#vartid)
	in
	let supp = Expr0.O.bddsupport lce (PSette.elements setvar) in
	Some(supp, Env.permutation21 lce nenv)
    in
    { intro = intro; remove = remove }

  let apply_change_environment
      (value:'a Cudd.Bdd.t)
      (change_environment:'a change_environment)
      :
      'a Cudd.Bdd.t
      =
    let nvalue = match change_environment.intro with
      | None -> value
      | Some perm -> Cudd.Bdd.permute value perm
    in
    let nnvalue = match change_environment.remove with
      | None -> nvalue
      | Some(supp,perm) ->
	  let res = Cudd.Bdd.exist supp nvalue in
	  Cudd.Bdd.permute res perm
    in
    nnvalue

  let change_environment abs nenv =
    let change_environment = compute_change_environment abs.Env.env nenv in
    make_value
      nenv
      (apply_change_environment abs.Env.val0 change_environment)
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr1.Bool.t
type 'a lexpr = ('a Env.t, 'a Expr0.t list) Env.value

let size = O.size
let print = O.print
let bottom = O.bottom
let top = O.top
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let is_variable_unconstrained = O.is_variable_unconstrained
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let assign_lexpr = O.assign_lexpr
let substitute_lexpr = O.substitute_lexpr
let assign_listexpr = O.assign_listexpr
let substitute_listexpr = O.substitute_listexpr
let forget_list = O.forget_list
let change_environment = O.change_environment
let rename = O.rename
