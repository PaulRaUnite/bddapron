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
type ('a,'b) expr = ('a,'b) Expr1.O.expr

let check2 t1 t2 =
  if not (MappeS.equal (=) t1.env#vartyp t2.env#vartyp) then begin
    if true then
      printf "env1=%a@.env2=%a@."
	(MappeS.print pp_print_string Env.print_typ) t1.env#vartyp
	(MappeS.print pp_print_string Env.print_typ) t2.env#vartyp
    ;
    failwith "The two abstract values are not defined on the same environment"
  end
  ;
  ()
let check_expr (permute:'a -> int array -> 'a) env expr =
  if not (MappeS.subset (=) env#vartyp expr.env#vartyp) then
    failwith "The expression is not defined on a subenvironment of the value"
  ;
  permute expr.value (Env.permutation12 expr.env env)

let size t = Domain0.size t.value
let print fmt t = Domain0.O.print t.env fmt t.value

let bottom = Expr1.O.Bool.dfalse
let top = Expr1.O.Bool.dtrue

let is_bottom t = Domain0.O.is_bottom t.env t.value
let is_top t = Domain0.O.is_top t.env t.value
let is_leq t1 t2 = check2 t1 t2; Expr1.O.Bool.is_leq t1 t2
let is_eq t1 t2 = check2 t1 t2; Expr1.O.Bool.is_eq t1 t2

let is_variable_unconstrained t var =
  Domain0.O.is_variable_unconstrained t.env t.value var

let meet t1 t2 = check2 t1 t2; Expr1.O.Bool.dand t1 t2
let join t1 t2 = check2 t1 t2; Expr1.O.Bool.dor t1 t2
let meet_cond = meet

let assign_list ?relational ?nodependency (t:('a,'b) t) (lvarexpr:(string * ('a,'b) Expr1.O.expr) list) =
  if lvarexpr=[] then t
  else begin
    let lvarexpr = List.map
      (fun (var,expr) ->
	Expr1.O.check_envvar t.env var;
	let expr = check_expr Expr0.O.permute t.env expr in
	(var,expr)
      )
      lvarexpr
    in
    make_value t.env (Domain0.O.assign_list ?relational ?nodependency t.env t.value lvarexpr)
  end

let substitute_list (t:('a,'b) t) (lvarexpr:(string * ('a,'b) Expr1.O.expr) list) =
  if lvarexpr=[] then t
  else begin
    let lvarexpr = List.map
      (fun (var,expr) ->
	Expr1.O.check_envvar t.env var;
	let expr = check_expr Expr0.O.permute t.env expr in
	(var,expr)
      )
      lvarexpr
    in
    make_value t.env (Domain0.O.substitute_list t.env t.value lvarexpr)
  end

let forget_list t lvar =
  List.iter (Expr1.O.check_envvar t.env) lvar;
  make_value t.env (Domain0.O.forget_list t.env t.value lvar)

let rename t lvarvar =
  if lvarvar=[] then t else
    let nenv = Oo.copy t.env in
    let operm = nenv#rename_vars lvarvar in
    make_value nenv 
      (match operm with
      | None -> t.value
      | Some perm -> Cudd.Bdd.permute t.value perm
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
	SetteS.diff
	  (MappeS.maptoset lce#vartid)
	  (MappeS.maptoset nenv#vartid)
      in
      let supp = Expr0.O.bddsupport lce (SetteS.elements setvar) in
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
    (apply_change_environment abs.Env.value change_environment)
end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

let make_env = Env.make

type 'a t = 'a Expr1.Bool.t
type 'a expr = 'a Expr1.expr

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
let meet_cond = O.meet_cond
let assign_list = O.assign_list
let substitute_list = O.substitute_list
let forget_list = O.forget_list
let change_environment = O.change_environment
let rename = O.rename
