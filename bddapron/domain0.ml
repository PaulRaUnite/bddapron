(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env

type 'a man = 'a ApronDD.man = {
  apron: 'a Apron.Manager.t;
  table : 'a ApronDD.table;
  oglobal : 'a ApronDD.global option;
}
type 'a t = 'a ApronDD.t

let make_man = ApronDD.make_man
let make_env = Env.make

let size man = Cudd.Mtbdd.size

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

let make_env = Env.O.make

(*  ====================================================================== *)
(** {3 Interface to ApronDD} *)
(*  ====================================================================== *)

let print env =
  ApronDD.print (Bdd.Expr0.O.print_bdd env)

let bottom man env =
  ApronDD.bottom ~cudd:(env#cudd) man env#apron_env
let top man env =
  ApronDD.top ~cudd:(env#cudd) man env#apron_env
let is_bottom = ApronDD.is_bottom
let is_top = ApronDD.is_top
let is_leq = ApronDD.is_leq
let is_eq = ApronDD.is_eq
let meet = ApronDD.meet
let join = ApronDD.join
let widening = ApronDD.widening

(*  ====================================================================== *)
(** {3 Meet with an elementary condition, cofactors} *)
(*  ====================================================================== *)

let meet_idcondb
    (man:'a man)
    (env:('b,'c,Env.cond) #Env.O.t)
    (t:'a t)
    (idcondb:int*bool)
    :
    'a t
    =
  let (idcond,b) = idcondb in
  if PMappe.mem idcond env#idcondvar then begin
    let bdd = Cudd.Bdd.ithvar env#cudd idcond in
    let bdd = if b then bdd else Cudd.Bdd.dnot bdd in
    Cudd.Mtbdd.ite bdd t (bottom man env)
  end
  else begin
    let `Apron cond = env#cond_of_idb (idcond,b) in
    let tcons1 = Apronexpr.Condition.to_tcons1 env#apron_env cond in
    let tcons = Apron.Tcons1.array_make env#apron_env 1 in
    Apron.Tcons1.array_set tcons 0 tcons1;
    ApronDD.meet_tcons_array man t tcons
  end


let cofactors
    (man:'a man)
    (env:('b,'c,Env.cond) #Env.O.t)
    (t:'a t)
    (idcond:int)
    :
    ('a t * 'a t)
    =
  if PMappe.mem idcond env#idcondvar then begin
    let bdd = Cudd.Bdd.ithvar env#cudd idcond in
    (Cudd.Mtbdd.cofactor t bdd,
    Cudd.Mtbdd.cofactor t (Cudd.Bdd.dnot bdd))
  end
  else begin
    let `Apron cond1 = env#cond_of_idb (idcond,true) in
    let `Apron cond2 = env#cond_of_idb (idcond,false) in
    let tcons1 = Apronexpr.Condition.to_tcons1 env#apron_env cond1 in
    let tcons2 = Apronexpr.Condition.to_tcons1 env#apron_env cond2 in
    let tcons = Apron.Tcons1.array_make env#apron_env 1 in
    Apron.Tcons1.array_set tcons 0 tcons1;
    let t1 = ApronDD.meet_tcons_array man t tcons in
    Apron.Tcons1.array_set tcons 0 tcons2;
    let t2 = ApronDD.meet_tcons_array man t tcons in
    (t1,t2)
  end

(*  ====================================================================== *)
(** {3 Module Descend} *)
(*  ====================================================================== *)

module Descend = struct
  let texpr_cofactor (texpr:Expr0.t array) bdd =
    Array.map (fun expr -> Expr0.cofactor expr bdd) texpr

  let texpr_support env (texpr: Expr0.t array) =
    Array.fold_left
      (fun res expr ->
	let supp =
	  Cudd.Bdd.support_inter
	    env#cond_supp
	    (Expr0.support_cond env expr)
	in
	Cudd.Bdd.support_union res supp
      )
      (Cudd.Bdd.dtrue env#cudd)
      texpr

  let texpr_cofactors env (texpr: Expr0.t array) topvar =
    let bdd = Cudd.Bdd.ithvar env#cudd topvar in
    let nbdd = Cudd.Bdd.dnot bdd in
    let t1 = Array.map (fun e -> Expr0.cofactor e bdd) texpr in
    let t2 = Array.map (fun e -> Expr0.cofactor e nbdd) texpr in
    (t1,t2)

  (** Performs a recursive descend of MTBDDs [t],[tbdd], [tmtbdd] and [odest],
    until there is no arithmetic conditions in [tbdd] and [tmtbdd], in which case
    calls [f t tbdd tmtbdd odest]. Returns [bottom] if [t] or [odest] is
    bottom. *)
  let rec descend_arith
      (man:'a man)
      (env:('b,'c,Env.cond) #Env.O.t)
      (f:'a t -> Expr0.t array -> 'a t)
      (t:'a t)
      (texpr:Expr0.t array)
      =
    if is_bottom man t then t
    else begin
      let supp = texpr_support env texpr in
      if Cudd.Bdd.is_cst supp then
	f t texpr
      else begin
	let topvar = Cudd.Bdd.topvar supp in
	let (texpr1,texpr2) = texpr_cofactors env texpr topvar in
	let (t1,t2) = cofactors man env t topvar in
	let res1 = descend_arith man env f t1 texpr1 in
	let res2 = descend_arith man env f t2 texpr2 in
	join man res1 res2
      end
    end

end

(*  ====================================================================== *)
(** {3 Meet with Boolean formula} *)
(*  ====================================================================== *)

let meet_cond man env (t:'a t) (cond:Expr0.Bool.t) : 'a t =
  let bottom = bottom man env in
  Descend.descend_arith man env
    (begin fun t texpr ->
      match texpr.(0) with
      | `Bool bdd ->
	  Cudd.Mtbdd.ite bdd t bottom
      | _ -> failwith ""
    end)
    t [| `Bool cond |]

(*  ====================================================================== *)
(** {3 Assignement/Substitution} *)
(*  ====================================================================== *)

let rev_split (l : ('a * 'b) list) : 'a list * 'b list =
  let rec split ((l1,l2) as p) = function
    | [] -> p
    | (x1,x2)::l -> split (x1::l1,x2::l2) l
  in
  split ([],[]) l

let split_lvarlexpr
    (lvar:string list)
    (lvarexpr:Expr0.t list)
    :
    (string * Cudd.Man.v Bdd.Expr0.t) list *
    Apron.Var.t array * ApronexprDD.t array
    =
  let lbvarexpr = ref [] in
  let lavar = ref [] in
  let laexpr = ref [] in
  List.iter2
    (begin fun var expr ->
      match expr with
      | (#Bdd.Expr0.t) as e ->
	  lbvarexpr := (var,e) :: !lbvarexpr
      | `Apron e ->
	  let var = Apron.Var.of_string var in
	  lavar := var :: !lavar;
	  laexpr := e :: !laexpr
    end)
    lvar lvarexpr
  ;
  (!lbvarexpr, Array.of_list !lavar, Array.of_list !laexpr)

let assign_list
    ?relational ?nodependency
    (man:'a man)
    (env:('b,'c,Env.cond) #Env.O.t) (t:'a t)
    (lvarexpr:(string * Expr0.t) list)
    (odest:'a t option)
    :
    'a t
    =
  let (lvar,texpr) =
    let (lvar,lexpr) = rev_split lvarexpr in
    (lvar, Array.of_list lexpr)
  in
  Descend.descend_arith man env
    (begin fun t texpr ->
      let lexpr = Array.to_list texpr in
      let (lbvarexpr,tavar,taexpr) = split_lvarlexpr lvar lexpr in
      let res =
	ApronDD.asssub_texpr_array
	  ~asssub_bdd:(fun bdd ->
	    Bdd.Domain0.O.assign_list ?relational ?nodependency
	      env bdd lbvarexpr
	  )
	  Apron.Abstract1.assign_texpr_array
	  man t tavar taexpr odest
      in
      res
    end)
    t texpr

let substitute_list
    (man:'a man)
    (env:('b,'c,Env.cond) #Env.O.t)
    (t:'a t)
    (lvarexpr:(string * Expr0.t) list)
    (odest:'a t option)
    :
    'a t
    =
  let dest0 = match odest with
    | Some x -> x
    | None -> top man env
  in
  let (lvar,texpr) =
    let (lvar,lexpr) = rev_split lvarexpr in
    (lvar, Array.of_list lexpr)
  in
  Descend.descend_arith man env
    (begin fun dest texpr ->
      let lexpr = Array.to_list texpr in
      let (lbvarexpr,tavar,taexpr) = split_lvarlexpr lvar lexpr in
      if tavar=[||] then
	let compose = Bdd.Expr0.O.composition_of_substitution env lbvarexpr in
	let res = Cudd.Mtbdd.vectorcompose compose t in
	if odest=None && is_eq man dest dest0 then
	  res
	else
	  meet man res dest
      else
	let odest =
	  if odest=None && is_eq man dest dest0
	  then None
	  else Some dest
	in
	let res =
	  ApronDD.asssub_texpr_array
	    ~asssub_bdd:(fun bdd ->
	      Bdd.Domain0.O.substitute_list env bdd lbvarexpr
	    )
	    Apron.Abstract1.substitute_texpr_array
	    man t tavar taexpr odest
	in
	res
    end)
    dest0 texpr


(*  ====================================================================== *)
(** {3 Forget} *)
(*  ====================================================================== *)

let forget_list (man:'a man) env (t:'a t) lvar =
  if lvar=[] then t
  else begin
    let (bvar,avar) =
      List.fold_left
	(begin fun (bvar,avar) var ->
	  match env#typ_of_var var with
	  | #Bdd.Env.typ -> (var::bvar,avar)
	  | _ -> (bvar,(Apron.Var.of_string var)::avar)
	end)
	([],[])
	lvar
    in
    let avar = Array.of_list avar in
    if bvar=[] then
      ApronDD.forget_array man t avar
    else begin
      let supp = Bdd.Expr0.O.bddsupport env bvar in
      if avar=[||] then
	ApronDD.exist man ~supp t
      else
	let mop1 = `Fun (fun tu ->
	  Cudd.Mtbdd.unique man.table
	    (Apron.Abstract1.forget_array man.apron (Cudd.Mtbdd.get tu) avar false))
	in
	Cudd.Mtbdd.map_existop1 mop1 (ApronDD.make_fun man)
	  ~supp t
    end
  end

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

let print = O.print
let bottom = O.bottom
let top = O.top
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let meet = O.meet
let join = O.join
let meet_cond = O.meet_cond
let assign_list = O.assign_list
let substitute_list = O.substitute_list
let forget_list = O.forget_list
let widening = O.widening
let cofactors = O.cofactors
