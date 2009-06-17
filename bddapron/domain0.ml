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

let size man = Cudd.Mtbdd.size

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {3 Interface to ApronDD} *)
  (*  ==================================================================== *)

  let print env =
    ApronDD.print (Bdd.Expr0.O.print_bdd env)

  let bottom man env =
    ApronDD.bottom ~cudd:(env#cudd) man env#apron_env
  let top man env =
    ApronDD.top ~cudd:(env#cudd) man env#apron_env
  let of_apron man env abs1 =
    let apron_env = env#apron_env in
    if not (Apron.Environment.equal apron_env abs1.Apron.Abstract1.env) then
      failwith "Bddapron.Domain0.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
    ;
    ApronDD.cst ~cudd:(env#cudd) man abs1

  let is_bottom = ApronDD.is_bottom
  let is_top = ApronDD.is_top
  let is_leq = ApronDD.is_leq
  let is_eq = ApronDD.is_eq
  let to_bddapron (man:'a man) (x:'a t) = 
    let tab = Cudd.Mtbdd.guardleafs x in
    Array.fold_left 
      (begin fun res ((bdd,abs) as pair) ->
	if Apron.Abstract1.is_bottom man.apron abs 
	then res
	else pair::res
      end)
      [] tab

  let meet = ApronDD.meet
  let join = ApronDD.join
  let widening = ApronDD.widening

  (*  ==================================================================== *)
  (** {3 Meet with an elementary condition, cofactors} *)
  (*  ==================================================================== *)

  let meet_idcondb
      (man:'a man)
      (env:(('b,'c) #Env.O.t as 'd))
      (cond:(Cond.cond,'d) #Cond.O.t)
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
      let `Apron condition = cond#cond_of_idb (idcond,b) in
      let tcons1 = Apronexpr.Condition.to_tcons1 env#apron_env condition in
      let tcons = Apron.Tcons1.array_make env#apron_env 1 in
      Apron.Tcons1.array_set tcons 0 tcons1;
      ApronDD.meet_tcons_array man t tcons
    end

  (*  ==================================================================== *)
  (** {3 Meet with Boolean formula} *)
  (*  ==================================================================== *)

  let meet_condition man env cond (t:'a t) (condition:Expr0.Bool.t) : 'a t =
    let bottom = bottom man env in
    Descend.descend_mtbdd man env cond
      (begin fun t texpr ->
	match texpr.(0) with
	| `Bool bdd ->
	    Cudd.Mtbdd.ite bdd t bottom
	| _ -> failwith ""
      end)
      t [| `Bool condition |]

  (*  ==================================================================== *)
  (** {3 Assignement/Substitution} *)
  (*  ==================================================================== *)

  let assign_lexpr
      ?relational ?nodependency
      (man:'a man)
      (env:(('b,'c) #Env.O.t as 'd))
      (cond:(Cond.cond,'d) #Cond.O.t)
      (t:'a t)
      (lvar : string list) (lexpr: Expr0.t list)
      (odest:'a t option)
      :
      'a t
      =
    assert(List.length lvar = List.length lexpr);
    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun t texpr ->
	let lexpr = Array.to_list texpr in
	let (lbvar,lbexpr,tavar,taexpr) = Descend.split_lvarlexpr lvar lexpr in
	let res =
	  ApronDD.asssub_texpr_array
	    ~asssub_bdd:(fun bdd ->
	      Bdd.Domain0.O.assign_lexpr ?relational ?nodependency
		env bdd lbvar lbexpr
	    )
	    Apron.Abstract1.assign_texpr_array
	    man t tavar taexpr odest
	in
	res
      end)
      t texpr

  let substitute_lexpr
      (man:'a man)
      (env:(('b,'c) #Env.O.t as 'd))
      (cond:(Cond.cond,'d) #Cond.O.t)
      (t:'a t)
      (lvar : string list) (lexpr: Expr0.t list)
      (odest:'a t option)
      :
      'a t
      =
    assert(List.length lvar = List.length lexpr);
    let dest0 = match odest with
      | Some x -> x
      | None -> top man env
    in
    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun dest texpr ->
	let lexpr = Array.to_list texpr in
	let (lbvar,lbexpr,tavar,taexpr) = Descend.split_lvarlexpr lvar lexpr in
	if tavar=[||] then
	  let compose = Bdd.Expr0.O.composition_of_lvarlexpr env lbvar lbexpr in
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
		Bdd.Domain0.O.substitute_lexpr env bdd lbvar lbexpr
	      )
	      Apron.Abstract1.substitute_texpr_array
	      man t tavar taexpr odest
	  in
	  res
      end)
      dest0 texpr

  (*  ==================================================================== *)
  (** {3 Forget} *)
  (*  ==================================================================== *)

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
let of_apron = O.of_apron
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let to_bddapron = O.to_bddapron
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let assign_lexpr = O.assign_lexpr
let substitute_lexpr = O.substitute_lexpr
let forget_list = O.forget_list
let widening = O.widening
