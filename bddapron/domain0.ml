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

let size man = Cudd.Mtbddc.size

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  (*  ==================================================================== *)
  (** {3 Interface to ApronDD} *)
  (*  ==================================================================== *)

  let print env =
    let apron_env = env#apron_env in
    let string_of_dim dim =
      let var = Apron.Environment.var_of_dim apron_env dim in
      let str = Apron.Var.to_string var in
      str
    in
    ApronDD.print (Bdd.Expr0.O.print_bdd env) string_of_dim

  let bottom man env =
    ApronDD.bottom
      ~cudd:(env#cudd) man (Apron.Environment.dimension env#apron_env)
  let top man env =
    ApronDD.top
      ~cudd:(env#cudd) man (Apron.Environment.dimension env#apron_env)
  let of_apron man env abs0 =
    ApronDD.cst ~cudd:(env#cudd) man abs0

  let is_bottom = ApronDD.is_bottom
  let is_top = ApronDD.is_top
  let is_leq = ApronDD.is_leq
  let is_eq = ApronDD.is_eq
  let to_bddapron (man:'a man) (x:'a t) =
    let tab = Cudd.Mtbddc.guardleafs x in
    Array.fold_left
      (begin fun res ((bdd,abs) as pair) ->
	if Apron.Abstract0.is_bottom man.apron abs
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
      Cudd.Mtbddc.ite bdd t (bottom man env)
    end
    else begin
      let `Apron condition = cond#cond_of_idb (idcond,b) in
      let tcons0 = Apronexpr.Condition.to_tcons0 env#apron_env condition in
      ApronDD.meet_tcons_array man t [|tcons0|]
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
	    Cudd.Mtbddc.ite bdd t bottom
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
    let (lbvar,tavar) = Descend.split_lvar lvar lexpr in
    let apron_env = env#apron_env in
    let tadim = Array.map (Apron.Environment.dim_of_var apron_env) tavar in

    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun t texpr ->
	let (lbexpr,taexpr) = Descend.split_texpr texpr in
	let res =
	  ApronDD.asssub_texpr_array
	    ~asssub_bdd:(fun bdd ->
	      Bdd.Domain0.O.assign_lexpr ?relational ?nodependency
		env bdd lbvar lbexpr
	    )
	    Apron.Abstract0.assign_texpr_array
	    man apron_env t tadim taexpr odest
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
    let (lbvar,tavar) = Descend.split_lvar lvar lexpr in
    let apron_env = env#apron_env in
    let tadim = Array.map (Apron.Environment.dim_of_var apron_env) tavar in
    let dest0 = match odest with
      | Some x -> x
      | None -> top man env
    in
    let texpr = Array.of_list lexpr in
    Descend.descend_mtbdd man env cond
      (begin fun dest texpr ->
	let (lbexpr,taexpr) = Descend.split_texpr texpr in
	if tadim=[||] then
	  let compose = Bdd.Expr0.O.composition_of_lvarlexpr env lbvar lbexpr in
	  let res = Cudd.Mtbddc.vectorcompose compose t in
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
	      Apron.Abstract0.substitute_texpr_array
	      man apron_env t tadim taexpr odest
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
    let apron_env = env#apron_env in
      let (lbvar,ladim) =
	List.fold_left
	  (begin fun (lbvar,ladim) var ->
	    match env#typ_of_var var with
	    | #Bdd.Env.typ -> (var::lbvar,ladim)
	    | _ ->
		(lbvar,
		(Apron.Environment.dim_of_var apron_env
		  (Apron.Var.of_string var))::ladim)
	  end)
	  ([],[])
	  lvar
      in
      let tadim = Array.of_list ladim in
      if lbvar=[] then
	ApronDD.forget_array man t tadim
      else begin
	let supp = Bdd.Expr0.O.bddsupport env lbvar in
	if tadim=[||] then
	  ApronDD.exist man ~supp t
	else
	  let mop1 = `Fun (fun tu ->
	    Cudd.Mtbddc.unique man.table
	      (Apron.Abstract0.forget_array man.apron (Cudd.Mtbddc.get tu) tadim false))
	  in
	  Cudd.User.map_existop1 mop1 (ApronDD.make_fun man)
	    ~supp t
      end
    end

  let apply_change ~bottom man t change =
    if
      change.bdd.Bdd.Env.intro = None &&
      change.bdd.Bdd.Env.remove = None
    then
      ApronDD.apply_dimchange2 man t change.Env.apron false
    else if
      change.Env.apron.Apron.Dim.add=None && change.Env.apron.Apron.Dim.remove=None
    then begin
      let bdd = change.bdd in
      let t = match bdd.Bdd.Env.intro with
	| None -> t
	| Some perm -> Cudd.Mtbddc.permute t perm
      in
      match bdd.Bdd.Env.remove with
      | None -> t
      | Some(supp,perm) ->
	  let t = ApronDD.exist man ~supp t in
	  Cudd.Mtbddc.permute t perm
    end
    else begin
      let mtbdd = Cudd.Mapleaf.expansivemapleaf1
	~default:bottom
	~merge:(ApronDD.join man)
	(begin fun guard absu ->
	  let nguard = Bdd.Domain0.O.apply_change guard change.bdd in
	  let abs = Cudd.Mtbddc.get absu in
	  let nabs = Apron.Abstract0.apply_dimchange2 man.apron abs change.Env.apron false
	  in
	  let nabsu = Cudd.Mtbddc.unique man.table nabs in
	  (nguard,nabsu)
	end)
	t
      in
      mtbdd
    end

  let apply_permutation man (t:'a t) (operm,oapronperm) =
    let res = match oapronperm with
      | None -> t
      | Some apronperm ->
	  ApronDD.permute_dimensions man t apronperm
    in
    let res = match operm with
      | None -> res
      | Some perm ->
	  Cudd.Mtbddc.permute res perm
    in
    res

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
