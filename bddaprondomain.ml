(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bddenv

type env = Bddapronexpr.env
type 'a manager = {
  apron: 'a Apron.Manager.t;
  aprondd : 'a Apron.Abstract1.t Mtbdd2.manager;
}
type 'a t = 'a Apron.Abstract1.t Mtbdd2.t

let make_manager (apron:'a Apron.Manager.t) : 'a manager=
  {
    apron=apron;
    aprondd=ApronDD.make_manager apron
  }
let size env t = ApronDD.size t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

(*  ====================================================================== *)
(** {3 Interface to ApronDD} *)
(*  ====================================================================== *)

let print env =
  ApronDD.print (Bddexpr.O.print_bdd env)

let bottom man env =
  ApronDD.bottom env#manager man.aprondd man.apron env#apron_env
let top man env =
  ApronDD.top env#manager man.aprondd man.apron env#apron_env
let is_bottom man =
  ApronDD.is_bottom man.apron
let is_top man =
  ApronDD.is_top man.apron

let bottom_of_abs man (mtbdd:'a t) : 'a Apron.Abstract1.t =
  let oabs = Mtbdd2.pick_leaf mtbdd in
  match oabs with
  | Some abs ->
    Apron.Abstract1.bottom man.apron (abs.Apron.Abstract1.env)
  | None -> failwith ""

let is_leq (man:'a manager) (x:'a t) (y:'a t) = ApronDD.is_leq ~bottom:(bottom_of_abs man x) man.apron x y
let is_eq man = ApronDD.is_eq man.apron

let meet man x y = ApronDD.meet ~bottom:(bottom_of_abs man x) man.apron x y
let join man x y = ApronDD.join ~bottom:(bottom_of_abs man x) man.apron x y
let widening man x y = ApronDD.widening ~bottom:(bottom_of_abs man x) man.apron x y

(*  ====================================================================== *)
(** {3 Meet with an elementary condition, cofactors} *)
(*  ====================================================================== *)

let meet_idcondb
    (man:'a manager)
    (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
    (t:'a t)
    (idcondb:int*bool)
    :
    'a t
    =
  let (idcond,b) = idcondb in
  if MappeI.mem idcond env#idcondvar then begin
    let bdd = Bdd.ithvar env#manager idcond in
    let bdd = if b then bdd else Bdd.dnot bdd in
    ApronDD.ite bdd t (bottom man env)
  end
  else begin
    let `Apron cond = env#cond_of_idb (idcond,b) in
    let tcons1 = Apronexpr.Condition.to_tcons1 env#apron_env cond in
    let tcons = Apron.Tcons1.array_make env#apron_env 1 in
    Apron.Tcons1.array_set tcons 0 tcons1;
    ApronDD.meet_tcons_array man.apron t tcons
  end


let cofactors
    (man:'a manager)
    (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
    (t:'a t)
    (idcond:int)
    :
    ('a t * 'a t)
    =
  if MappeI.mem idcond env#idcondvar then begin
    let bdd = Bdd.ithvar env#manager idcond in
    (ApronDD.cofactor t bdd,
    ApronDD.cofactor t (Bdd.dnot bdd))
  end
  else begin
    let `Apron cond1 = env#cond_of_idb (idcond,true) in
    let `Apron cond2 = env#cond_of_idb (idcond,false) in
    let tcons1 = Apronexpr.Condition.to_tcons1 env#apron_env cond1 in
    let tcons2 = Apronexpr.Condition.to_tcons1 env#apron_env cond2 in
    let tcons = Apron.Tcons1.array_make env#apron_env 1 in
    Apron.Tcons1.array_set tcons 0 tcons1;
    let t1 = ApronDD.meet_tcons_array man.apron t tcons in
    Apron.Tcons1.array_set tcons 0 tcons2;
    let t2 = ApronDD.meet_tcons_array man.apron t tcons in
    (t1,t2)
  end

(*  ====================================================================== *)
(** {3 Module Descend} *)
(*  ====================================================================== *)

module Descend = struct
  let texpr_cofactor (texpr:Bddapronexpr.expr array) bdd =
    Array.map (fun expr -> Bddapronexpr.cofactor expr bdd) texpr
  let tbexpr_cofactor (texpr:Bddexpr.expr array) bdd =
    Array.map (fun expr -> Bddexpr.cofactor expr bdd) texpr
  let taexpr_cofactor (texpr:ApronexprDD.expr array) bdd =
    Array.map (fun expr -> ApronexprDD.cofactor expr bdd) texpr

  let is_obottom man (abs:'a t option) = match abs with
    | None -> false
    | Some t -> is_bottom man t

  let extract_option = function
    | Some t -> t
    | None -> failwith ""

  (** variant of {!Bddapronexpr.texpr_of_tbddidd}, where the IDDs are assumed
    to be constant *)
  let of_tbddidd
      (oldtexpr:Bddapronexpr.expr array)
      (tbdd:Bdd.t array) (tidd:Idd.t array)
      :
      Bddexpr.expr list * Apronexpr.expr array
      =
    let lbddexpr = ref [] in
    let tapronexpr = Array.make (Array.length tidd) (Obj.magic 0) in
    let indexb = ref (Array.length tbdd) in
    let indexi = ref (Array.length tidd) in
    for i = pred (Array.length oldtexpr) downto 0 do
      begin match oldtexpr.(i) with
      | `Bool x ->
	  decr indexb;
	  let res = tbdd.(!indexb) in
	  lbddexpr := (`Bool res) :: !lbddexpr
      | `Bint x ->
	  indexb := !indexb - (Array.length x.Bddint.reg);
	  let res =
	    { x with Bddint.reg =
		Array.mapi (fun i bdd -> tbdd.(!indexb + i)) x.Bddint.reg
	    }
	  in
	  lbddexpr := (`Bint res) :: !lbddexpr
      | `Benum x ->
	  indexb := !indexb - (Array.length x.Bddenum.reg);
	  let res =
	    { x with Bddenum.reg =
		Array.mapi (fun i bdd -> tbdd.(!indexb + i)) x.Bddenum.reg
	    }
	  in
	  lbddexpr := (`Benum res) :: !lbddexpr
      | `Apron x ->
	  decr indexi;
	  let idd = tidd.(!indexi) in
	  let expr = ApronexprDD.leaf_of_id x.Mtbdd2.man (Idd.dval idd) in
	  tapronexpr.(!indexi) <- expr
      end
    done;
    assert(!indexb=0 && !indexi=0);
    (!lbddexpr, tapronexpr)

  (** Performs a recursive descend of MTBDDs [t],[tbdd], [tidd] and [odest],
    until there is no arithmetic conditions in [tbdd] and [tidd], in which case
    calls [f t tbdd tidd odest]. Returns [bottom] if [t] or [odest] is
    bottom. *)
  let rec descend_arith
      (man:'a manager)
      (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
      (f:'a t -> Bdd.t array -> Idd.t array -> 'a t option -> 'a t)
      (t:'a t)
      (tbdd:Bdd.t array) (tidd:Idd.t array)
      (odest:'a t option)
      =
    if is_bottom man t then t
    else if is_obottom man odest then extract_option odest
    else begin
      let supp = Idd.vectorsupport2 tbdd tidd in
      let suppc = Bdd.support_inter supp env#cond_supp in
      if Bdd.is_cst suppc then
	f t tbdd tidd odest
      else begin
	let cudd = env#manager in
	let topvar = Bdd.topvar suppc in
	let bdd = Bdd.ithvar cudd topvar in
	let nbdd = Bdd.dnot bdd in
	let tbdd1 = Array.map (fun x -> Bdd.cofactor x bdd) tbdd in
	let tidd1 = Array.map (fun x -> Idd.cofactor x bdd) tidd in
	let tbdd2 = Array.map (fun x -> Bdd.cofactor x nbdd) tbdd in
	let tidd2 = Array.map (fun x -> Idd.cofactor x nbdd) tidd in
	let (t1,t2) = cofactors man env t topvar in
	let (odest1,odest2) = match odest with
	  | None -> (None,None)
	  | Some t ->
	      let (t1,t2) = cofactors man env t topvar in
	      (Some t1, Some t2)
	in
	let res1 = descend_arith man env f t1 tbdd1 tidd1 odest1 in
	let res2 = descend_arith man env f t2 tbdd2 tidd2 odest2 in
	join man res1 res2
      end
    end

  (** Performs a recursive descend of MTBDDs [t],[tbdd], [tidd] and
    [odest], until there is no conditions in [tidd], in which case calls
    [f t tbdd tidd odest]. Returns [bottom] if [t] or [odest] is bottom. *)
  let rec descend_bool
      (man:'a manager)
      (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
      (f:'a t -> Bdd.t array -> Idd.t array -> 'a t option -> 'a t)
      (t:'a t)
      (tbdd:Bdd.t array) (tidd:Idd.t array)
      (odest:'a t option)
      =
    if is_bottom man t then t
    else if is_obottom man odest then extract_option odest
    else begin
      let cudd = env#manager in
      let supp =
	if tidd<>[||]
	then Idd.vectorsupport tidd
	else Bdd.dtrue cudd
      in
      if Bdd.is_cst supp then
	f t tbdd tidd odest
      else begin
	let topvar = Bdd.topvar supp in
	let bdd = Bdd.ithvar cudd topvar in
	let nbdd = Bdd.dnot bdd in
	let tbdd1 = Array.map (fun x -> Bdd.cofactor x bdd) tbdd in
	let tidd1 = Array.map (fun x -> Idd.cofactor x bdd) tidd in
	let tbdd2 = Array.map (fun x -> Bdd.cofactor x nbdd) tbdd in
	let tidd2 = Array.map (fun x -> Idd.cofactor x nbdd) tidd in
	let (t1,t2) = cofactors man env t topvar in
	let (odest1,odest2) = match odest with
	  | None -> (None,None)
	  | Some t ->
	      let (t1,t2) = cofactors man env t topvar in
	      (Some t1, Some t2)
	in
	let res1 = descend_bool man env f t1 tbdd1 tidd1 odest1 in
	let res2 = descend_bool man env f t2 tbdd2 tidd2 odest2 in
	ApronDD.ite bdd res1 res2
      end
    end

  (** Combines [descend_arith], and then [descend_bool] *)
  let descend
      (man:'a manager)
      (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
      (f:'a t -> Bddexpr.expr list -> Apronexpr.expr array -> 'a t option -> 'a t)
      (t:'a t)
      (texpr:Bddapronexpr.expr array)
      (odest:'a t option)
      =
    let (tbdd,tidd) = Bddapronexpr.O.tbddidd_of_texpr texpr in
    descend_arith man env
      (begin fun t tbdd tidd odest ->
	begin
	  descend_bool man env
	    (begin fun t tbdd tidd odest ->
	      let (lbddexpr,tapronexpr) = of_tbddidd texpr tbdd tidd in
	      f t lbddexpr tapronexpr odest
	    end)
	    t tbdd tidd odest
	end
      end)
      t tbdd tidd odest

  let rec descend_cond
      (man:'a manager)
      (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
      (f:'a t -> Bddapronexpr.Bool.t -> 'a t)
      (t:'a t)
      (bexpr:Bddapronexpr.Bool.t)
      =
    if Bdd.is_false bexpr then bottom man env
    else if is_bottom man t then t
    else begin
      let supp = Bdd.support bexpr in
      let suppc = Bdd.support_inter supp env#cond_supp in
      if Bdd.is_cst suppc then
	f t bexpr
      else begin
	let cudd = env#manager in
	let topvar = Bdd.topvar suppc in
	let bdd = Bdd.ithvar cudd topvar in
	let bexpr1 = Bdd.cofactor bexpr bdd in
	let bexpr2 = Bdd.cofactor bexpr (Bdd.dnot bdd) in
	if Bdd.is_false bexpr1 then
	  descend_cond man env f
	    (meet_idcondb man env t (topvar,false))
	    bexpr2
	else if Bdd.is_false bexpr2 then
	  descend_cond man env f
	    (meet_idcondb man env t (topvar,true))
	    bexpr1
	else
	  let (t1,t2) = cofactors man env t topvar in
	  let res1 = descend_cond man env f t1 bexpr1 in
	  let res2 = descend_cond man env f t2 bexpr2 in
	  join man res1 res2
      end
    end

end

(*  ====================================================================== *)
(** {3 Meet with Boolean formula} *)
(*  ====================================================================== *)

let meet_cond man env (t:'a t) (cond:Bddapronexpr.Bool.t) : 'a t =
  let bottom = bottom man env in
  Descend.descend_cond man env
    (begin fun t cond ->
      ApronDD.ite cond t bottom
    end)
    t cond

(*  ====================================================================== *)
(** {3 Assignement/Substitution} *)
(*  ====================================================================== *)

module Asssub = struct
  let lbvar_tavar_texpr_of_sub
      (sub:(string * Bddapronexpr.expr) list)
      :
      string list *  Apron.Var.t array * Bddapronexpr.expr array
      =
    let (lbvar,lavar,lexpr) =
      List.fold_left
	(begin fun (lbvar,lavar,lexpr) (var,expr) ->
	  match expr with
	  | #Bddexpr.expr ->
	      (var::lbvar,lavar,expr::lexpr)
	  | `Apron _ ->
	      let var = Apron.Var.of_string var in
	      (lbvar,var::lavar,expr::lexpr)
	end)
	([],[],[])
	sub
    in
    (lbvar, Array.of_list lavar, Array.of_list lexpr)
end

let assign_list
    ?relational ?nodependency
    (man:'a manager)
    (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env) (t:'a t)
    (sub:(string * Bddapronexpr.expr) list)
    (odest:'a t option)
    :
    'a t
    =
  let cudd = env#manager in
  let apron_env = env#apron_env in
  let bottomdd = bottom man env in
  let bottom = ApronDD.dval bottomdd in
  let (lbvar,tavar,texpr) = Asssub.lbvar_tavar_texpr_of_sub sub in

  let assign_elementary
      (t:'a t)
      (lbexpr:Bddexpr.expr list) (taexpr:Apronexpr.expr array)
      (odest:'a t option)
      :
      'a t
      =
    let bsub = List.combine lbvar lbexpr in
    let taexpr = Array.map (Apronexpr.to_texpr1 apron_env) taexpr in
    let res =
      if bsub<>[] then
	match odest with
	| None ->
	    begin match bsub with
	    | [varexpr] when false && tavar=[||] && env#bddincr=2 ->
		let (relation,supp,tbdd) =
		  Bdddomain.O.relation_supp_compose_of_lvarexpr
		    env bsub
		in
		let image =
		  ApronDD.mapexistandop
		    ~absorbant:(bottom_of_abs man t)
		    (Apron.Abstract1.join man.apron)
		    supp relation t
		in
		let image = ApronDD.vectorcompose tbdd image in
		image
	    | _ ->
		ApronDD.mapguardleaf man.apron
		  (begin fun (guard,abs) ->
		      let nguard = Bdddomain.O.assign_list ?relational ?nodependency env guard bsub in
		      let nabs =
			if tavar=[||] then abs
			else
			  Apron.Abstract1.assign_texpr_array man.apron
			    abs tavar taexpr None
		      in
		      (nguard,nabs)
		  end)
		  t
		  bottom
	    end
	| Some dest ->
	    let res = ref bottomdd in
	    Array.iter
	      (begin fun (guard,abs) ->
		if not (Apron.Abstract1.is_bottom man.apron abs) then begin
		  let nguard = Bdddomain.O.assign_list ?relational ?nodependency env guard bsub in
		  let dest = ApronDD.ite nguard dest bottomdd in
		  if not (is_bottom man dest) then begin
		    let nabs =
		      if tavar=[||] then
			meet man
			  (ApronDD.cst cudd man.aprondd abs)
			  dest
		      else if ApronDD.is_cst dest then
			ApronDD.cst cudd man.aprondd
			  (Apron.Abstract1.assign_texpr_array man.apron
			    abs tavar taexpr (Some (ApronDD.dval dest)))
		      else
			ApronDD.assign_texpr_array man.apron
			  (ApronDD.cst cudd man.aprondd abs) tavar taexpr
			  (Some dest)
		    in
		    res := join man !res
		      (ApronDD.ite nguard nabs bottomdd)
		  end
		end
	      end)
	      (ApronDD.guardleafs t)
	    ;
	    !res
      else
	ApronDD.assign_texpr_array man.apron
	  t tavar taexpr
	  odest
    in
    res
  in

  if sub=[]
  then t
  else Descend.descend man env assign_elementary t texpr odest

let substitute_list
    (man:'a manager)
    (env:('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env)
    (t:'a t) (sub:(string * Bddapronexpr.expr) list)
    (odest:'a t option)
    :
    'a t
    =
  let cudd = env#manager in
  let apron_env = env#apron_env in
  let bottomdd = bottom man env in
  let (lbvar,tavar,texpr) = Asssub.lbvar_tavar_texpr_of_sub sub in

  let substitute_elementary
      (t:'a t)
      (lbexpr:Bddexpr.expr list) (taexpr:Apronexpr.expr array)
      (odest:'a t option)
      :
      'a t
      =
    let bsub = List.combine lbvar lbexpr in
    let taexpr = Array.map (Apronexpr.to_texpr1 apron_env) taexpr in
    let res =
      if bsub<>[] then
	let tbdd = Bddexpr.O.composition_of_substitution env bsub in
	let t = ApronDD.vectorcompose tbdd t in
	match (tavar,odest) with
	| [||],None -> t
	| [||],(Some dest) -> meet man t dest
	| (_,None) ->
	    ApronDD.substitute_texpr_array man.apron
	      t tavar taexpr None
	| (_,Some dest) ->
	    let res = ref bottomdd in
	    Array.iter
	      (begin fun (nguard,abs) ->
		if not (Apron.Abstract1.is_bottom man.apron abs) then begin
		  let dest = ApronDD.ite nguard dest bottomdd in
		  if not (is_bottom man dest) then begin
		    let nabs =
		      if ApronDD.is_cst dest then
			ApronDD.cst cudd man.aprondd
			  (Apron.Abstract1.substitute_texpr_array man.apron
			    abs tavar taexpr (Some (ApronDD.dval dest)))
		      else
			ApronDD.substitute_texpr_array man.apron
			  (ApronDD.cst cudd man.aprondd abs) tavar taexpr
			  (Some dest)
		    in
		    res := join man !res
		      (ApronDD.ite nguard nabs bottomdd)
		  end
		end
	      end)
	      (ApronDD.guardleafs t)
	    ;
	    !res
      else
	ApronDD.substitute_texpr_array man.apron t tavar taexpr odest
    in
    res
  in

  if sub=[]
  then t
  else Descend.descend man env substitute_elementary t texpr odest

(*  ====================================================================== *)
(** {3 Forget} *)
(*  ====================================================================== *)

let forget_list (man:'d manager) (env:('a,'b,'c) #Bddapronexpr.O.env) (t:'d t) lvar =
  if lvar=[] then t
  else begin
    let apron_env = env#apron_env in
    let (bvar,avar) =
      List.fold_left
	(begin fun (bvar,avar) var ->
	  match env#typ_of_var var with
	  | #Bddexpr.typ -> (var::bvar,avar)
	  | _ -> (bvar,(Apron.Var.of_string var)::avar)
	end)
	([],[])
	lvar
    in
    let avar = Array.of_list avar in
    if bvar=[] then
      ApronDD.forget_array man.apron t avar
    else begin
      let supp = Bddexpr.O.bddsupport env bvar in
      let bottom = Apron.Abstract1.bottom man.apron apron_env in
      ApronDD.mapguardleaf man.apron
	(begin fun (guard,abs) ->
	  let nguard = Bdd.exist supp guard in
	  let nabs =
	    if avar=[||] then abs
	    else
	      Apron.Abstract1.forget_array man.apron abs avar false
	  in
	  (nguard,nabs)
	end)
	t
	bottom
    end
  end

(*  ====================================================================== *)
(** {3 Environment renaming} *)
(*  ====================================================================== *)

module Internal = struct

  let rename_vars
      (env:('a,'b,'c) #Bddapronexpr.O.env as 'e)
      (lvarvar:(string*string) list)
      :
      'e * int array
      =
    let (env,operm) = Bddenv.clear_cond env in
    begin match operm with
    | None -> ()
    | Some perm -> permute env perm
    end;
    let (nenv,perm) = Bddenv.rename_vars env lvarvar in
    let perm = match operm with
      | None -> perm
      | Some p -> Bddenv.compose_permutation p perm
    in
    let (lvar1,lvar2) =
      List.fold_left
	(begin fun ((lvar1,lvar2) as acc) (var1,var2) ->
	  match (env#typ_of_var var1) with
	  | `Int
	  | `Real ->
	      ((Apron.Var.of_string var1)::lvar1,
	      (Apron.Var.of_string var2)::lvar2)
	  | _ -> acc
	end)
	([],[]) lvarvar
    in
    if lvar1<>[] then begin
      nenv#set_apron_env
	(Apron.Environment.rename nenv#apron_env
	  (Array.of_list lvar1) (Array.of_list lvar2))
    end;
    (nenv,perm)

end

let rename_vars
    (env:('a,'b,'c) #Bddapronexpr.O.env as 'e)
    (lvarvar:(string*string) list)
    :
    'e
    =
  fst (Internal.rename_vars env lvarvar)

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

let rename_vars = O.rename_vars

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
let rename_vars = O.rename_vars
let widening = O.widening
let cofactors = O.cofactors
