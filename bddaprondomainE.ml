(** Combined Boolean/Numerical domain with environment *)

open Format
open Bddenv

type 'a manager = 'a Bddaprondomain.manager = {
  apron: 'a Apron.Manager.t;
  aprondd : 'a Apron.Abstract1.t Mtbdd2.manager;
}

let print_env = Bddapronexpr.print_env

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O = struct

type ('a,'b) t = ('a,'b Bddaprondomain.t) Bddenv.value
constraint 'a = ('c,'d,'e) #Bddapronexpr.O.env

let check_expr permute t expr =
  if not (MappeS.subset (=) expr.env#vartyp t.env#vartyp) then
    failwith "The expression is not defined on a subenvironment of the value"
  ;
  let nenv = Bddenv.lce2 t.env expr.env in
  let t = BddexprE.O.normalize ApronDD.permute t nenv in
  let expr = BddexprE.O.normalize permute expr nenv in
  (nenv,t,expr)

let check_lvarexprodest permute t lvarexpr odest =
  begin match odest with
  | None -> ()
  | Some dest -> BdddomainE.O.check2 t dest;
  end;
  let nenv =
    List.fold_left
      (begin fun nenv (var,expr) ->
	if not (MappeS.subset (=) expr.env#vartyp t.env#vartyp) then
	  failwith "The expression is not defined on a subenvironment of the value"
	;
	Bddenv.lce2 nenv expr.env
      end)
      t.env lvarexpr
  in
  let t = BddexprE.O.normalize ApronDD.permute t nenv in
  let odest = match odest with
    | None -> None
    | Some dest -> Some(BddexprE.O.normalize ApronDD.permute dest nenv)
  in
  let lvarexpr =
    List.map
      (fun (var,expr) -> (var,BddexprE.O.normalize permute expr nenv))
      lvarexpr
  in
  (nenv,t,lvarexpr,odest)

(*  ********************************************************************** *)
(** {2 Expressions and conditions} *)
(*  ********************************************************************** *)

let size man t = Bddaprondomain.size man t.value
let print fmt t = Bddaprondomain.O.print t.env fmt t.value
let bottom man env = make_value env (Bddaprondomain.O.bottom man env)
let top man env = make_value env (Bddaprondomain.O.top man env)
let is_bottom man t = Bddaprondomain.O.is_bottom man t.value
let is_top man t = Bddaprondomain.O.is_top man t.value
let is_leq man t1 t2 =
  BdddomainE.O.check2 t1 t2;
  Bddaprondomain.O.is_leq man t1.value t2.value
let is_eq man t1 t2 =
  BdddomainE.O.check2 t1 t2;
  Bddaprondomain.O.is_eq man t1.value t2.value

let meet man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
  BdddomainE.O.check2 t1 t2;
  BddexprE.O.mapbinop ApronDD.permute ApronDD.permute
    (Bddaprondomain.O.meet man) t1 t2

let join man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
  BdddomainE.O.check2 t1 t2;
  BddexprE.O.mapbinop ApronDD.permute ApronDD.permute
    (Bddaprondomain.O.join man) t1 t2

let widening man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
  BdddomainE.O.check2 t1 t2;
  BddexprE.O.mapbinop ApronDD.permute ApronDD.permute
    (Bddaprondomain.O.widening man) t1 t2

let meet_cond
(*
  (t:(('c,'d,Bddapronexpr.cond,'b) #env as 'a,'b) t)
  (cond:'a BddapronexprE.Bool.t)
*)
  man
  (t:(('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a, 'b) t)
  (cond: ('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env BddapronexprE.O.Bool.t)
  :
  ('a,'b) t
  =
  let (nenv,t,cond) = check_expr Bddapronexpr.O.Bool.permute t cond in
  make_value nenv (Bddaprondomain.O.meet_cond man nenv t cond)

let assign_list
  man
  (t:(('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a,'b) t)
  (sub:(string * ('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env BddapronexprE.O.expr) list)
  (odest:('a,'b) t option)
  =
  let (nenv,t,sub,odest) =
    check_lvarexprodest Bddapronexpr.permute t sub odest
  in
  make_value nenv
    (Bddaprondomain.O.assign_list man nenv t sub odest)

let substitute_list
  man
  (t:(('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env as 'a,'b) t)
  (sub:(string * ('c,'d,Bddapronexpr.cond) #Bddapronexpr.O.env BddapronexprE.O.expr) list)
  (odest:('a,'b) t option)
  :
  ('a,'b) t
  =
  let (nenv,t,sub,odest) =
    check_lvarexprodest Bddapronexpr.permute t sub odest
  in
  make_value nenv
    (Bddaprondomain.O.substitute_list man nenv t sub odest)

let forget_list man (t:('a,'b) t) (lvar:string list) : ('a,'b) t =
  make_value t.env
    (Bddaprondomain.O.forget_list man t.env t.value lvar)

let change_environment man (t:('a,'b) t) (nenv:'a) =
  assert (
    if Bdd.is_true
      (Bdd.support_inter t.env#cond_supp
	(ApronDD.support t.value))
    then true
    else begin
      printf "@.t=%a@."
	print t
      ;
      false
    end
  )
  ;
  if Bddenv.is_eq t.env nenv then
    t
  else begin
    let apron_env = t.env#apron_env in
    let napron_env = nenv#apron_env in
    let apron_env_is_eq = Apron.Environment.equal apron_env napron_env in
    let bottom = Apron.Abstract1.bottom man.apron napron_env in
    let mtbdd =
      ApronDD.mapguardleaf man.apron
	(begin fun (guard,abs) ->
	  let nguardE =
	    BdddomainE.O.change_environment
	      (make_value t.env guard)
	      nenv
	  in
	  let nabs =
	    if apron_env_is_eq then abs
	    else
	      Apron.Abstract1.change_environment man.apron abs napron_env false
	  in
(*
	  printf "BddaprondomainE.change_env: abs=%a@.nabs=%a@.apron_env=%a@.napron_env=%a@."
	    Apron.Abstract1.print abs
	    Apron.Abstract1.print nabs
	    (fun x -> Apron.Environment.print x) apron_env
	    (fun x -> Apron.Environment.print x) napron_env
	  ;
*)
	  (nguardE.value,nabs)
	end)
	t.value
	bottom
    in
    let res = make_value nenv mtbdd in
  assert (
    if Bdd.is_true
      (Bdd.support_inter res.env#cond_supp
	(ApronDD.support res.value))
    then true
    else begin
      printf "@.res=%a@."
	print res
      ;
      false
    end
  )
  ;
    res
  end

let unify man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t
  =
  let nenv = Bddenv.lce t1.env t2.env in
  nenv#set_apron_env (Apron.Environment.lce t1.env#apron_env t2.env#apron_env);
  let nt1 = change_environment man t1 nenv in
  let nt2 = change_environment man t2 nenv in
  let res = meet man nt1 nt2 in
  assert (
    Bdd.is_true
    (Bdd.support_inter res.env#cond_supp
      (ApronDD.support res.value)))
  ;
(*
  printf "BddaprondomainE.unify:@.%a@.%a@.%a@.%a@.=> %a@."
    print t1 print nt1
    print t2 print nt2
    print res
  ;
*)
  res

let rename man (t:('a,'b) t) lvarvar =
  if lvarvar=[] then t
  else begin
    let (nenv,perm) = Bddaprondomain.O.Internal.rename_vars t.env lvarvar in
(*
    printf "BddaprondomainE.rename: %a@.env=%a@.nenv=%a@.perm=%a@."
      (Print.list
	(Print.pair pp_print_string pp_print_string))
      lvarvar
      print_env (Obj.magic t.env)
      print_env (Obj.magic nenv)
      (Print.array pp_print_int) perm
    ;
*)
    let (bool,lvar1,lvar2) =
      List.fold_left
	(begin fun ((bool,lvar1,lvar2) as acc) (var1,var2) ->
	  match (t.env#typ_of_var var1) with
	  | `Int
	  | `Real ->
	      (bool,
	      (Apron.Var.of_string var1)::lvar1,
	      (Apron.Var.of_string var2)::lvar2)
	  | #Bddexpr.typ ->
	      (true,lvar1,lvar2)
	  | _ -> acc
	end)
	(false,[],[]) lvarvar
    in
    let (tvar1,tvar2) = (Array.of_list lvar1,Array.of_list lvar2) in
    let mtbdd =
      if not bool then
	ApronDD.rename_array man.apron t.value tvar1 tvar2
      else begin
	let bottom = Apron.Abstract1.bottom man.apron nenv#apron_env in
	ApronDD.mapguardleaf man.apron
	  (begin fun (guard,abs) ->
	    (Bdd.permute guard perm,
	     if tvar1=[||] then abs else
	       Apron.Abstract1.rename_array man.apron abs tvar1 tvar2)
	  end)
	  t.value
	  bottom
      end
    in
    make_value nenv mtbdd
  end

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

type env = Bddapronexpr.env
let make_env = Bddapronexpr.make_env
let add_typ = Bddapronexpr.add_typ
let add_vars = Bddapronexpr.add_vars
let remove_vars = Bddapronexpr.remove_vars
let rename_vars = Bddaprondomain.rename_vars

let make_manager = Bddaprondomain.make_manager
type 'a t = (env, 'a Bddaprondomain.t) Bddenv.value

let size = O.size 
let print fmt x = O.print fmt x
let bottom = O.bottom 
let top= O.top
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
let change_environment = O.change_environment 
let rename = O.rename 
let widening = O.widening 
let unify = O.unify
