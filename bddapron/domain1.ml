(** Combined Boolean/Numerical domain with environment *)

open Format
open Env

type 'a man = 'a ApronDD.man = {
  apron: 'a Apron.Manager.t;
  table : 'a ApronDD.table;
  oglobal : 'a ApronDD.global option;
}


(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O = struct

  type ('a,'b) t = ('a,'b Domain0.t) Bdd.Env.value
  constraint 'a = ('c,'d,'e) #Env.O.t

  let print fmt (t:('a,'b) t) : unit = Domain0.O.print t.env fmt t.value

  let make_env = Env.O.make

  let check_expr print_expr permute t expr =
    if not (PMappe.subset (=) expr.env#vartyp t.env#vartyp) then
      failwith
	(Print.sprintf
	  "The expression is not defined on a subenvironment of the value:@.expr=%a@.t=%a@.epxr.env=%a@.t.env=%a@."
	  (print_expr expr.env) expr.value (Domain0.O.print t.env) t.value
	  Env.O.print expr.env Env.O.print t.env
	)
    ;
    let nenv = Bdd.Env.lce2 t.env expr.env in
    let t = Bdd.Expr1.O.normalize Cudd.Mtbdd.permute t nenv in
    let expr = Bdd.Expr1.O.normalize permute expr nenv in
    (nenv,t,expr)

  let check_lvarexprodest permute t lvarexpr odest =
    begin match odest with
    | None -> ()
    | Some dest -> Bdd.Domain1.O.check2 t dest;
    end;
    let nenv =
      List.fold_left
	(begin fun nenv (var,expr) ->
	  if not (PMappe.subset (=) expr.env#vartyp t.env#vartyp) then
	    failwith "The expression is not defined on a subenvironment of the value"
	  ;
	  Bdd.Env.lce2 nenv expr.env
	end)
	t.env lvarexpr
    in
    let t = Bdd.Expr1.O.normalize Cudd.Mtbdd.permute t nenv in
    let odest = match odest with
      | None -> None
      | Some dest -> Some(Bdd.Expr1.O.normalize Cudd.Mtbdd.permute dest nenv)
    in
    let lvarexpr =
      List.map
	(fun (var,expr) -> (var,Bdd.Expr1.O.normalize permute expr nenv))
	lvarexpr
    in
    (nenv,t,lvarexpr,odest)

(*  ********************************************************************** *)
(** {2 Expressions and conditions} *)
(*  ********************************************************************** *)

  let size man t = Domain0.size man t.value
  let bottom man env = make_value env (Domain0.O.bottom man env)
  let top man env = make_value env (Domain0.O.top man env)
  let is_bottom man t = Domain0.O.is_bottom man t.value
  let is_top man t = Domain0.O.is_top man t.value
  let is_leq man t1 t2 =
    Bdd.Domain1.O.check2 t1 t2;
    Domain0.O.is_leq man t1.value t2.value
  let is_eq man t1 t2 =
    Bdd.Domain1.O.check2 t1 t2;
    Domain0.O.is_eq man t1.value t2.value

  let meet man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Domain1.O.check2 t1 t2;
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (Domain0.O.meet man) t1 t2

  let join man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Domain1.O.check2 t1 t2;
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (Domain0.O.join man) t1 t2

  let widening man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Domain1.O.check2 t1 t2;
    Bdd.Expr1.O.mapbinop Cudd.Mtbdd.permute Cudd.Mtbdd.permute
      (Domain0.O.widening man) t1 t2

  let meet_cond
(*
  (t:(('c,'d,Env.cond,'b) #env as 'a,'b) t)
  (cond:'a Expr1.Bool.t)
*)
      man
      (t:(('c,'d,Env.cond) #Env.O.t as 'a, 'b) t)
      (cond: ('c,'d,Env.cond) #Env.O.t Expr1.O.Bool.t)
      :
      ('a,'b) t
      =
    let (nenv,t,cond) = check_expr Expr0.O.Bool.print Expr0.O.Bool.permute t cond in
    make_value nenv (Domain0.O.meet_cond man nenv t cond)

  let assign_list
      ?relational ?nodependency
      man
      (t:(('c,'d,Env.cond) #Env.O.t as 'a,'b) t)
      (sub:(string * ('c,'d,Env.cond) #Env.O.t Expr1.O.expr) list)
      (odest:('a,'b) t option)
      =
    let (nenv,t,sub,odest) =
      check_lvarexprodest Expr0.permute t sub odest
    in
    make_value nenv
      (Domain0.O.assign_list ?relational ?nodependency man nenv t sub odest)

  let substitute_list
      man
      (t:(('c,'d,Env.cond) #Env.O.t as 'a,'b) t)
      (sub:(string * ('c,'d,Env.cond) #Env.O.t Expr1.O.expr) list)
      (odest:('a,'b) t option)
      :
      ('a,'b) t
      =
    let (nenv,t,sub,odest) =
      check_lvarexprodest Expr0.permute t sub odest
    in
    make_value nenv
      (Domain0.O.substitute_list man nenv t sub odest)

  let forget_list man (t:('a,'b) t) (lvar:string list) : ('a,'b) t =
    make_value t.env
      (Domain0.O.forget_list man t.env t.value lvar)

  let change_environment man t nenv =
    assert (
      if Cudd.Bdd.is_true
	(Cudd.Bdd.support_inter t.env#cond_supp
	  (Cudd.Mtbdd.support t.value))
      then true
      else begin
	printf "@.t=%a@."
	  print t
	;
	false
      end
    )
    ;
    if Bdd.Env.is_eq t.env nenv then
      t
    else begin
      let apron_env = t.env#apron_env in
      let napron_env = nenv#apron_env in
      let apron_env_is_eq = Apron.Environment.equal apron_env napron_env in
      let bottom = Domain0.O.bottom man nenv in
      let change_environment = Bdd.Domain1.O.compute_change_environment t.env nenv in
      let mtbdd =
	Cudd.Mtbdd.expansivemapleaf1
	  ~default:bottom
	  ~merge:(ApronDD.join man)
	  (begin fun guard absu ->
	    let nguardE = Bdd.Domain1.O.apply_change_environment guard change_environment
	    in
	    let abs = Cudd.Mtbdd.get absu in
	    let nabsu =
	      if apron_env_is_eq then absu
	      else
		Cudd.Mtbdd.unique man.table
		  (Apron.Abstract1.change_environment
		    man.apron abs napron_env false)
	    in
(*
  printf "BddaprondomainE.change_env: abs=%a@.nabs=%a@.apron_env=%a@.napron_env=%a@."
  Apron.Abstract1.print abs
  Apron.Abstract1.print nabs
  (fun x -> Apron.Environment.print x) apron_env
  (fun x -> Apron.Environment.print x) napron_env
  ;
*)
	    (nguardE,nabsu)
	  end)
	  t.value
      in
      let res = make_value nenv mtbdd in
      assert (
	if Cudd.Bdd.is_true
	  (Cudd.Bdd.support_inter res.env#cond_supp
	    (Cudd.Mtbdd.support res.value))
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
    let nenv = Env.O.unify t1.env t2.env in
    let nt1 = change_environment man t1 nenv in
    let nt2 = change_environment man t2 nenv in
    let res = meet man nt1 nt2 in
    assert (
      Cudd.Bdd.is_true
	(Cudd.Bdd.support_inter res.env#cond_supp
	  (Cudd.Mtbdd.support res.value)))
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
    if false then printf "rename %a@."
      (Print.list (Print.pair pp_print_string  pp_print_string)) lvarvar
    ;
    let nenv = Oo.copy t.env in
    let (operm,oapronperm) = nenv#rename_vars_apron lvarvar in
    let mtbdd =
      match operm with
      | None ->
	  begin match oapronperm with
	  | None -> t.value
	  | Some(apronperm) ->
	      Cudd.Mtbdd.map_op1
		(fun absu ->
		  let abs = Cudd.Mtbdd.get absu in
		  let abs0 = abs.Apron.Abstract1.abstract0 in
		  let nabs0 = Apron.Abstract0.permute_dimensions man.apron abs0 apronperm in
		  let nabs = {
		    Apron.Abstract1.env = nenv#apron_env;
		    Apron.Abstract1.abstract0 = nabs0 }
		  in
		  let nabsu = Cudd.Mtbdd.unique man.table nabs in
		  nabsu
		)
		t.value
	  end
      | Some(perm) ->
	  let bottom = Domain0.O.bottom man nenv in
	  Cudd.Mtbdd.retractivemapleaf1
	    ~default:bottom
	    (begin fun guard absu ->
	      (Cudd.Bdd.permute guard perm,
	      begin match oapronperm with
	      | None -> absu
	      | Some perm ->
		  let abs = Cudd.Mtbdd.get absu in
		  let abs0 = abs.Apron.Abstract1.abstract0 in
		  let nabs0 = Apron.Abstract0.permute_dimensions man.apron abs0 perm in
		  let nabs = {
		    Apron.Abstract1.env = nenv#apron_env;
		    Apron.Abstract1.abstract0 = nabs0 }
		  in
		  let nabsu = Cudd.Mtbdd.unique man.table nabs in
		  nabsu
	      end)
	    end)
	    t.value
    in
    make_value nenv mtbdd

end

(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

let make_man = Domain0.make_man
let make_env = Env.make

type 'a t = (Env.t, 'a Domain0.t) Bdd.Env.value

let size = O.size
let print = O.print
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
let rename (man:'a man) (x:'a t) lvarvar = O.rename man x lvarvar
let widening = O.widening
let unify = O.unify
