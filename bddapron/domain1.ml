(** Combined Boolean/Numerical domain with environment *)

open Format
open Cond
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

  type ('a,'b) t = ('a,'b Domain0.t) Env.value
  constraint 'a = ('c,'d) #Env.O.t

  let print fmt (t:('a,'b) t) : unit = Domain0.O.print t.env fmt t.val0

  (*  ******************************************************************** *)
  (** {2 Expressions and conditions} *)
  (*  ******************************************************************** *)

  let size man t = Domain0.size man t.val0
  let bottom man env = make_value env (Domain0.O.bottom man env)
  let top man env = make_value env (Domain0.O.top man env)
  let of_apron man env abs1 = make_value env (Domain0.O.of_apron man env abs1)
  let is_bottom man t = Domain0.O.is_bottom man t.val0
  let is_top man t = Domain0.O.is_top man t.val0
  let is_leq man t1 t2 =
    Bdd.Expr1.O.check_value2 t1 t2;
    Domain0.O.is_leq man t1.val0 t2.val0
  let is_eq man t1 t2 =
    Bdd.Expr1.O.check_value2 t1 t2;
    Domain0.O.is_eq man t1.val0 t2.val0
  let to_bddapron man t =
    let list0 = Domain0.to_bddapron man t.val0 in
    List.map
      (fun (bdd,abs1) -> (Env.make_value t.Env.env bdd, abs1))
      list0

  let meet man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Expr1.O.check_value2 t1 t2;
    Bdd.Expr1.O.mapbinop (Domain0.O.meet man) t1 t2

  let join man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Expr1.O.check_value2 t1 t2;
    Bdd.Expr1.O.mapbinop (Domain0.O.join man) t1 t2

  let widening man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Bdd.Expr1.O.check_value2 t1 t2;
    Bdd.Expr1.O.mapbinop (Domain0.O.widening man) t1 t2

  let meet_condition
      man 
      (cond:(Cond.cond,'a) #Cond.O.t)
      (t:('a,'b) t) (condition: 'a Expr1.O.Bool.t)
      :
      ('a,'b) t
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition t.env
    in
    make_value t.env
      (Domain0.O.meet_condition man t.env cond t.val0 condition0)
      
  let meet_condition2
      (man:'b man)
      (t:('a,'b) t) (condition2: ('c,'a) Expr2.O.Bool.t)
      :
      ('a,'b) t
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition2.Cond.val1 t.env
    in
    make_value t.env
      (Domain0.O.meet_condition man t.env condition2.Cond.cond
	t.val0 condition0)
      
  let assign_lexpr
      ?relational ?nodependency
      man
      (cond:(Cond.cond,'a) #Cond.O.t)
      (t:('a,'b) t)
      (lvar:string list) (lexpr:'a Expr1.O.t list)
      (odest:('a,'b) t option)
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Bdd.Expr1.O.check_ovalue t.env odest in
    make_value t.env
      (Domain0.O.assign_lexpr ?relational ?nodependency man
	t.env cond t.val0 lvar lexpr0 odest0)

  let assign_listexpr2
      ?relational ?nodependency
      man
      (t:('a,'b) t)
      (lvar:string list) (listexpr2:('c,'a) Expr2.O.List.t)
      (odest:('a,'b) t option)
      =
    let lexpr0 = 
      Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
    in
    let odest0 = Bdd.Expr1.O.check_ovalue t.env odest in
    make_value t.env
      (Domain0.O.assign_lexpr ?relational ?nodependency man
	t.env listexpr2.Cond.cond t.val0 lvar lexpr0 odest0)

  let substitute_lexpr
      man
      (cond:(Cond.cond,'a) #Cond.O.t)
      (t:('a,'b) t)
      (lvar:string list) (lexpr:'a Expr1.O.t list)
      (odest:('a,'b) t option)
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Bdd.Expr1.O.check_ovalue t.env odest in
    make_value t.env
      (Domain0.O.substitute_lexpr man
	t.env cond t.val0 lvar lexpr0 odest0)

  let substitute_listexpr2
      man
      (t:('a,'b) t)
      (lvar:string list) (listexpr2:('c,'a) Expr2.O.List.t)
      (odest:('a,'b) t option)
      =
    let lexpr0 = 
      Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
    in
    let odest0 = Bdd.Expr1.O.check_ovalue t.env odest in
    make_value t.env
      (Domain0.O.substitute_lexpr man
	t.env listexpr2.Cond.cond t.val0 lvar lexpr0 odest0)

  let forget_list man (t:('a,'b) t) (lvar:string list) : ('a,'b) t =
    make_value t.env
      (Domain0.O.forget_list man t.env t.val0 lvar)

  let change_environment man t nenv =
    if Bdd.Env.is_eq t.env nenv then
      t
    else begin
      let apron_env = t.env#apron_env in
      let napron_env = nenv#apron_env in
      let apron_env_is_eq = Apron.Environment.equal apron_env napron_env in
      let bottom = Domain0.O.bottom man nenv in
      let change_environment = Bdd.Domain1.O.compute_change_environment t.env nenv in
      let mtbdd = Cudd.Mtbdd.expansivemapleaf1
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
	t.val0
      in
      make_value nenv mtbdd
    end

  let unify man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t
      =
    let nenv = Env.O.unify t1.env t2.env in
    let nt1 = change_environment man t1 nenv in
    let nt2 = change_environment man t2 nenv in
    let res = meet man nt1 nt2 in
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
    let mtbdd = match operm with
      | None -> begin match oapronperm with
	| None -> t.val0
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
	      t.val0
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
	    t.val0
    in
    make_value nenv mtbdd

end

  (*  ******************************************************************** *)
  (** {2 Closed signature} *)
  (*  ******************************************************************** *)

let make_man = Domain0.make_man

type 'a t = (Env.t, 'a Domain0.t) Env.value

let size = O.size
let print = O.print
let bottom = O.bottom
let top= O.top
let of_apron = O.of_apron
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let to_bddapron = O.to_bddapron
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let meet_condition2 = O.meet_condition2
let assign_lexpr = O.assign_lexpr
let assign_listexpr2 = O.assign_listexpr2
let substitute_lexpr = O.substitute_lexpr
let substitute_listexpr2 = O.substitute_listexpr2
let forget_list = O.forget_list
let change_environment = O.change_environment
let rename (man:'a man) (x:'a t) lvarvar = O.rename man x lvarvar
let widening = O.widening
let unify = O.unify
