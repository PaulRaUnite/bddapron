(** Combined Boolean/Numerical domain with environment *)

open Format
open Bdd.Cond
open Bdd.Env
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

  type ('a,'b) t = ('a,'b Mtbdddomain0.t) Env.value
  constraint 'a = ('c,'d,'e) Env.O.t

  let print fmt (t:('a,'b) t) : unit = Mtbdddomain0.O.print t.env fmt t.val0

  (*  ******************************************************************** *)
  (** {2 Expressions and conditions} *)
  (*  ******************************************************************** *)

  let size man t = Mtbdddomain0.size man t.val0
  let bottom man env = make_value env (Mtbdddomain0.O.bottom man env)
  let top man env = make_value env (Mtbdddomain0.O.top man env)
  let of_apron man env abs1 = 
    let eapron = env.ext.eapron in
    if not (Apron.Environment.equal eapron abs1.Apron.Abstract1.env) then
      failwith "Bddapron.Mtbdddomain0.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
    ;
    make_value env (Mtbdddomain0.O.of_apron man env abs1.Apron.Abstract1.abstract0)

  let is_bottom man t = Mtbdddomain0.O.is_bottom man t.val0
  let is_top man t = Mtbdddomain0.O.is_top man t.val0
  let is_leq man t1 t2 =
    Env.check_value2 t1 t2;
    Mtbdddomain0.O.is_leq man t1.val0 t2.val0
  let is_eq man t1 t2 =
    Env.check_value2 t1 t2;
    Mtbdddomain0.O.is_eq man t1.val0 t2.val0
  let to_bddapron man t =
    let list0 = Mtbdddomain0.to_bddapron man t.val0 in
    let env = t.Env.env in
    let eapron = env.ext.eapron in
    List.map
      (fun (bdd,abs0) -> (
	Env.make_value env bdd, 
	{ 
	  Apron.Abstract1.env = eapron;
	  Apron.Abstract1.abstract0 = abs0;
	}
      ))
      list0

  let meet man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Env.check_value2 t1 t2;
    Env.mapbinop (Mtbdddomain0.O.meet man) t1 t2

  let join man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Env.check_value2 t1 t2;
    Env.mapbinop (Mtbdddomain0.O.join man) t1 t2

  let widening man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t =
    Env.check_value2 t1 t2;
    Env.mapbinop (Mtbdddomain0.O.widening man) t1 t2

  let meet_condition
      man cond
      (t:('a,'b) t) (condition: 'a Expr1.O.Bool.t)
      :
      ('a,'b) t
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition t.env
    in
    make_value t.env
      (Mtbdddomain0.O.meet_condition man t.env cond t.val0 condition0)
      
  let meet_condition2
      (man:'b man)
      (t:('a,'b) t) (condition2: 'a Expr2.O.Bool.t)
      :
      ('a,'b) t
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition2.Cond.val1 t.env
    in
    make_value t.env
      (Mtbdddomain0.O.meet_condition man t.env condition2.Cond.cond
	t.val0 condition0)
      
  let assign_lexpr
      ?relational ?nodependency
      man cond
      (t:('a,'b) t)
      (lvar:string list) (lexpr:'a Expr1.O.t list)
      (odest:('a,'b) t option)
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Mtbdddomain0.O.assign_lexpr ?relational ?nodependency man
	t.env cond t.val0 lvar lexpr0 odest0)

  let assign_listexpr2
      ?relational ?nodependency
      man
      (t:('a,'b) t)
      (lvar:string list) (listexpr2:'a Expr2.O.List.t)
      (odest:('a,'b) t option)
      =
    let lexpr0 = 
      Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
    in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Mtbdddomain0.O.assign_lexpr ?relational ?nodependency man
	t.env listexpr2.Cond.cond t.val0 lvar lexpr0 odest0)

  let substitute_lexpr
      man
      cond
      (t:('a,'b) t)
      (lvar:string list) (lexpr:'a Expr1.O.t list)
      (odest:('a,'b) t option)
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Mtbdddomain0.O.substitute_lexpr man
	t.env cond t.val0 lvar lexpr0 odest0)

  let substitute_listexpr2
      man
      (t:('a,'b) t)
      (lvar:string list) (listexpr2:'a Expr2.O.List.t)
      (odest:('a,'b) t option)
      =
    let lexpr0 = 
      Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
    in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Mtbdddomain0.O.substitute_lexpr man
	t.env listexpr2.Cond.cond t.val0 lvar lexpr0 odest0)

  let forget_list man (t:('a,'b) t) (lvar:string list) : ('a,'b) t =
    make_value t.env
      (Mtbdddomain0.O.forget_list man t.env t.val0 lvar)

  let change_environment man t nenv =
    if Env.is_eq t.env nenv then
      t
    else begin
      let change = Env.compute_change t.env nenv in
      make_value nenv 
	(Mtbdddomain0.O.apply_change 
	  ~bottom:(Mtbdddomain0.O.bottom man nenv)
	  man t.val0 change)
    end
  
  let unify man (t1:('a,'b) t) (t2:('a,'b) t) : ('a,'b) t
      =
    let nenv = Env.lce t1.env t2.env in
    let nt1 = change_environment man t1 nenv in
    let nt2 = change_environment man t2 nenv in
    let res = meet man nt1 nt2 in
    res

  let rename man (t:('a,'b) t) lvarvar =
    if false then printf "rename %a@."
      (Print.list (Print.pair pp_print_string  pp_print_string)) lvarvar
    ;
    let nenv = Env.copy t.env in
    let perm = Env.rename_vars_with nenv lvarvar in
    make_value nenv 
      (Mtbdddomain0.O.apply_permutation man t.val0 perm)

end

(*  ******************************************************************** *)
(** {2 Closed signature} *)
(*  ******************************************************************** *)

let make_man = Mtbdddomain0.make_man

type 'a t = (Env.t, 'a Mtbdddomain0.t) Env.value

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
