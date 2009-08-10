(** Boolean/Numerical domain linked to environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) man = ('a,'b,'c,'d) Domain0.man
type 'd t = (Env.t, 'd) Env.value
  
let canonicalize ?apron man t = Domain0.canonicalize ?apron man t.val0
let print man fmt t = Domain0.print man t.env fmt t.val0
let size man t = Domain0.size man t.val0
let bottom man env = make_value env (Domain0.bottom man env)
let top man env = make_value env (Domain0.top man env)
let of_apron man env abs1 = 
  let eapron = env.ext.eapron in
  if not (Apron.Environment.equal eapron abs1.Apron.Abstract1.env) then
    failwith "Bddapron.Domain1.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
  ;
  make_value env 
    (Domain0.of_apron man env abs1.Apron.Abstract1.abstract0)
    
let is_bottom man t = Domain0.is_bottom man t.val0
let is_top man t = Domain0.is_top man t.val0
let is_leq man t1 t2 =
  Env.check_value2 t1 t2;
  Domain0.is_leq man t1.val0 t2.val0
let is_eq man t1 t2 =
  Env.check_value2 t1 t2;
  Domain0.is_eq man t1.val0 t2.val0
let to_bddapron man t =
  let eapron = t.env.ext.eapron in
  let list0 = Domain0.to_bddapron man t.val0 in
  List.map
    (fun (bdd,abs0) -> 
      (Env.make_value t.Env.env bdd,
      { 
	Apron.Abstract1.env = eapron;
	Apron.Abstract1.abstract0 = abs0
      }
      ))
    list0
    
let meet man t1 t2 =
  Env.check_value2 t1 t2;
  Env.mapbinop (Domain0.meet man) t1 t2
    
let join man t1 t2 =
  Env.check_value2 t1 t2;
  Env.mapbinop (Domain0.join man) t1 t2
    
let widening man t1 t2 =
  Env.check_value2 t1 t2;
  Env.mapbinop (Domain0.widening man) t1 t2
    
let meet_condition man cond t condition
    =
  let condition0 = 
    Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
      condition t.env
  in
  make_value t.env
    (Domain0.meet_condition man t.env cond t.val0 condition0)
    
let meet_condition2 man t condition2
    =
  let condition0 = 
    Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
      condition2.val1 t.env
  in
  make_value t.env
    (Domain0.meet_condition man t.env condition2.cond
      t.val0 condition0)
    
let assign_lexpr
    ?relational ?nodependency
    man cond
    t lvar lexpr odest
    =
  let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.assign_lexpr ?relational ?nodependency man
      t.env cond t.val0 lvar lexpr0 odest0)
    
let assign_listexpr2
    ?relational ?nodependency
    man
    t lvar listexpr2 odest
    =
  let lexpr0 = 
    Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
  in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.assign_lexpr ?relational ?nodependency man
      t.env listexpr2.cond t.val0 lvar lexpr0 odest0)
    
let substitute_lexpr
    man cond
    t lvar lexpr odest
    =
  let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.substitute_lexpr man
      t.env cond t.val0 lvar lexpr0 odest0)
    
let substitute_listexpr2
    man
    t lvar listexpr2 odest
    =
  let lexpr0 = 
    Bdd.Domain1.O.check_value Expr0.O.permute_list listexpr2.val1 t.env
  in
  let odest0 = Env.check_ovalue t.env odest in
  make_value t.env
    (Domain0.substitute_lexpr man
      t.env listexpr2.cond t.val0 lvar lexpr0 odest0)
    
let forget_list man t lvar =
  make_value t.env
    (Domain0.forget_list man t.env t.val0 lvar)
    
let change_environment man t nenv =
  if Env.is_eq t.env nenv then
    t
  else begin
    let change = Env.compute_change t.env nenv in
    let bottom = Domain0.bottom man nenv in
    make_value nenv
      (Domain0.apply_change ~bottom man t.val0 change)
  end
    
let unify man t1 t2
    =
  let nenv = Env.lce t1.env t2.env in
  let nt1 = change_environment man t1 nenv in
  let nt2 = change_environment man t2 nenv in
  let res = meet man nt1 nt2 in
  res
    
let rename man t lvarvar =
  let nenv = Env.copy t.env in
  let perm = Env.rename_vars_with nenv lvarvar in
  make_value nenv 
    (Domain0.apply_permutation man t.val0 perm)
    
(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain1}} *)
(*  ********************************************************************** *)

let make_mtbdd = Domain0.make_mtbdd
let to_mtbdd = Domain0.to_mtbdd

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain1}} *)
(*  ********************************************************************** *)

let make_bdd = Domain0.make_bdd
let to_bdd = Domain0.to_bdd
