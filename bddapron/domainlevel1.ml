(** Functor to transform an abstract domain interface from level 0
    to level 1 (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Cond
open Cond
open Bdd.Env
open Env

module type Level0 = sig
  type 'a man
  type 'a t
    
  val size : 'a man -> 'a t -> int
  val print : Env.t -> Format.formatter -> 'c t -> unit

  val bottom : 'a man -> Env.t -> 'a t
  val top : 'a man -> Env.t -> 'a t
  val of_apron : 'a man -> Env.t -> 'a Apron.Abstract0.t -> 'a t

  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val to_bddapron : 
    'a man -> 'a t -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val meet_condition : 
    'a man -> Env.t -> Cond.t -> 'a t -> Expr0.Bool.t -> 'a t
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    'a man -> Env.t -> Cond.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val substitute_lexpr :
    'a man -> Env.t -> Cond.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val forget_list : 'a man -> Env.t -> 'a t -> string list -> 'a t
  val widening : 'a man -> 'a t -> 'a t -> 'a t
  val apply_change :
    bottom:'a t -> 'a man -> 'a t -> Env.change -> 'a t
  val apply_permutation :
    'a man -> 'a t -> int array option * Apron.Dim.perm option -> 'a t
end

module type Level1 = sig
  type 'a man
  type 'a t0
  type 'a t = (Env.t, 'a t0) Env.value
    
  val size : 'a man -> 'a t -> int
  val print : Format.formatter -> 'c t -> unit
  val bottom : 'a man -> Env.t -> 'a t
  val top : 'a man -> Env.t -> 'a t
  val of_apron : 'a man -> Env.t -> 'a Apron.Abstract1.t -> 'a t
  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val to_bddapron : 'a man -> 'a t -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val meet_condition : 'a man -> Cond.t -> 'a t -> Expr1.Bool.t -> 'a t
  val meet_condition2 : 'a man -> 'a t -> Expr2.Bool.t -> 'a t
    
  val assign_lexpr :
    ?relational:bool -> ?nodependency:bool ->
    'a man -> Cond.t ->
    'a t -> string list -> Expr1.t list -> 'a t option -> 'a t
  val assign_listexpr2 :
    ?relational:bool -> ?nodependency:bool ->
    'a man ->
    'a t -> string list -> Expr2.List.t -> 'a t option ->
    'a t
  val substitute_lexpr :
    'a man -> Cond.t ->
    'a t -> string list -> Expr1.t list -> 'a t option -> 'a t
  val substitute_listexpr2 :
    'a man ->
    'a t -> string list -> Expr2.List.t -> 'a t option -> 'a t
  val forget_list :
    'a man -> 'a t -> string list -> 'a t
  val widening : 'a man -> 'a t -> 'a t -> 'a t
  val change_environment : 'a man -> 'a t -> Env.t -> 'a t
  val rename : 'a man -> 'a t -> (string*string) list -> 'a t
  val unify : 'a man -> 'a t -> 'a t -> 'a t
end

module Make(Level0:Level0) : 
  (Level1 with type 'a man = 'a Level0.man
	  and type 'a t0 = 'a Level0.t)
  = 
struct
  type 'a man = 'a Level0.man
  type 'a t0 = 'a Level0.t
  type 'a t = (Env.t, 'a t0) Env.value

  let print fmt t = Level0.print t.env fmt t.val0
  let size man t = Level0.size man t.val0
  let bottom man env = make_value env (Level0.bottom man env)
  let top man env = make_value env (Level0.top man env)
  let of_apron man env abs1 = 
    let eapron = env.ext.eapron in
    if not (Apron.Environment.equal eapron abs1.Apron.Abstract1.env) then
      failwith "Bddapron.domainlevel1.of_apron: the APRON environment of the APRON abstract value is different from the numerical part of the BDDAPRON environment"
    ;
    make_value env 
      (Level0.of_apron man env abs1.Apron.Abstract1.abstract0)

  let is_bottom man t = Level0.is_bottom man t.val0
  let is_top man t = Level0.is_top man t.val0
  let is_leq man t1 t2 =
    Env.check_value2 t1 t2;
    Level0.is_leq man t1.val0 t2.val0
  let is_eq man t1 t2 =
    Env.check_value2 t1 t2;
    Level0.is_eq man t1.val0 t2.val0
  let to_bddapron man t =
    let eapron = t.env.ext.eapron in
    let list0 = Level0.to_bddapron man t.val0 in
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
    Env.mapbinop (Level0.meet man) t1 t2

  let join man t1 t2 =
    Env.check_value2 t1 t2;
    Env.mapbinop (Level0.join man) t1 t2

  let widening man t1 t2 =
    Env.check_value2 t1 t2;
    Env.mapbinop (Level0.widening man) t1 t2

  let meet_condition man cond t condition
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition t.env
    in
    make_value t.env
      (Level0.meet_condition man t.env cond t.val0 condition0)
      
  let meet_condition2 man t condition2
      =
    let condition0 = 
      Bdd.Domain1.O.check_value Expr0.O.Bool.permute 
	condition2.val1 t.env
    in
    make_value t.env
      (Level0.meet_condition man t.env condition2.cond
	t.val0 condition0)
      
  let assign_lexpr
      ?relational ?nodependency
      man cond
      t lvar lexpr odest
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Level0.assign_lexpr ?relational ?nodependency man
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
      (Level0.assign_lexpr ?relational ?nodependency man
	t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

  let substitute_lexpr
      man cond
      t lvar lexpr odest
      =
    let lexpr0 = Bdd.Domain1.O.check_lvalue Expr0.O.permute lexpr t.env in
    let odest0 = Env.check_ovalue t.env odest in
    make_value t.env
      (Level0.substitute_lexpr man
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
      (Level0.substitute_lexpr man
	t.env listexpr2.cond t.val0 lvar lexpr0 odest0)

  let forget_list man t lvar =
    make_value t.env
      (Level0.forget_list man t.env t.val0 lvar)

  let change_environment man t nenv =
    if Env.is_eq t.env nenv then
      t
    else begin
      let change = Env.compute_change t.env nenv in
      let bottom = Level0.bottom man nenv in
      make_value nenv
	(Level0.apply_change ~bottom man t.val0 change)
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
      (Level0.apply_permutation man t.val0 perm)

end
  
