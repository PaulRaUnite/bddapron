(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

exception Bddindex = Bdd.Env.Bddindex

type typ = [
  | Bdd.Env.typ
  | Apronexpr.typ
]
type typdef = Bdd.Env.typdef
type expr = [
  | Cudd.Man.v Bdd.Expr0.expr
  | `Apron of ApronexprDD.t
]

let print_typ fmt (typ:[< typ]) =
  match typ with
  | #Bdd.Env.typ as x -> Bdd.Env.print_typ fmt x
  | #Apronexpr.typ as x -> Apronexpr.print_typ fmt x

let print_typdef fmt (x:[< typdef]) = Bdd.Env.print_typdef fmt (x:>typdef)

module O = struct
  class type ['a,'b] t = object
    inherit [[> typ] as 'a,[> typdef] as 'b, Cudd.Man.v] Bdd.Env.O.t

    val mutable v_apron_env : Apron.Environment.t
    method apron_env : Apron.Environment.t
    method set_apron_env : Apron.Environment.t -> unit

    method rename_vars_apron : (string * string) list -> int array option * Apron.Dim.perm option
  end

  let print fmt (env:('a,'b) #t) =
    Format.fprintf fmt "{@[<v>%a;@ apron_env = %a@]}"
      (Bdd.Env.O.print print_typ print_typdef) (env:>('a,'b,Cudd.Man.v) Bdd.Env.O.t)
      (fun fmt x -> Apron.Environment.print fmt x) env#apron_env

  class ['a,'b] make ?bddindex0 ?bddsize ?relational man : ['a,'b] t = object(self)
    inherit ['a,'b,Cudd.Man.v] Bdd.Env.O.make ?bddindex0 ?bddsize ?relational man as super
    val mutable v_apron_env = Apron.Environment.make [||] [||]
    method apron_env = v_apron_env
    method set_apron_env apron_env = v_apron_env <- apron_env

    method vars : string PSette.t =
      let vars = PMappe.maptoset v_vartid in
      let (ivar,qvar) = Apron.Environment.vars v_apron_env in
      let add ap_var set = PSette.add (Apron.Var.to_string ap_var) set in
      let vars = Array.fold_right add ivar vars in
      let vars = Array.fold_right add qvar vars in
      vars

    method add_vars (lvartyp:(string*'a) list) : int array option =
      let (integer,real) =
	List.fold_left
	  (begin fun ((integer,real) as acc) (var,typ) ->
	    match typ with
	    | `Int -> ((Apron.Var.of_string var)::integer,real)
	    | `Real -> (integer,(Apron.Var.of_string var)::real)
	    | _ -> acc
	  end)
	  ([],[]) lvartyp
      in
      let operm = super#add_vars lvartyp in
      if integer<>[] || real<>[] then begin
	v_apron_env <-
	  (Apron.Environment.add v_apron_env
	    (Array.of_list integer) (Array.of_list real))
      end;
      operm

    method remove_vars (lvar:string list) : int array option =
      let arith =
	List.fold_left
	  (begin fun acc var ->
	    match self#typ_of_var var with
	    | `Int
	    | `Real -> (Apron.Var.of_string var)::acc
	    | _ -> acc
	  end)
	  [] lvar
      in
      let operm = super#remove_vars lvar in
      if arith<>[] then begin
	v_apron_env <-
	  (Apron.Environment.remove v_apron_env
	    (Array.of_list arith))
      end;
      operm

    method rename_vars_apron (lvarvar:(string*string) list)
      :
      int array option * Apron.Dim.perm option
      =
      let (lvar1,lvar2) =
	List.fold_left
	  (begin fun ((lvar1,lvar2) as acc) (var1,var2) ->
	    match (self#typ_of_var var1) with
	    | `Int
	    | `Real ->
		((Apron.Var.of_string var1)::lvar1,
		(Apron.Var.of_string var2)::lvar2)
	    | _ -> acc
	  end)
	  ([],[]) lvarvar
      in
      let operm = super#rename_vars lvarvar in
      let oapronperm =
	if lvar1<>[] then begin
	  let (n_apron_env,perm) =
	    Apron.Environment.rename_perm
	      v_apron_env
	      (Array.of_list lvar1) (Array.of_list lvar2)
	  in
	  v_apron_env <- n_apron_env;
	  Some perm
	end
	else
	  None
      in
      (operm,oapronperm)

    method rename_vars (lvarvar:(string*string) list) : int array option
      =
      fst (self#rename_vars_apron lvarvar)
  end

  let make ?bddindex0 ?bddsize ?relational man =
    new make ?bddindex0 ?bddsize ?relational man

  let unify env1 env2 =
    let nenv = Bdd.Env.lce env1 env2 in
    if nenv!=env1 && nenv!=env2 then
      nenv#set_apron_env (Apron.Environment.lce env1#apron_env env2#apron_env);
    nenv
end

type t = (typ,typdef) O.t
let make = O.make
let print = O.print
let unify = O.unify

let add_typ = Bdd.Env.add_typ
let add_vars = Bdd.Env.add_vars
let remove_vars = Bdd.Env.remove_vars
let rename_vars = Bdd.Env.rename_vars

(*  ********************************************************************** *)
(** {2 Precomputing change of environments} *)
(*  ********************************************************************** *)

type change = {
  bdd : Cudd.Man.v Bdd.Env.change;
  apron : Apron.Dim.change2;
}

let compute_change env1 env2 =
  let bdd = Bdd.Env.compute_change env1 env2 in
  let apron_env1 = env1#apron_env in
  let apron_env2 = env2#apron_env in
  let apron =
    Apron.Environment.dimchange2 apron_env1 apron_env2
  in
  { bdd = bdd; apron = apron; }

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

type ('a,'b) value = ('a,'b) Bdd.Env.value = {
  env : 'a;
  val0 : 'b
}

let make_value = Bdd.Env.make_value
