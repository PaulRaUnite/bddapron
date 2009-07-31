(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Bdd.Env

type typ = [
  | Bdd.Env.typ
  | Apronexpr.typ
]
type typdef = Bdd.Env.typdef
(** Environment *)
type 'a ext = {
  mutable eapron : Apron.Environment.t;
  mutable aext : 'a;
}
type ('a,'b,'c) t0 = ('a,'b,Cudd.Man.v,'c ext) Bdd.Env.t0
module O = struct
  type ('a,'b,'c) t = ('a,'b,'c) t0
  constraint 'a = [>typ]
  constraint 'b = [>typdef]

  let make ?bddindex0 ?bddsize ?relational cudd aext copy_aext =
    Bdd.Env.O.make ?bddindex0 ?bddsize ?relational cudd
      {
	eapron = Apron.Environment.make [||] [||];
	aext = aext;
      }
      (fun ext -> { ext with aext = copy_aext ext.aext })

  let print_ext print_aext fmt ext =
    Format.fprintf fmt "{@[<v>eapron = %a;@ aext = %a@]}"
      (fun fmt x -> Apron.Environment.print fmt x) ext.eapron
      print_aext ext.aext

  let print print_typ print_typdef print_aext fmt env =
    Bdd.Env.O.print print_typ print_typdef
      (print_ext print_aext)
      fmt env
end

type t = (typ,typdef,unit) O.t

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

let print_typ fmt typ =
  match typ with
  | #Apronexpr.typ as x -> Apronexpr.print_typ fmt x
  | _ as x -> Bdd.Env.print_typ fmt x
let print_typdef fmt typdef = Bdd.Env.print_typdef fmt typdef

let print fmt env =
  O.print print_typ print_typdef (fun fmt _ -> pp_print_string fmt "_") fmt env

let print_idcondb = Bdd.Env.print_idcondb
let print_order = Bdd.Env.print_order

(*  ********************************************************************** *)
(** {2 Constructors} *)
(*  ********************************************************************** *)
let make ?bddindex0 ?bddsize ?relational cudd = 
  O.make ?bddindex0 ?bddsize ?relational cudd () (fun x -> x)

let copy = Bdd.Env.copy

(*  ********************************************************************** *)
(** {2 Accessors} *)
(*  ********************************************************************** *)

let mem_typ = Bdd.Env.mem_typ
let mem_var = Bdd.Env.mem_var
let mem_label = Bdd.Env.mem_label
let typdef_of_typ = Bdd.Env.typdef_of_typ
let typ_of_var = Bdd.Env.typ_of_var
let vars env =
  let vars = PMappe.maptoset env.vartid in
  let (ivar,qvar) = Apron.Environment.vars env.ext.eapron in
  let add ap_var set = PSette.add (Apron.Var.to_string ap_var) set in
  let vars = Array.fold_right add ivar vars in
  let vars = Array.fold_right add qvar vars in
  vars
let labels = Bdd.Env.labels

(*  ********************************************************************** *)
(** {2 Adding types and variables} *)
(*  ********************************************************************** *)

let add_typ_with = Bdd.Env.add_typ_with
let add_typ = Bdd.Env.add_typ

let add_vars_with env (lvartyp:(string*'a) list) : int array option =
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
  let operm = Bdd.Env.add_vars_with env lvartyp in
  if integer<>[] || real<>[] then begin
    env.ext.eapron <-
      (Apron.Environment.add env.ext.eapron
	(Array.of_list integer) (Array.of_list real))
  end;
  operm

let remove_vars_with env (lvar:string list) : int array option =
  let arith =
    List.fold_left
      (begin fun acc var ->
	match typ_of_var env var with
	| `Int
	| `Real -> (Apron.Var.of_string var)::acc
	| _ -> acc
      end)
      [] lvar
  in
  let operm = Bdd.Env.remove_vars_with env lvar in
  if arith<>[] then begin
    env.ext.eapron <-
      (Apron.Environment.remove env.ext.eapron (Array.of_list arith))
  end;
  operm

let rename_vars_with env (lvarvar:(string*string) list)
    :
    int array option * Apron.Dim.perm option
    =
  let (lvar1,lvar2) =
    List.fold_left
      (begin fun ((lvar1,lvar2) as acc) (var1,var2) ->
	match (typ_of_var env var1) with
	| `Int
	| `Real ->
	    ((Apron.Var.of_string var1)::lvar1,
	    (Apron.Var.of_string var2)::lvar2)
	| _ -> acc
      end)
      ([],[]) lvarvar
  in
  let operm = Bdd.Env.rename_vars_with env lvarvar in
  let oapronperm =
    if lvar1<>[] then begin
      let (n_eapron,perm) =
	Apron.Environment.rename_perm
	  env.ext.eapron
	  (Array.of_list lvar1) (Array.of_list lvar2)
      in
      env.ext.eapron <- n_eapron;
      Some perm
    end
    else
      None
  in
  (operm,oapronperm)

let add_vars env lvartyp =
  let nenv = copy env in
  ignore (add_vars_with nenv lvartyp);
  nenv
let remove_vars env lvars =
  let nenv = copy env in
  ignore (remove_vars_with nenv lvars);
  nenv
let rename_vars env lvarvar =
  let nenv = copy env in
  ignore (rename_vars_with nenv lvarvar);
  nenv

(* ********************************************************************** *)
(** {2 Operations} *)
(* ********************************************************************** *)

let is_leq = Bdd.Env.is_leq
let is_eq = Bdd.Env.is_eq

let lce env1 env2 =
  let env = Bdd.Env.lce env1 env2 in
  if not (env==env1 || env==env2) then
    env.ext.eapron <- Apron.Environment.lce env1.ext.eapron env2.ext.eapron
  ;
  env

(*  ********************************************************************** *)
(** {2 Precomputing change of environments} *)
(*  ********************************************************************** *)

type change = {
  cbdd : Cudd.Man.v Bdd.Env.change;
  capron : Apron.Dim.change2;
}

let compute_change env1 env2 =
  let cbdd = Bdd.Env.compute_change env1 env2 in
  let capron =
    Apron.Environment.dimchange2 env1.ext.eapron env2.ext.eapron
  in
  { cbdd = cbdd; capron = capron; }

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

type ('a,'b) value = ('a,'b) Bdd.Env.value = {
  env : 'a;
  val0 : 'b
}

let make_value = Bdd.Env.make_value
let check_var = Bdd.Env.check_var
let check_lvar = Bdd.Env.check_lvar
let check_value = Bdd.Env.check_value
let check_value2 = Bdd.Env.check_value2
let check_value3 = Bdd.Env.check_value3
let check_lvarvalue = Bdd.Env.check_lvarvalue
let check_lvalue = Bdd.Env.check_lvalue
let check_ovalue = Bdd.Env.check_ovalue
let mapunop = Bdd.Env.mapunop
let mapbinop = Bdd.Env.mapbinop
let mapbinope = Bdd.Env.mapbinope
let mapterop = Bdd.Env.mapterop
