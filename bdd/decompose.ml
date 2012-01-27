(** Separation of Boolean formula in purely Boolean/conditional parts *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Cudd
open Cond
open Env

type vdd = bool Vdd.t

let vdd_of_bdd bdd =
  let cudd = Bdd.manager bdd in
  Vdd.ite bdd (Vdd.cst cudd true) (Vdd.cst cudd false)
let bdd_of_vdd vdd =
  Vdd.guard_of_leaf vdd true

type typ = Bool | Cond | Other

type info = {
  mutable minlevelbool : int;
  mutable maxlevelbool : int;
  mutable minlevelcond : int;
  mutable maxlevelcond : int;
  varlevel : int array;
  levelvar : int array;
  vartyp : typ array;
  leveltyp : typ array
}

let make_info env cond =
  let cudd = env.Env.cudd in
  let nb = Man.get_bddvar_nb cudd in
  let r = {
    minlevelbool = max_int;
    maxlevelbool = min_int;
    minlevelcond = max_int;
    maxlevelcond = min_int;
    varlevel = Array.make nb 0;
    levelvar = Array.make nb 0;
    vartyp = Array.make nb Other;
    leveltyp = Array.make nb Other;
  }
  in
  for var=env.bddindex0 to pred (env.bddindex0 + env.bddindex) do
    let level = Man.level_of_var cudd var in
    if level < r.minlevelbool then r.minlevelbool <- level;
    if level > r.maxlevelbool then r.maxlevelbool <- level;
    r.varlevel.(var) <- level;
    r.levelvar.(level) <- var;
    r.vartyp.(var) <- Bool;
    r.leveltyp.(level) <- Bool;
  done;
  for var=cond.Cond.bddindex0 to pred (cond.Cond.bddindex0 + cond.Cond.bddindex) do
    let level = Man.level_of_var cudd var in
    if level < r.minlevelcond then r.minlevelcond <- level;
    if level > r.maxlevelcond then r.maxlevelcond <- level;
    r.varlevel.(var) <- level;
    r.levelvar.(level) <- var;
    r.vartyp.(var) <- Cond;
    r.leveltyp.(level) <- Cond;
  done;
  r

let split_level (bdd:Bdd.vt) (level:int) : (Bdd.vt * Bdd.vt) list
    =
  let vdd = vdd_of_bdd bdd in
  let tcondexpr = Vdd.nodes_below_level vdd (Some level) in
  let lcondexpr = Array.to_list tcondexpr in
  List.fold_left
    (begin fun res condexpr ->
      if Vdd.is_cst condexpr && (Vdd.dval condexpr)=false then
	res
      else
	(Vdd.guard_of_node vdd condexpr, bdd_of_vdd condexpr)::res
    end)
    [] lcondexpr

let decompose_boolcond
    env cond
    (bexpr:Bdd.vt)
    :
    (Bdd.vt * Bdd.vt) list
    =
  if Bdd.is_true bexpr then
    [(bexpr,bexpr)]
  else if Bdd.is_false bexpr then
    []
  else
    let r = make_info env cond in
    if r.maxlevelbool < r.minlevelcond then begin
      split_level bexpr r.minlevelcond
    end
    else begin
      let blevel = ref (-1) in
      let clevel = ref (!blevel + (env.bddindex-env.bddindex0)) in
      let permute = Array.make (Array.length r.levelvar) 0 in
      let ipermute = Array.make (Array.length r.levelvar) 0 in
      Array.iteri
	(begin fun level var ->
	  let typ = r.leveltyp.(level) in
	  let nlevel = match typ with
	    | Bool -> incr blevel; !blevel
	    | Cond -> incr clevel; !clevel
	    | Other -> level
	  in
	  let nvar = r.levelvar.(nlevel) in
	  permute.(var) <- nvar;
	  permute.(nvar) <- var;
	end)
	r.levelvar
      ;
      let pbexpr = Bdd.permute bexpr permute in
      let pres = split_level pbexpr (!blevel+1) in
      let memo = Memo.Hash(Hash.create 1) in
      let res =
	List.rev_map
	  (fun (boolexpr,condexpr) ->
	    (Bdd.permute ~memo boolexpr ipermute,
	    Bdd.permute ~memo condexpr ipermute))
	  pres
      in
      Memo.clear memo;
      res
    end

let conjunction_of_minterm ?first ?last of_idb minterm =
  let first = match first with
    | Some n -> n
    | None -> 0
  in
  let last =
    let length = Array.length minterm in
    match last with
    | Some n -> min n length
    | None -> length
  in
  let l = ref [] in
  for id=first to pred last do
    begin match minterm.(id) with
    | Man.Top -> ()
    | _ as tb -> l := (of_idb (id, (tb=Man.True))) :: !l
    end
  done;
  Normalform.Conjunction(!l)

let dnf_of_bdd ?first ?last of_idb bdd =
  let open Normalform in
  if Bdd.is_false bdd then
    Disjunction []
  else if Bdd.is_true bdd then
    Dtrue
  else begin
    let disj = ref [] in
    Bdd.iter_cube
      (fun minterm ->
	let conj = conjunction_of_minterm ?first ?last of_idb minterm in
	disj := conj :: !disj
      )
      bdd
    ;
    Disjunction(!disj)
  end
