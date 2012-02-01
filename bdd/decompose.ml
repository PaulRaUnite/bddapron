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

let splitpermutation_of_envcond
    env cond typ
    :
    int * (int array * int array) option
    =
  let r = make_info env cond in
  if typ=`BoolCond && r.maxlevelbool < r.minlevelcond then
    (r.minlevelcond,None)
  else if typ=`CondBool && r.maxlevelcond < r.minlevelbool then
    (r.minlevelbool,None)
  else begin
    let permute1 = Array.make (Array.length r.levelvar) 0 in
    let permute2 = Array.make (Array.length r.levelvar) 0 in
    let (blevel,clevel) = match typ with
      | `BoolCond ->
	  let blevel = ref (-1) in
	  let clevel = ref (!blevel + (env.bddindex-env.bddindex0)) in
	  (blevel,clevel)
      | `CondBool ->
	  let clevel = ref (-1) in
	  let blevel = ref (!clevel + (cond.Cond.bddindex-cond.Cond.bddindex0)) in
	  (blevel,clevel)
    in
    Array.iteri
      (begin fun level var ->
	let typ = r.leveltyp.(level) in
	let nlevel = match typ with
	  | Bool -> incr blevel; !blevel
	  | Cond -> incr clevel; !clevel
	  | Other -> level
	in
	let nvar = r.levelvar.(nlevel) in
	permute1.(var) <- nvar;
	permute2.(nvar) <- var;
      end)
      r.levelvar
    ;
    let level = match typ with
      | `BoolCond -> !blevel+1
      | `CondBool -> !clevel+1
    in
    (level,Some(permute1,permute2))
  end

let split_bdd
    ?memo1 ?memo2
    (level,opermutations)
    (bexpr:'a Bdd.t)
    :
    ('a Bdd.t * 'a Bdd.t) list
    =
  match opermutations with
  | None ->
      split_level bexpr level
  | Some(permute1,permute2) ->
      let pbexpr = Bdd.permute ?memo:memo1 bexpr permute1 in
      let pres = split_level pbexpr level in
      let memo = match memo2 with
	| None -> Memo.Hash(Hash.create 1)
	| Some m -> m
      in
      let res =
	List.rev_map
	  (fun (boolexpr,condexpr) ->
	    (Bdd.permute ~memo boolexpr permute2,
	    Bdd.permute ~memo condexpr permute2))
	  pres
      in
      if memo2=None then Memo.clear memo;
      res

let cube_split cond cube =
  let supp = Cudd.Bdd.support cube in
  let suppbool = Cudd.Bdd.support_diff supp cond.Cond.supp in
  let cubecond = Cudd.Bdd.exist suppbool cube in
  let cubebool = Cudd.Bdd.cofactor cube cubecond in
  (cubebool,cubecond)

let decompose_bdd_boolcond
    env cond
    (bexpr:'a Bdd.t)
    :
    ('a Bdd.t * 'a Bdd.t) list
    =
  if Bdd.is_true bexpr then
    [(bexpr,bexpr)]
  else if Bdd.is_false bexpr then
    []
  else
    let splitperm = splitpermutation_of_envcond env cond `BoolCond in
    split_bdd splitperm bexpr

let decompose_bdd_condbool
    env cond
    (bexpr:'a Bdd.t)
    :
    ('a Bdd.t * 'a Bdd.t) list
    =
  if Bdd.is_true bexpr then
    [(bexpr,bexpr)]
  else if Bdd.is_false bexpr then
    []
  else
    let splitperm = splitpermutation_of_envcond env cond `CondBool in
    split_bdd splitperm bexpr

let select_cond cond supp =
  let inter = Bdd.support_inter cond.Cond.supp supp in
  if Bdd.is_true inter
  then -1
  else Bdd.topvar inter

let descend
    ~(cudd:'c Cudd.Man.t)
    ~(maxdepth:int)
    ~(nocare:('a -> bool))
    ~(cube_of_down:('a -> 'c Cudd.Bdd.t))
    ~(cofactor:('a -> 'c Cudd.Bdd.t -> 'a))
    ~(select:('a -> int))
    ~(terminal:(depth:int -> newcube:'c Cudd.Bdd.t -> cube:'c Cudd.Bdd.t ->
		 down:'a -> 'b option))
    ~(ite:(depth:int -> newcube:'c Cudd.Bdd.t ->
	    cond:int -> dthen:'b option -> delse:'b option -> 'b option))
    ~(down:'a)
    :
    'b option
    =
  let rec map depth cube down =
    if nocare down then None
    else begin
      let newcube = cube_of_down down in
      let (cube,down) =
	if Cudd.Bdd.is_true newcube
	then (cube,down)
	else (Cudd.Bdd.dand cube newcube, cofactor down newcube)
      in
      if nocare down then None
      else begin
	let cond = select down in
	if (cond<0) then (* End case *)
	  terminal ~depth:max_int ~newcube ~cube ~down
	else if depth>=maxdepth then (* End case *)
	  terminal ~depth ~newcube ~cube ~down
	else begin (* Recursive case *)
	  let var = Cudd.Bdd.ithvar cudd cond in
	  let nvar = Cudd.Bdd.dnot var in
	  let dthen =
	    map (depth+1) (Cudd.Bdd.dand cube var) (cofactor down var)
	  and delse =
	    map (depth+1) (Cudd.Bdd.dand cube nvar) (cofactor down nvar)
	  in
	  ite ~depth ~newcube ~cond ~dthen ~delse
	end
      end
    end
  in
  map 0 (Cudd.Bdd.dtrue cudd) down

let decompose_bdd_treecondbool
    env cond
    (bexpr:'a Bdd.t)
    :
    (int, 'a Bdd.t) Normalform.tree
    =
  let cudd = cond.Cond.cudd in
  let dtrue = Bdd.dtrue cudd in

  let select =
    let r = make_info env cond in
    if r.maxlevelcond < r.minlevelbool then begin
      fun bdd ->
	let id = Bdd.topvar bdd in
	if id >= cond.Cond.bddindex then -1 else id
    end else begin
      fun bdd -> select_cond cond (Bdd.support bdd)
    end
  in

  let otree =
    descend
      ~cudd:cudd
      ~maxdepth:max_int
      ~nocare:(fun _ -> false)
      ~cube_of_down:(fun _ -> dtrue)
      ~cofactor:Bdd.cofactor
      ~select
      ~terminal:(fun ~depth ~newcube ~cube ~down ->
	Some(Normalform.Leaf(down))
      )
      ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	match (dthen,delse) with
	| (Some t1),(Some t2) -> Some(Normalform.Ite(cond,t1,t2))
	| _ -> failwith ""
      )
      ~down:bexpr
  in
  match otree with
  | Some t -> t
  | None -> failwith ""





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
