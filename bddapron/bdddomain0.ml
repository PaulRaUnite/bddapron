(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type 'a man = {
  apron : 'a Apron.Manager.t;
  mutable bddrestrict : Cudd.Bdd.vt -> Cudd.Bdd.vt -> Cudd.Bdd.vt;
  mutable meet_exclusive : bool;
  mutable join_exclusive : bool;
  mutable meet_cond_nodoublon : bool;
  mutable meet_cond_exclusive : bool;
  mutable meet_cond_depth : int;
}

type 'a elt = {
  guard : Cudd.Bdd.vt;
  abs : 'a Apron.Abstract0.t
}

type 'a t = {
  mutable list : 'a elt list;
  bottom : 'a elt;
  mutable nodoublon : bool;
  mutable exclusive : bool;
}


(*  ********************************************************************** *)
(** {2 Trees} *)
(*  ********************************************************************** *)

type 'a tree =
  | Nil
  | Node of Cudd.Bdd.vt * 'a node
and 'a node =
  | Leaf of 'a
  | Tree of int * 'a tree * 'a tree

let rec tree_iter_leaf f = function
  | Nil -> ()
  | Node(_, node) ->
      begin match node with
      | Leaf x -> f x
      | Tree(_,ta,tb) ->
	  tree_iter_leaf f ta;
	  tree_iter_leaf f tb;
      end

let rec tree_fold_leaf f res = function
  | Nil -> res
  | Node(_, node) ->
      begin match node with
      | Leaf x -> f res x
      | Tree(_,ta,tb) ->
	  let res = tree_fold_leaf f res ta in
	  tree_fold_leaf f res tb
      end

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

let bdd_diff a b = Cudd.Bdd.dand a (Cudd.Bdd.dnot b)

(*  ====================================================================== *)
(** {3 Iterators} *)
(*  ====================================================================== *)

let fold2 f res list1 list2 =
  List.fold_left
    (begin fun res elt1 ->
      List.fold_left
	(begin fun res elt2 ->
	  f res elt1 elt2
	end)
	res
	list2
    end)
    res
    list1
let iter2 f list1 list2 =
  List.iter
    (begin fun elt1 ->
      List.iter
	(begin fun elt2 ->
	  f elt1 elt2
	end)
	list2
    end)
    list1

(*
let meet_bdd man t bdd =
  let nlist =
    List.fold_left
      (begin fun res elt ->
	let nguard = Cudd.Bdd.dand elt.guard bdd in
	if Cudd.Bdd.is_false nguard then
	  res
	else
	  { guard = nguard; abs=elt.abs }::res
      end)
      []
      t.list
  in
  { t with
    list = nlist;
    bottom = { t.bottom with
      guard = Cudd.Bdd.dor t.bottom.guard (Cudd.Bdd.dnot bdd) }
  }
*)

(*  ====================================================================== *)
(** {3 Elements} *)
(*  ====================================================================== *)

module Elt = struct

  let abs_meet_cubecond man env cond abs cubecond =
    let apron_env = env#apron_env in
    let lidcondb = Cudd.Bdd.list_of_cube cubecond in
    let tidcondb = Array.of_list lidcondb in
    let ttcons0 =
      Array.map
	(begin fun idb ->
	  let `Apron condition = cond#cond_of_idb idb in
	    let tcons0 = Apronexpr.Condition.to_tcons0 apron_env condition in
	    tcons0
	end)
	tidcondb
    in
    Apron.Abstract0.meet_tcons_array man.apron abs ttcons0

  let meet_cube man env cond elt cube =
    let supp = Cudd.Bdd.support cube in
    let suppbool = Cudd.Bdd.support_diff supp cond#cond_supp in
    let cubecond = Cudd.Bdd.exist suppbool cube in
    let cubebool = Cudd.Bdd.cofactor cube cubecond in
    let nguard = Cudd.Bdd.dand elt.guard cubebool in
    let elt = {
      guard = nguard;
      abs =
	if Cudd.Bdd.is_cst cubecond || Cudd.Bdd.is_false nguard then
	  elt.abs
	else
	  abs_meet_cubecond man env cond elt.abs cubecond
    }
    in
    elt

  let is_bottom man elt =
    Cudd.Bdd.is_false elt.guard || Apron.Abstract0.is_bottom man.apron elt.abs

  (* bdd may contain constraints. *)

  let meet_bdd ~depth man env cond elt bdd =
    let cudd = cond#cudd in
    let cond_supp = cond#cond_supp in

    let rec descend depth elt bdd =
      if Cudd.Bdd.is_inter_empty elt.guard bdd then
	Nil
      else begin
	let cube = Cudd.Bdd.cube_of_bdd bdd in
	let (elt,bdd) =
	  if Cudd.Bdd.is_cst cube then
	    (elt,bdd)
	  else
	    (meet_cube man env cond elt cube, Cudd.Bdd.cofactor bdd cube)
	in
	if is_bottom man elt then
	  Nil
	else begin
	  let supp = Cudd.Bdd.support bdd in
	  let suppcond = Cudd.Bdd.support_inter supp cond_supp in
	  if Cudd.Bdd.is_cst suppcond then begin
	    let elt = { elt with guard = Cudd.Bdd.dand elt.guard bdd } in
	    Node(cube,Leaf(elt))
	  end
	  else if depth<=0 then begin
	    let bdd = Cudd.Bdd.exist cond_supp bdd in
	    let elt = { elt with guard = Cudd.Bdd.dand elt.guard bdd } in
	    Node(cube,Leaf(elt))
	  end
	  else begin
	    let topcond = Cudd.Bdd.topvar suppcond in
	    let conda = Cudd.Bdd.ithvar cudd topcond in
	    let condb = Cudd.Bdd.dnot conda in
	    let bdda = Cudd.Bdd.dand bdd conda in
	    let bddb = Cudd.Bdd.dand bdd condb in
	    let resa = descend (depth-1) elt bdda in
	    let resb = descend (depth-1) elt bddb in
	    match (resa,resb) with
	    | (Nil, x) | (x,Nil) -> x
	    | (Node(cubea,nodea), Node(cubeb,nodeb)) ->
		let nodea = Node( (Cudd.Bdd.cofactor cubea conda), nodea ) in
		let nodeb = Node( (Cudd.Bdd.cofactor cubeb condb), nodeb ) in
		Node(cube,Tree(topcond,nodea,nodeb))
	  end
	end
      end
    in
    let res = descend depth elt bdd in
    res

(*
  let descend_texpr ~depth man env cond elt texpr =
    let cudd = cond#cudd in
    let cond_supp = cond#cond_supp in

    let rec descend depth elt texpr =
      if is_bottom man elt then
	Nil
      else begin
	let supp = Domain0.O.Descend.texpr_support cond texpr in
	

      if Cudd.Bdd.is_inter_empty elt.guard bdd then
	Nil
      else begin
	let cube = Cudd.Bdd.cube_of_bdd bdd in
	let (elt,bdd) =
	  if Cudd.Bdd.is_cst cube then
	    (elt,bdd)
	  else
	    (meet_cube man env cond elt cube, Cudd.Bdd.cofactor bdd cube)
	in
	if is_bottom man elt then
	  Nil
	else begin
	  let supp = Cudd.Bdd.support bdd in
	  let suppcond = Cudd.Bdd.support_inter supp cond_supp in
	  if Cudd.Bdd.is_cst suppcond then begin
	    let elt = { elt with guard = Cudd.Bdd.dand elt.guard bdd } in
	    Node(cube,Leaf(elt))
	  end
	  else if depth<=0 then begin
	    let bdd = Bdd.exist cond_supp bdd in
	    let elt = { elt with guard = Cudd.Bdd.dand elt.guard bdd } in
	    Node(cube,Leaf(elt))
	  end
	  else begin
	    let topcond = Cudd.Bdd.topvar suppcond in
	    let conda = Cudd.Bdd.ithvar cudd topcond in
	    let condb = Cudd.Bdd.dnot conda in
	    let bdda = Cudd.Bdd.dand bdd conda in
	    let bddb = Cudd.Bdd.dand bdd condb in
	    let resa = descend (depth-1) elt bdda in
	    let resb = descend (depth-1) elt bddb in
	    match (resa,resb) with
	    | (Nil, x) | (x,Nil) -> x
	    | (Node(cubea,nodea), Node(cubeb,nodeb)) ->
		let nodea = Node( (Cudd.Bdd.cofactor cubea conda), nodea ) in
		let nodeb = Node( (Cudd.Bdd.cofactor cubeb condb), nodeb ) in
		Node(cube,Tree(topcond,nodea,nodeb)
	  end
	end
      end
    in
    let res = descend depth elt bdd in
    res
*)
end

(*  ====================================================================== *)
(** {3 Lists} *)
(*  ====================================================================== *)

module ListE = struct

  (* Checking function: raises [Failure] if problem, returns [true]
     otherwise.

     Checks that
     - no guard is false
     - no abstract value is bottom
     - no doublons of abstract values
  *)
  let check_doublons man list =
    let rec check = function
      | [] -> true
      | elt::list ->
	  if Cudd.Bdd.is_false elt.guard then
	    failwith "Bddapron.Domain0.check_doublons: false guard in a list"
	  ;
	  if Apron.Abstract0.is_bottom man.apron elt.abs then
	    failwith "Bddapron.Domain0.check_doublons: empty APRON value in a list";
	  List.iter
	    (begin fun elt2 ->
	      if Apron.Abstract0.is_eq man.apron elt.abs elt2.abs then
		failwith "Bddapron.Domain0.check_doublons: two equal APRON values in a canonicalized abstract value."
	      ;
	    end)
	    list
	  ;
	  check list
    in
    check list

  (* Checking function: raises [Failure] if problem, returns [true]
     otherwise. Checks that the guards are exclusive. *)
  let check_exclusive man list =
    let rec check = function
      | [] -> true
      | elt::list ->
	  List.iter
	    (begin fun elt2 ->
	      if not (Cudd.Bdd.is_inter_empty elt.guard elt2.guard) then
		failwith "Bddapron.Domain0: two non disjoint guards in a canonicalized abstract value.";
	    end)
	    list
	  ;
	  check list
    in
    check list

  (* Performs the join of a list with an element *)
  let join_nodoublon man list2 elt1 =
    let rec join_nodoublon res list2 = match list2 with
      | [] -> elt1::res
      | elt2::tail2 ->
	  if Apron.Abstract0.is_eq man.apron elt1.abs elt2.abs then begin
	    let nelt2 = { elt2 with
	      guard = Cudd.Bdd.dor elt1.guard elt2.guard
	    } in
	    nelt2 :: (List.rev_append res tail2)
	  end
	  else
	    join_nodoublon (elt2::res) tail2
    in
    join_nodoublon [] list2

  (* Performs the join of a list with an element *)
  let join_exclusive man list2 elt1 =
    let (nlist2,nlist12,nguard1) =
      List.fold_left
	(begin fun (list2,list12,guard1) elt2 ->
	   if Cudd.Bdd.is_inter_empty guard1 elt2.guard then
	     (elt2::list2, list12, guard1)
	   else begin
	     let nguard1 = bdd_diff guard1 elt2.guard in
	     let nguard2 = bdd_diff elt2.guard guard1 in
	     let nguard12 =
	       if Cudd.Bdd.is_false nguard1 then guard1
	       else if Cudd.Bdd.is_false nguard2 then elt2.guard
	       else Cudd.Bdd.dand guard1 elt2.guard
	     in
	     let nlist2 =
	       if Cudd.Bdd.is_false nguard2 then
		 list2
	       else
		 { elt2 with guard = nguard2 } :: list2
	     in
	     let nlist12 =
	       if Cudd.Bdd.is_false nguard12 then
		 list12
	       else
		 join_nodoublon man list12
		   {
		     guard = nguard12;
		     abs = Apron.Abstract0.join man.apron elt1.abs elt2.abs
		   }
	     in
	     (nlist2,nlist12,nguard1)
	   end
	end)
	([],[],elt1.guard)
	list2
    in
    let res =
      List.fold_left
	(begin fun res elt12 ->
	  join_nodoublon man res elt12
	end)
	nlist2
	nlist12
    in
    if Cudd.Bdd.is_false nguard1 then
      res
    else
      join_nodoublon man res
	{ elt1 with guard = nguard1 }

  let join ~nodoublon ~exclusive  man list2 elt1 =
    assert(if exclusive then nodoublon else true);
    if exclusive then
      join_exclusive man list2 elt1
    else if nodoublon then
      join_nodoublon man list2 elt1
    else
      elt1::list2

  (* Remove doublons (by reconstructing the list) *)
  let merge_doublons ~exclusive man list =
    let nlist =
      if exclusive then
	List.fold_left
	  (begin fun res elt -> join_exclusive man res elt end)
	  [] list
      else
	List.fold_left
	(begin fun res elt -> join_nodoublon man res elt end)
	  []
	  list
    in
    nlist

end


(*  ********************************************************************** *)
(** {2 Checking functions} *)
(*  ********************************************************************** *)


(* Checking function: raises [Failure] if problem, returns [true]
   otherwise. Checks that

   - no guard is false
   - no abstract value is bottom
   - the guard of bottom is the negation of all the other guards
   - the guards are exclusive
*)
let check_wellformed man t =
  ignore (if t.nodoublon then ListE.check_doublons man t.list else true);
  let guardofnonbottom =
    let cudd = Cudd.Bdd.manager t.bottom.guard in
    List.fold_left
      (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
     (Cudd.Bdd.dfalse cudd) t.list
  in
  if not (Cudd.Bdd.is_equal t.bottom.guard (Cudd.Bdd.dnot guardofnonbottom)) then
    failwith "The guard of bottom is not the complement of the other guards"
  ;
  if t.exclusive then ListE.check_exclusive man t.list else true

(*  ********************************************************************** *)
(** {2 API} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Representation and constructors} *)
(*  ====================================================================== *)

let canonicalize ?(nodoublon=true) ?(exclusive=true) man t =
  assert(if exclusive then nodoublon else true);
  if nodoublon && not t.nodoublon ||
    exclusive && not t.exclusive
  then begin
    t.list <- ListE.merge_doublons ~exclusive man t.list;
    t.nodoublon <- true;
    t.exclusive <- true
  end;
  assert(check_wellformed man t);
  ()

let size man t =
  List.fold_left
    (begin fun size elt ->
      size + (Cudd.Bdd.size elt.guard) + (Apron.Abstract0.size man.apron elt.abs)
    end)
    (Cudd.Bdd.size t.bottom.guard)
    t.list

let print env fmt t =
  if t.list=[] then
    pp_print_string fmt "bottom"
  else
    Print.list
      ~first:"{ @[<v>" ~sep:" or@," ~last:"@] }"
      (begin fun fmt elt ->
	fprintf fmt "@[<hv>(%a) and@ %a@]"
	  (Bdd.Expr0.O.print_bdd env)
	  elt.guard
	  Apron.Abstract1.print
	  {
	    Apron.Abstract1.abstract0 = elt.abs;
	    Apron.Abstract1.env = env#apron_env
	  }
      end)
      fmt
      t.list

let abs_of_env bottom_or_top man env =
  let dim = Apron.Environment.dimension env#apron_env in
  bottom_or_top man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald

let bottom man env =
  {
    list = [];
    bottom = {
      guard = Cudd.Bdd.dtrue env#cudd;
      abs = abs_of_env Apron.Abstract0.bottom man env
    };
    nodoublon = true;
    exclusive = true;
  }
let top man env =
  {
    list = [{
      guard = Cudd.Bdd.dtrue env#cudd;
      abs = abs_of_env Apron.Abstract0.top man env
    }];
    bottom = {
      guard = Cudd.Bdd.dfalse env#cudd;
      abs = abs_of_env Apron.Abstract0.bottom man env
    };
    nodoublon = true;
    exclusive = true;
  }

(*  ====================================================================== *)
(** {3 Tests} *)
(*  ====================================================================== *)

let is_bottom man t =
  Cudd.Bdd.is_true t.bottom.guard

let is_top man t =
  Cudd.Bdd.is_false t.bottom.guard && begin
    canonicalize man t;
    match t.list with
    | [elt] when Apron.Abstract0.is_top man.apron elt.abs ->
	assert (Cudd.Bdd.is_true elt.guard);
	true
    | _ ->
	false
  end

let is_leq man t1 t2 =
  Cudd.Bdd.is_included_in t2.bottom.guard t1.bottom.guard && begin
    canonicalize man t2;
    begin try
      iter2
	(begin fun elt2 elt1 ->
	  if not (Cudd.Bdd.is_inter_empty elt2.guard elt1.guard) &&
	    not (Apron.Abstract0.is_leq man.apron elt1.abs elt2.abs)
	  then
	    raise Exit
	end)
	t2.list t1.list
      ;
      true
    with Exit ->
      false
    end
  end

let is_eq man t1 t2 =
  Cudd.Bdd.is_equal t1.bottom.guard t2.bottom.guard && begin
    canonicalize man t1;
    canonicalize man t2;
    (List.length t1.list) = (List.length t2.list) && begin
      let cmp_elt elt1 elt2 = Pervasives.compare elt1.guard elt2.guard in
      let list1 = List.fast_sort cmp_elt t1.list in
      let list2 = List.fast_sort cmp_elt t2.list in
      begin try
	List.iter2
	  (begin fun elt1 elt2 ->
	    if not (
	      (Cudd.Bdd.is_equal elt1.guard elt2.guard) &&
		(Apron.Abstract0.is_eq man.apron elt1.abs elt2.abs))
	    then
	      raise Exit
	  end)
	  list1 list2
	;
	true
      with Exit ->
	false
      end
    end
  end

(*  ====================================================================== *)
(** {3 Meet and join} *)
(*  ====================================================================== *)

let meet_internal ~exclusive man t1 t2 =
  if Cudd.Bdd.is_inter_empty
    (Cudd.Bdd.dnot t1.bottom.guard)
    (Cudd.Bdd.dnot t2.bottom.guard)
  then begin
    let cudd = Cudd.Bdd.manager t1.bottom.guard in {
      list = [];
      bottom = { guard = Cudd.Bdd.dtrue cudd; abs = t1.bottom.abs };
      nodoublon = true;
      exclusive = true;
    }
  end
  else begin
    canonicalize ~nodoublon:true ~exclusive man t1;
    canonicalize ~nodoublon:true ~exclusive man t2;
    let nlist = fold2
      (begin fun res elt1 elt2 ->
	let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	if Cudd.Bdd.is_false nguard then
	  res
	else begin
	  let nabs = Apron.Abstract0.meet man.apron elt1.abs elt2.abs in
	  if Apron.Abstract0.is_bottom man.apron nabs then begin
	    res
	  end
	  else begin
	    let nelt = { guard = nguard; abs = nabs } in
	    if t1.exclusive && t2.exclusive then
	      ListE.join_nodoublon man res nelt
	    else
	      ListE.join ~nodoublon:true ~exclusive man res nelt
	  end
	end
      end)
      []
      t1.list t2.list
    in
    let nbottom =
      let guardnonbottom =
	List.fold_left
	  (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
	  (Cudd.Bdd.dfalse (Cudd.Bdd.manager t1.bottom.guard))
	  nlist
      in
      { t1.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
    in
    {
      list = nlist;
      bottom = nbottom;
      nodoublon = true;
      exclusive = exclusive || (t1.exclusive && t2.exclusive);
    }
  end

let meet man t1 t2 =
  let res = meet_internal ~exclusive:man.meet_exclusive man t1 t2 in
  assert(check_wellformed man res);
  res

let join_internal ~exclusive man t1 t2 =
  canonicalize ~nodoublon:true ~exclusive man t1;
  canonicalize ~nodoublon:true ~exclusive man t2;
  if is_bottom man t1 then t2
  else if is_bottom man t2 then t1
  else begin
    let nbottom = { t1.bottom with
      guard = Cudd.Bdd.dand t1.bottom.guard t2.bottom.guard
    }
    in
    let myjoin =
      if t1.exclusive && t2.exclusive
      then ListE.join_nodoublon man
      else ListE.join ~nodoublon:true ~exclusive man
    in
    let nlist =
      fold2
	(begin fun res elt1 elt2 ->
	  let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	  if Cudd.Bdd.is_false nguard then
	    res
	  else begin
	    let nabs = Apron.Abstract0.join man.apron elt1.abs elt2.abs in
	    let nelt = { guard = nguard; abs = nabs } in
	    myjoin res nelt
	  end
	end)
	[]
	t1.list t2.list
    in
    let nlist =
      List.fold_left
	(begin fun res elt1 ->
	  let nguard = Cudd.Bdd.dand elt1.guard t2.bottom.guard in
	  if  Cudd.Bdd.is_false nguard then
	    res
	  else
	    let nelt = { elt1 with guard = nguard } in
	    myjoin res nelt
	end)
	nlist t1.list
    in
    let nlist =
      List.fold_left
	(begin fun res elt2 ->
	  let nguard = Cudd.Bdd.dand elt2.guard t1.bottom.guard in
	  if  Cudd.Bdd.is_false nguard then
	    res
	  else
	    let nelt = { elt2 with guard = nguard } in
	    myjoin res nelt
	end)
	nlist t2.list
    in
    {
      list = nlist;
      bottom = nbottom;
      nodoublon = true;
      exclusive = exclusive || (t1.exclusive && t2.exclusive);
    }
  end

let join man t1 t2 =
  let res = join_internal ~exclusive:man.join_exclusive man t1 t2 in
  assert(check_wellformed man res);
  res

(*  ====================================================================== *)
(** {3 Meet with a guard } *)
(*  ====================================================================== *)

let meet_cond_internal ~nodoublon ~exclusive ~depth man env cond t bdd =
  assert(if exclusive then nodoublon else true);
  let nlist =
    List.fold_left
      (begin fun res elt ->
	if Cudd.Bdd.is_inter_empty elt.guard bdd then
	  res
	else begin
	  let bdd = man.bddrestrict bdd elt.guard in
	  let tree = Elt.meet_bdd ~depth man env cond elt bdd in
	  if exclusive then
	    if t.exclusive then
	      let list = tree_fold_leaf (ListE.join_exclusive man) [] tree in
	      List.fold_left (ListE.join_nodoublon man) res list
	    else
	      tree_fold_leaf (ListE.join_exclusive man) res tree
	  else
	    tree_fold_leaf (ListE.join ~nodoublon ~exclusive:false man) res tree
	end
      end)
      []
      t.list
  in
  let nbottom =
    let guardnonbottom =
      List.fold_left
	(begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
	(Cudd.Bdd.dfalse (Cudd.Bdd.manager t.bottom.guard))
	nlist
    in
    { t.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
  in
  {
    list = nlist;
    bottom = nbottom;
    exclusive = exclusive;
    nodoublon = nodoublon;
  }

let meet_cond man (env:('b,'c) #Env.O.t as 'd) (cond:(Cond.cond,'d) #Cond.O.t) t bdd =
  let res =
    meet_cond_internal
      ~nodoublon:man.meet_cond_nodoublon
      ~exclusive:man.meet_cond_exclusive
      ~depth:man.meet_cond_depth
      man env cond t bdd
  in
  assert(check_wellformed man res);
  res

(*  ====================================================================== *)
(** {3 Assignement/Substitution} *)
(*  ====================================================================== *)
