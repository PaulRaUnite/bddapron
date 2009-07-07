(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format
open Env
open Bddleaf

type 'a man = {
  apron : 'a Apron.Manager.t;
  mutable bdd_restrict : Cudd.Bdd.vt -> Cudd.Bdd.vt -> Cudd.Bdd.vt;
  mutable expr_restrict : Expr0.t -> Cudd.Bdd.vt -> Expr0.t;
  mutable meet_disjoint : bool;
  mutable join_disjoint : bool;
  mutable meet_cond_unique : bool;
  mutable meet_cond_disjoint : bool;
  mutable meet_cond_depth : int;
  mutable assign_unique : bool;
  mutable assign_disjoint : bool;
  mutable substitute_unique : bool;
  mutable substitute_disjoint : bool;
  mutable forget_unique : bool;
  mutable forget_disjoint : bool;
  mutable change_environment_unique : bool;
  mutable change_environment_disjoint : bool;
}

type 'a elt = (Cudd.Man.v, 'a Apron.Abstract0.t) Bddleaf.elt

type 'a t = {
  mutable list : 'a elt list;
  bottom : 'a elt;
  mutable unique : bool;
  mutable disjoint : bool;
}

module O = struct

  (*  ******************************************************************** *)
  (** {2 Functions on lists of elements} *)
  (*  ******************************************************************** *)

  module L = struct

    (* Checking function: raises [Failure] if problem, returns
       [true] otherwise.

       Checks that
       - no guard is false
       - no abstract value is bottom
       - no doublons of abstract values
    *)
    let check_unicity man list =
      ignore (
	Bddleaf.check_unicity
	  ~is_equal:(Apron.Abstract0.is_eq man.apron)
	  list
      );
      List.iter
	(begin fun elt ->
	  if Apron.Abstract0.is_bottom man.apron elt.leaf then
	    failwith "Bddapron.Bdddomain0.check_unicity: empty APRON value in a list";
	end)
	list;
      true

    (* Checking function: raises [Failure] if problem, returns
       [true] otherwise. Checks that the guards are disjoint. *)
    let check_disjointness man list =
      Bddleaf.check_disjointness list

    (* Performs the join of a list with an element *)
    let cons_unique man elt1 list2 =
      Bddleaf.cons_unique
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	elt1 list2
    let cons_disjoint man elt1 list2 =
      Bddleaf.cons_disjoint
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	~merge:(Apron.Abstract0.join man.apron)
	elt1 list2
    let cons ~disjoint ~unique man elt1 list2 =
      Bddleaf.cons
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	~merge:(Apron.Abstract0.join man.apron)
	~disjoint ~unique
	elt1 list2
    let append_unique man list1 list2 =
      Bddleaf.append_unique
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	list1 list2
    let append_disjoint man list1 list2 =
      Bddleaf.append_disjoint
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	~merge:(Apron.Abstract0.join man.apron)
	list1 list2
    let append ~disjoint ~unique man list1 list2 =
      Bddleaf.append
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	~merge:(Apron.Abstract0.join man.apron)
	~disjoint ~unique
	list1 list2
    let merge_disjoint man list1 list2 =
      if (List.length list1) >= (List.length list2)
      then append_disjoint man list1 list2
      else append_disjoint man list2 list1
    let merge ~disjoint ~unique man list1 list2 =
      if disjoint && (List.length list1) >= (List.length list2)
      then append ~disjoint ~unique man list1 list2
      else append ~disjoint ~unique man list2 list1

    let make_unique ~disjoint man list =
      Bddleaf.make_unique
	~is_equal:(Apron.Abstract0.is_eq man.apron)
	~merge:(Apron.Abstract0.join man.apron)
	~disjoint
	list
  end


  (*  ******************************************************************** *)
  (** {2 Checking functions} *)
  (*  ******************************************************************** *)

  (* Checking function: raises [Failure] if problem, returns [true]
     otherwise. Checks that

     - no guard is false
     - no abstract value is bottom
     - the guard of bottom is the negation of all the other guards
     - the guards are disjoint
  *)
  let check_wellformed man t =
    ignore (if t.unique then L.check_unicity man t.list else true);
    let cudd = Cudd.Bdd.manager t.bottom.guard in
    let guardofnonbottom = Bddleaf.guard ~cudd t.list in
    if not (Cudd.Bdd.is_equal t.bottom.guard (Cudd.Bdd.dnot guardofnonbottom)) then
      failwith "The guard of bottom is not the complement of the other guards"
    ;
    if t.disjoint then L.check_disjointness man t.list else true

  (*  ******************************************************************** *)
  (** {2 Representation} *)
  (*  ******************************************************************** *)

  let canonicalize ?(apron=false) ?(unique=true) ?(disjoint=true) man t =
    assert(if disjoint then unique else true);
    if unique && not t.unique ||
      disjoint && not t.disjoint
    then begin
      t.list <- L.make_unique ~disjoint man t.list;
      t.unique <- true;
      t.disjoint <- t.disjoint || disjoint;
    end;
    assert(check_wellformed man t);
    if apron then
      List.iter 
	(fun elt -> Apron.Abstract0.canonicalize man.apron elt.leaf) 
	t.list;
    ()

  let size man t =
    List.fold_left
      (begin fun size elt ->
	size + (Cudd.Bdd.size elt.guard) + (Apron.Abstract0.size man.apron elt.leaf)
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
	      Apron.Abstract1.abstract0 = elt.leaf;
	      Apron.Abstract1.env = env#apron_env
	    }
	end)
	fmt
	t.list

  (*  ******************************************************************** *)
  (** {2 Constructors, accessors, tests, property extraction} *)
  (*  ******************************************************************** *)

  (*  ==================================================================== *)
  (** {3 Basic constructors} *)
  (*  ==================================================================== *)

  let abs_of_env bottom_or_top man env =
    let dim = Apron.Environment.dimension env#apron_env in
    bottom_or_top man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald

  let bottom man env =
    {
      list = [];
      bottom = {
	guard = Cudd.Bdd.dtrue env#cudd;
	leaf = abs_of_env Apron.Abstract0.bottom man env
      };
      unique = true;
      disjoint = true;
    }
  let top man env =
    let cudd = env#cudd in
    {
      list = [{
	guard = Cudd.Bdd.dtrue cudd;
	leaf = abs_of_env Apron.Abstract0.top man env;
      }];
      bottom = {
	guard = Cudd.Bdd.dfalse cudd;
	leaf = abs_of_env Apron.Abstract0.bottom man env;
      };
      unique = true;
      disjoint = true;
    }

  let of_apron man env abs0 =
    let cudd = env#cudd in
    if Apron.Abstract0.is_bottom man.apron abs0
    then
      {
	list = [];
	bottom = {
	  guard = Cudd.Bdd.dtrue env#cudd;
	  leaf = abs0;
	};
	unique = true;
	disjoint = true;
      }
    else
      {
	list = [{
	  guard = Cudd.Bdd.dtrue cudd;
	  leaf = abs0;
	}];
	bottom = {
	  guard = Cudd.Bdd.dfalse cudd;
	  leaf = abs_of_env Apron.Abstract0.bottom man env;
	};
	unique = true;
	disjoint = true;
      }

  (*  ==================================================================== *)
  (** {3 Tests} *)
  (*  ==================================================================== *)

  let is_bottom man t =
    Cudd.Bdd.is_true t.bottom.guard

  let is_top man t =
    Cudd.Bdd.is_false t.bottom.guard && begin
      canonicalize man t;
      match t.list with
      | [elt] when Apron.Abstract0.is_top man.apron elt.leaf ->
	  assert (Cudd.Bdd.is_true elt.guard);
	  true
      | _ ->
	  false
    end

  let is_leq man t1 t2 =
    Cudd.Bdd.is_included_in t2.bottom.guard t1.bottom.guard && begin
      canonicalize man t2;
      begin try
	Bddleaf.iter2
	  (begin fun elt2 elt1 ->
	    if not (Cudd.Bdd.is_inter_empty elt2.guard elt1.guard) &&
	      not (Apron.Abstract0.is_leq man.apron elt1.leaf elt2.leaf)
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
		  (Apron.Abstract0.is_eq man.apron elt1.leaf elt2.leaf))
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

  (*  ==================================================================== *)
  (** {3 Extractors} *)
  (*  ==================================================================== *)

  let to_bddapron man t = Obj.magic t.list

  (*  ******************************************************************** *)
  (** {2 Meet, join and widening} *)
  (*  ******************************************************************** *)

  let meet_internal ~disjoint man t1 t2 =
    let cudd = Cudd.Bdd.manager t1.bottom.guard in
    if Cudd.Bdd.is_inter_empty
      (Cudd.Bdd.dnot t1.bottom.guard)
      (Cudd.Bdd.dnot t2.bottom.guard)
    then begin
      {
	list = [];
	bottom = { guard = Cudd.Bdd.dtrue cudd; leaf = t1.bottom.leaf };
	unique = true;
	disjoint = true;
      }
    end
    else begin
      canonicalize ~unique:true ~disjoint man t1;
      canonicalize ~unique:true ~disjoint man t2;
      let nlist = fold2
	(begin fun res elt1 elt2 ->
	  let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	  if Cudd.Bdd.is_false nguard then
	    res
	  else begin
	    let nleaf = Apron.Abstract0.meet man.apron elt1.leaf elt2.leaf in
	    if Apron.Abstract0.is_bottom man.apron nleaf then begin
	      res
	    end
	    else begin
	      let nelt = { guard = nguard; leaf = nleaf } in
	      if t1.disjoint && t2.disjoint then
		L.cons_unique man nelt res
	      else
		L.cons ~unique:true ~disjoint man nelt res
	    end
	  end
	end)
	[]
	t1.list t2.list
      in
      let nbottom =
	let guardnonbottom = Bddleaf.guard ~cudd nlist in
	{ t1.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
      in
      {
	list = nlist;
	bottom = nbottom;
	unique = true;
	disjoint = disjoint || (t1.disjoint && t2.disjoint);
      }
    end

  let meet man t1 t2 =
    let res = meet_internal ~disjoint:man.meet_disjoint man t1 t2 in
    assert(check_wellformed man res);
    res

  let join_internal ~disjoint man t1 t2 =
    canonicalize ~unique:true ~disjoint man t1;
    canonicalize ~unique:true ~disjoint man t2;
    if is_bottom man t1 then t2
    else if is_bottom man t2 then t1
    else begin
      let mycons =
	if t1.disjoint && t2.disjoint
	then L.cons_unique man
	else L.cons ~unique:true ~disjoint man
      in
      let nlist =
	fold2
	  (begin fun res elt1 elt2 ->
	    let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else begin
	      let nleaf = Apron.Abstract0.join man.apron elt1.leaf elt2.leaf in
	      let nelt = { guard = nguard; leaf = nleaf } in
	      mycons nelt res
	    end
	  end)
	  []
	  t1.list t2.list
      in
      let nlist =
	List.fold_left
	  (begin fun res elt1 ->
	    let nguard = Cudd.Bdd.dand elt1.guard t2.bottom.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else
	      let nelt = { elt1 with guard = nguard } in
	      mycons nelt res
	  end)
	  nlist t1.list
      in
      let nlist =
	List.fold_left
	  (begin fun res elt2 ->
	    let nguard = Cudd.Bdd.dand elt2.guard t1.bottom.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else
	      let nelt = { elt2 with guard = nguard } in
	      mycons nelt res
	  end)
	  nlist t2.list
      in
      {
	list = nlist;
	bottom = { t1.bottom with guard =
	    Cudd.Bdd.dand t1.bottom.guard t2.bottom.guard };
	unique = true;
	disjoint = disjoint || (t1.disjoint && t2.disjoint);
      }
    end

  let join man t1 t2 =
    let res = join_internal ~disjoint:man.join_disjoint man t1 t2 in
    assert(check_wellformed man res);
    res

  let widening man t1 t2 =
    canonicalize ~unique:true ~disjoint:true man t1;
    canonicalize ~unique:true ~disjoint:true man t2;
    if is_bottom man t1 then t2
    else begin
      if is_bottom man t2 then failwith "Bddapron.widening: second value is bottom";
      let nlist =
	fold2
	  (begin fun res elt1 elt2 ->
	    let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else begin
	      let nleaf = Apron.Abstract0.widening man.apron elt1.leaf elt2.leaf in
	      let nelt = { guard = nguard; leaf = nleaf } in
	      L.cons_unique man nelt res
	    end
	  end)
	  []
	  t1.list t2.list
      in
      List.iter
	(begin fun elt1 ->
	  if not (Cudd.Bdd.is_inter_empty elt1.guard t2.bottom.guard) then
	    failwith "Bddapron.widening: second value does not contain first value"
	end)
	t1.list
      ;
      let nlist =
	List.fold_left
	  (begin fun res elt2 ->
	    let nguard = Cudd.Bdd.dand elt2.guard t1.bottom.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else
	      let nelt = { elt2 with guard = nguard } in
	      L.cons_unique man nelt res
	  end)
	  nlist t2.list
      in
      let res =
	{
	  list = nlist;
	  bottom = { t1.bottom with guard =
	      Cudd.Bdd.dand t1.bottom.guard t2.bottom.guard };
	  unique = true;
	  disjoint = true;
	}
      in
      assert(check_wellformed man res);
      res
    end

  (*  ******************************************************************** *)
  (** {2 Intersection with a guard} *)
  (*  ******************************************************************** *)

  let elt_is_bottom apron elt =
    Cudd.Bdd.is_false elt.guard ||
      Apron.Abstract0.is_bottom apron elt.leaf

  let descend_merge ~merge (ga,lista) (gb,listb) =
    let g = Cudd.Bdd.dor ga gb in
    let list = merge lista listb in
    (g,list)

  (** Intersection of an element with a cube of conditions *)
  let abs_meet_cubecond
      apron
      (env:('a,'b) #Env.O.t as 'c)
      (cond:(Cond.cond,'c) #Cond.O.t)
      abs (cubecond:Cudd.Bdd.vt)
      =
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
    Apron.Abstract0.meet_tcons_array apron abs ttcons0

  (** Intersection of an element with a general cube *)
  let elt_meet_cube apron env cond (elt:'a elt) cube =
    let supp = Cudd.Bdd.support cube in
    let suppbool = Cudd.Bdd.support_diff supp cond#cond_supp in
    let cubecond = Cudd.Bdd.exist suppbool cube in
    let cubebool = Cudd.Bdd.cofactor cube cubecond in
    let nguard = Cudd.Bdd.dand elt.guard cubebool in
    let nelt = {
      guard = nguard;
      leaf =
	if Cudd.Bdd.is_cst cubecond || Cudd.Bdd.is_false nguard then
	  elt.leaf
	else
	  abs_meet_cubecond apron env cond elt.leaf cubecond
    }
    in
    nelt

  (** bdd may contain constraints. *)
  let elt_descend_bdd ~merge ~maxdepth apron env cond =
    let cudd = env#cudd in
    let cond_supp = cond#cond_supp in
    Descend.descend
      ~cudd ~maxdepth
      ~nocare:(begin fun (elt,bdd) ->
	Cudd.Bdd.is_false bdd || elt_is_bottom apron elt
      end)
      ~cube_of_down:(fun (elt,bdd) -> Cudd.Bdd.cube_of_bdd bdd)
      ~cofactor:(fun (elt,bdd) cube ->
	let nelt = elt_meet_cube apron env cond elt cube in
	(nelt, Cudd.Bdd.cofactor bdd cube)
      )
      ~select:(fun (elt,bdd) ->
	let supp = Cudd.Bdd.support bdd in
	let suppcond = Cudd.Bdd.support_inter supp cond_supp in
	if Cudd.Bdd.is_cst suppcond
	then -1
	else Cudd.Bdd.topvar suppcond
      )
      ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	match (dthen,delse) with
	| None,x | x,None -> x
	| Some(glista),Some(glistb) ->
	    Some(descend_merge ~merge glista glistb)
      )

  (* main function *)
  let meet_cond_internal ~unique ~disjoint ~maxdepth
      (man:'a man) env cond (t:'a t) (bdd:Cudd.Bdd.vt)
      =
    assert(if disjoint then unique else true);
    let cudd = Cudd.Bdd.manager t.bottom.guard in
    let dtrue = Cudd.Bdd.dtrue cudd in
    let cond_supp = cond#cond_supp in
    let supp = Cudd.Bdd.support_inter (Cudd.Bdd.support bdd) cond_supp in
    if Cudd.Bdd.is_true supp then begin
      let nlist =
	List.fold_left
	  (begin fun res elt ->
	    let nguard = Cudd.Bdd.dand elt.guard bdd in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else
	      { elt with guard=nguard } :: res
	  end)
	  [] t.list
      in
      {
	t with
	  list = nlist;
	  bottom = {
	    t.bottom with
	      guard = Cudd.Bdd.dnot (Bddleaf.guard ~cudd nlist) }
      }
    end
    else
      let nlist =
	List.fold_left
	  (begin fun res elt ->
	    if Cudd.Bdd.is_inter_empty elt.guard bdd then
	      res
	    else begin
	      let bdd = man.bdd_restrict bdd elt.guard in
	      let oglist =
		elt_descend_bdd
		  ~maxdepth
		  ~merge:(L.merge ~unique ~disjoint man)
		  ~terminal:(fun ~depth ~newcube ~cube ~down ->
		    let (elt,bdd) = down in
		    let nbdd = if depth<max_int then Cudd.Bdd.exist cond_supp bdd else bdd in
		    let nelt = {
		      elt with
			guard = Cudd.Bdd.dand elt.guard nbdd }
		    in
		    if elt_is_bottom man.apron nelt
		    then None
		    else Some(dtrue,[nelt])
		  )
		  ~down:(elt,bdd)
		  man.apron env cond
	      in
	      match oglist with
	      | None -> res
	      | Some(g,list) ->
		  if t.disjoint && disjoint then
		    if res=[] then list else L.append_unique man res list
		  else
		    L.append ~unique ~disjoint man res list
	    end
	  end)
	  [] t.list
      in
      let nbottom =
	let guardnonbottom = Bddleaf.guard ~cudd nlist in
	{ t.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
      in
      {
	list = nlist;
	bottom = nbottom;
	disjoint = disjoint;
	unique = unique;
      }

  let meet_condition man (env:('b,'c) #Env.O.t as 'd) (cond:(Cond.cond,'d) #Cond.O.t) t bdd =
    let res =
      meet_cond_internal
	~unique:man.meet_cond_unique
	~disjoint:man.meet_cond_disjoint
	~maxdepth:man.meet_cond_depth
	man env cond t bdd
    in
    assert(check_wellformed man res);
    res

  (*  ******************************************************************** *)
  (** {2 Assignement/Substitution} *)
  (*  ******************************************************************** *)

  let descend_texpr ~merge ~maxdepth ~asssub apron env cond org guard lvar lexpr dest =
    let texpr = Array.of_list lexpr in

    let cudd = env#cudd in
    let apron_env = env#apron_env in
    let tabsorbant = Array.make (Array.length texpr) None in
    let default = (Cudd.Bdd.dfalse env#cudd, []) in
    let guard = Cudd.Bdd.dand guard org.guard in
    let (lbvar,tavar) = Descend.split_lvar lvar lexpr in
    let tadim = Array.map (Apron.Environment.dim_of_var apron_env) tavar in
    let ores =
      Descend.descend
	~cudd ~maxdepth
	~nocare:(fun (guard,elt,texpr) ->
	  Cudd.Bdd.is_false guard || elt_is_bottom apron elt
	)
	~cube_of_down:(fun (guard,elt,texpr) -> Cudd.Bdd.cube_of_bdd guard)
	~cofactor:(fun (guard,elt,texpr) cube ->
	  let nguard = Cudd.Bdd.cofactor guard cube in
	  let nelt = elt_meet_cube apron env cond elt cube in
	  (nguard, nelt, Descend.texpr_cofactor Expr0.cofactor texpr cube)
	)
	~select:(fun (guard,elt,texpr) ->
	  let suppcond =
	    Cudd.Bdd.support_inter
	      cond#cond_supp
	      (Cudd.Bdd.support guard)
	  in
	  let suppcond =
	    if Cudd.Bdd.is_cst suppcond then
	      Descend.texpr_support cond texpr
	    else
	      suppcond
	  in
	  if Cudd.Bdd.is_cst suppcond
	  then -1
	  else Cudd.Bdd.topvar suppcond
	)
	~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	  match (dthen,delse) with
	  | None,x | x,None -> x
	  | Some(glista),Some(glistb) ->
	      Some(descend_merge ~merge glista glistb)
	)
	~terminal:(begin fun ~depth ~newcube ~cube ~down ->
	  assert(depth=max_int);
	  let (guard,org,texpr) = down in
	  let org = { org with guard=guard } in
	  let (lbexpr,taexpr) = Descend.split_texpr texpr in
	  let ((g,list) as glist) =
	    Cudd.Mapleaf.combineleaf_array
	      ~tabsorbant ~default
	      ~combine:(descend_merge ~merge)
	      (begin fun guard taexpr ->
		let org = { org with guard=guard } in
		let taexpr =
		  Array.map
		    (fun aexpr ->
		      Apronexpr.to_texpr0 apron_env (Cudd.Mtbdd.get aexpr))
		    taexpr
		in
		asssub ~merge apron env org lbvar lbexpr tadim taexpr dest
	      end)
	      taexpr
	  in
	  if list=[] then None else Some(glist)
	end)
	~down:(guard,org,texpr)
    in
    ores

  (*  ==================================================================== *)
  (** {3 Assignement} *)
  (*  ==================================================================== *)

  let assign_terminal
      ?relational ?nodependency
      ~merge
      apron env
      (org:'a elt) lbvar lbexpr tadim taexpr (oldest:'a elt list option)
      =
    if Cudd.Bdd.is_false org.guard then
      (org.guard,[])
    else
      let nguard =
	Bdd.Domain0.O.assign_lexpr ?relational ?nodependency env
	  org.guard lbvar lbexpr
      in
      let nlist =
	begin match oldest with
	| None ->
	    let nleaf =
	      Apron.Abstract0.assign_texpr_array
		apron org.leaf tadim taexpr None
	    in
	    [{ guard = nguard; leaf = nleaf }]
	| Some(ldest) ->
	    List.fold_left
	      (begin fun res dest ->
		let nguard = Cudd.Bdd.dand nguard dest.guard in
		if Cudd.Bdd.is_false nguard then
		  res
		else
		  let nleaf =
		    Apron.Abstract0.assign_texpr_array
		      apron org.leaf tadim taexpr (Some dest.leaf)
		  in
		  if Apron.Abstract0.is_bottom apron nleaf
		  then res
		  else merge res [{ guard = nguard; leaf = nleaf }]
	      end)
	      [] ldest
	end
      in
      (Cudd.Bdd.dtrue env#cudd, nlist)

  let assign_internal
      ~(expr_restrict:Expr0.t -> Cudd.Bdd.vt -> Expr0.t)
      ~unique ~disjoint
      ?relational ?nodependency
      man env cond org lvar (lexpr:Expr0.t list) odest
      =
    let cudd = env#cudd in
    let dtrue = Cudd.Bdd.dtrue cudd in
    let oldest = match odest with
      | None -> None
      | Some dest -> Some dest.list
    in
    let nlist =
      List.fold_left
	(begin fun res elt ->
	  let lexpr = List.map (fun e -> expr_restrict e elt.guard) lexpr in
	  let oglist =
	    descend_texpr
	      ~merge:(L.merge ~unique ~disjoint man)
	      ~maxdepth:max_int
	      ~asssub:(assign_terminal ?relational ?nodependency)
	      man.apron env cond elt dtrue lvar lexpr oldest
	  in
	  match oglist with
	  | None -> res
	  | Some(g,list) -> L.merge ~disjoint ~unique man res list
	end)
	[] org.list
    in
    let nbottom =
      let guardnonbottom = Bddleaf.guard ~cudd nlist in
      { org.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
    in
    {
      list = nlist;
      bottom = nbottom;
      disjoint = disjoint;
      unique = unique;
    }

  let assign_lexpr ?relational ?nodependency man env cond org lvar lexpr odest =
    assign_internal
      ~expr_restrict:man.expr_restrict
      ~unique:man.assign_unique
      ~disjoint:man.assign_disjoint
      ?relational ?nodependency
      man env cond org lvar lexpr odest

  (*  ==================================================================== *)
  (** {3 Substitution} *)
  (*  ==================================================================== *)

  let substitute_terminal
      ~merge
      apron env
      org lbvar lbexpr tadim taexpr ldest
      =
    let glist =
      if Cudd.Bdd.is_false org.guard then
	(org.guard,[])
      else
	let dfalse = Cudd.Bdd.dfalse env#cudd in
	List.fold_left
	  (begin fun res dest ->
	    let nguard = Bdd.Domain0.O.substitute_lexpr env dest.guard lbvar lbexpr in
	    let nguard = Cudd.Bdd.dand nguard org.guard in
	    if Cudd.Bdd.is_false nguard then
	      res
	    else
	      let nleaf =
		Apron.Abstract0.substitute_texpr_array
		  apron org.leaf tadim taexpr (Some dest.leaf)
	      in
	      if Apron.Abstract0.is_bottom apron nleaf
	      then res
	      else
		descend_merge ~merge res
		  (nguard,[{ guard = nguard; leaf = nleaf }])
	  end)
	  (dfalse,[]) ldest
    in
    glist

  let substitute_internal
      ~expr_restrict
      ~unique ~disjoint
      man env cond org lvar lexpr odest
      =
    let dest = org in
    let org = match odest with
      | None -> top man env
      | Some dest -> dest
    in
    let bdest = Cudd.Bdd.dnot (dest.bottom.guard) in
    let (lbvar,lbexpr,tavar,taexpr) = Descend.split_lvarlexpr lvar lexpr in
    let nguard = Bdd.Domain0.O.substitute_lexpr env bdest lbvar lbexpr in
    let nlist =
      List.fold_left
	(begin fun res elt ->
	  let nguard = Cudd.Bdd.dand nguard elt.guard in
	  if Cudd.Bdd.is_false nguard then
	    res
	  else begin
	    let lexpr = List.map (fun e -> expr_restrict e elt.guard) lexpr in
	    let oglist =
	      descend_texpr
		~merge:(L.merge ~unique ~disjoint man)
		~maxdepth:max_int
		~asssub:substitute_terminal
		man.apron env cond elt nguard lvar lexpr dest.list
	    in
	    match oglist with
	    | None -> res
	    | Some(g,list) -> L.merge ~disjoint ~unique man res list
	  end
	end)
	[] org.list
    in
    let nbottom =
      let guardnonbottom = Bddleaf.guard ~cudd:env#cudd nlist in
      { org.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
    in
    {
      list = nlist;
      bottom = nbottom;
      disjoint = disjoint;
      unique = unique;
    }

  let substitute_lexpr man env cond org lvar lexpr odest =
    substitute_internal
      ~expr_restrict:man.expr_restrict
      ~unique:man.substitute_unique
      ~disjoint:man.substitute_disjoint
      man env cond org lvar lexpr odest

  (*  ******************************************************************** *)
  (** {2 Forget} *)
  (*  ******************************************************************** *)

  let forget_list man env t lvar =
    if lvar=[] then t
    else begin
      let apron_env = env#apron_env in
      let (lbvar,ladim) =
	List.fold_left
	  (begin fun (lbvar,ladim) var ->
	    match env#typ_of_var var with
	    | #Bdd.Env.typ -> (var::lbvar,ladim)
	    | _ ->
		let avar = Apron.Var.of_string var in
		let adim = Apron.Environment.dim_of_var apron_env avar in
		(lbvar,adim::ladim)
	  end)
	  ([],[]) lvar
      in
      let bsupp = Bdd.Expr0.O.bddsupport env lbvar in
      let tadim = Array.of_list ladim in
      let nlist =
	List.map
	  (begin fun elt ->
	    {
	      guard = Cudd.Bdd.exist bsupp elt.guard;
	      leaf =
		if tadim=[||]
		then elt.leaf
		else Apron.Abstract0.forget_array man.apron elt.leaf tadim false
	    }
	  end)
	  t.list
      in
      let nbottom =
	let guardnonbottom = Bddleaf.guard ~cudd:env#cudd nlist in
	{ t.bottom with guard = Cudd.Bdd.dnot guardnonbottom }
      in
      let res = {
	list = nlist;
	bottom = nbottom;
	disjoint = false;
	unique = t.unique && tadim=[||];
      }
      in
      canonicalize ~disjoint:man.forget_disjoint ~unique:man.forget_unique
	man res;
      res
    end

  let apply_change ~bottom man t change =
    let notbdd =
      change.bdd.Bdd.Env.intro = None && 
      change.bdd.Bdd.Env.remove = None 
    in
    let notapron = 
      change.Env.apron.Apron.Dim.add=None && change.Env.apron.Apron.Dim.remove=None
    in
    let nlist =
      List.map
	(begin fun elt ->
	  let nguard = 
	    if notbdd then elt.guard else
	      Bdd.Domain0.O.apply_change elt.guard change.bdd 
	  in
	  let nleaf = 
	    if notapron then elt.leaf else
	      Apron.Abstract0.apply_dimchange2 man.apron elt.leaf change.Env.apron false
	  in
	  { guard=nguard; leaf=nleaf }
	end)
	t.list
    in
    let nbottom =
      let guardnonbottom = Bddleaf.guard ~cudd:(Cudd.Bdd.manager t.bottom.guard) nlist in
      { guard = Cudd.Bdd.dnot guardnonbottom; leaf = bottom }
    in
    let unique = t.unique && change.Env.apron.Apron.Dim.remove=None in
    let disjoint = t.disjoint && unique && change.bdd.Bdd.Env.remove=None in
    let res = {
      list = nlist;
      bottom = nbottom;
      disjoint = disjoint;
      unique = unique;
    }
    in
    canonicalize ~disjoint:man.change_environment_disjoint ~unique:man.change_environment_unique
      man res;
    res

  let apply_permutation man t (operm,oapronperm) =
    if operm=None && oapronperm=None then t
    else begin
      let nlist =
	List.map
	  (begin fun elt ->
	    let nguard = match operm with
	      | None -> elt.guard
	      | Some perm -> Cudd.Bdd.permute elt.guard perm
	    in
	    let nleaf = match oapronperm with
	      | None -> elt.leaf
	      | Some perm ->
		  Apron.Abstract0.permute_dimensions man.apron elt.leaf perm
	    in
	    { guard=nguard; leaf=nleaf }
	  end)
	  t.list
      in
      let nbottom = { t.bottom with
	guard = match operm with
	| None -> t.bottom.guard
	| Some perm -> Cudd.Bdd.permute t.bottom.guard perm
      }
      in
      let res = {
	list = nlist;
	bottom = nbottom;
	disjoint = t.disjoint;
	unique = t.unique;
      }
      in
      res
    end

end


(*  ******************************************************************** *)
(** {2 Closed version} *)
(*  ******************************************************************** *)

let make_man apron = {
  apron = apron;
  bdd_restrict = Cudd.Bdd.restrict;
  expr_restrict = Expr0.O.restrict;
  meet_disjoint = true;
  join_disjoint = true;
  meet_cond_unique = true;
  meet_cond_disjoint = false;
  meet_cond_depth = max_int;
  assign_unique = true;
  assign_disjoint = false;
  substitute_unique = true;
  substitute_disjoint = false;
  forget_unique = true;
  forget_disjoint = false;
  change_environment_unique = true;
  change_environment_disjoint = false;
}
  
let canonicalize = O.canonicalize
let size = O.size
let print = O.print
let bottom = O.bottom
let top = O.top
let of_apron = O.of_apron
let is_bottom = O.is_bottom
let is_top = O.is_top
let is_leq = O.is_leq
let is_eq = O.is_eq
let to_bddapron = O.to_bddapron
let meet = O.meet
let join = O.join
let meet_condition = O.meet_condition
let assign_lexpr = O.assign_lexpr
let substitute_lexpr = O.substitute_lexpr
let forget_list = O.forget_list
let widening = O.widening
