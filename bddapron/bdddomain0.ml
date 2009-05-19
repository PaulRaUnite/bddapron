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
(** {2 Utilities} *)
(*  ********************************************************************** *)

let bdd_diff a b = Cudd.Bdd.dand a (Cudd.Bdd.dnot b)

let descend
    ~(cudd: Cudd.Man.vt)
    ~(maxdepth:int)
    ~(nocare:('a -> bool))
    ~(cube_of_down:('a -> Cudd.Bdd.vt))
    ~(cofactor:('a -> Cudd.Bdd.vt -> 'a)) 
    ~(select:('a -> int))
    ~(terminal:(depth:int -> newcube:Cudd.Bdd.vt -> cube:Cudd.Bdd.vt -> down:'a -> 'b option))
    ~(ite:(depth:int -> newcube:Cudd.Bdd.vt -> cond:int -> dthen:'b option -> delse:'b option -> 'b option))
    ~(down:'a) 
    :
    'b option
    =
  let rec map depth cube down =
    if nocare down then
      None
    else begin
      let newcube = cube_of_down down in
      let (cube,down) = 
	if Cudd.Bdd.is_true newcube then
	  (cube,down)
	else
	  (Cudd.Bdd.dand cube newcube, cofactor down newcube)
      in
      if nocare down then 
	None
      else begin
	let cond = select down in
	if (cond<0) || depth<=0 then begin 
            (* End case *)
	  terminal ~depth ~newcube ~cube ~down
	end
	else begin
	    (* Recursive case *)
	  let var = Cudd.Bdd.ithvar cudd cond in
	  let nvar = Cudd.Bdd.dnot var in
	  let dthen =
	    map (depth-1) (Cudd.Bdd.dand cube var) (cofactor down var)
	  and delse =
	    map (depth-1) (Cudd.Bdd.dand cube nvar) (cofactor down nvar)
	  in
	  ite ~depth ~newcube ~cond ~dthen ~delse
	end
      end
    end
  in
  map maxdepth (Cudd.Bdd.dtrue cudd) down

(*  ====================================================================== *)
(** {3 Iterators on lists} *)
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
  let descend_elt_bdd ~join ~maxdepth ~terminal man env cond elt bdd =
    let cudd = env#cudd in
    let cond_supp = cond#cond_supp in
    descend
      ~cudd ~maxdepth
      ~nocare:(fun (elt,bdd) -> Cudd.Bdd.is_false bdd || is_bottom man elt)
      ~cube_of_down:(fun (elt,bdd) -> Cudd.Bdd.cube_of_bdd bdd)
      ~cofactor:(fun (elt,bdd) cube ->
	let nelt = meet_cube man env cond elt cube in
	(nelt, Cudd.Bdd.cofactor bdd cube)
      )
      ~select:(fun (elt,bdd) ->
	let supp = Cudd.Bdd.support bdd in
	let suppcond = Cudd.Bdd.support_inter supp cond_supp in
	if Cudd.Bdd.is_cst suppcond 
	then -1
	else Cudd.Bdd.topvar suppcond
      )
      ~terminal
      ~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	match (dthen,delse) with
	| None,x | x,None -> x
	| Some(ga,lista),Some(gb,listb) ->
	    let list = List.fold_left join lista listb in
	    Some(Cudd.Bdd.dor ga gb, list)
      )
	
  let meet_bdd ~join ~maxdepth man env cond elt bdd =
    let cudd = cond#cudd in
    let dtrue = Cudd.Bdd.dtrue cudd in
    let cond_supp = cond#cond_supp in
    let ores = 
      descend_elt_bdd 
	~join ~maxdepth man env cond elt bdd
	~terminal:(fun ~depth ~newcube ~cube ~down ->
	  let (elt,bdd) = down in
	  let nbdd = if depth<=0 then Cudd.Bdd.exist cond_supp bdd else bdd in
	  let nelt = { elt with guard = Cudd.Bdd.dand elt.guard nbdd } in
	  if is_bottom man nelt then None else Some(dtrue,[elt])
	)
	~down:(elt,bdd)
    in
    begin match ores with
    | None -> []
    | Some(guard,list) -> list
    end

  let descend_elt_texpr ~join ~maxdepth ~terminal man env cond elt texpr = 
    let cudd = cond#cudd in
    let dtrue = Cudd.Bdd.dtrue cudd in
    let cond_supp = cond#cond_supp in
    let ores =
      descend
	~cudd ~maxdepth
	~nocare:(fun (elt,texpr) -> is_bottom man elt)
	~cube_of_down:(fun (elt,texpr) -> Cudd.Bdd.cube_of_bdd elt.guard)
	~cofactor:(fun (elt,texpr) cube ->
	  let nelt = meet_cube man env cond elt cube in
	  (nelt, Domain0.O.Descend.texpr_cofactor Expr0.cofactor texpr cube)
	)
	~select:(fun (elt,texpr) ->
	  let suppcond = Domain0.O.Descend.texpr_support cond texpr in
	  if Cudd.Bdd.is_cst suppcond 
	  then -1
	  else Cudd.Bdd.topvar suppcond
	)
	~terminal
	~ite:(fun ~depth ~newcube ~cond ~dthen ~delse ->
	  match (dthen,delse) with
	  | None,x | x,None -> x
	  | Some(ga,lista),Some(gb,listb) ->
	      let g = Cudd.Bdd.dor ga gb in
	      let list = List.fold_left join lista listb in
	      Some(g,list)
	)
	~down:(elt,texpr)
    in
    ores
      
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

let meet_cond_internal ~nodoublon ~exclusive ~maxdepth (man:'a man) env cond (t:'a t) bdd =
  assert(if exclusive then nodoublon else true);
  let nlist =
    List.fold_left
      (begin fun res elt ->
	if Cudd.Bdd.is_inter_empty elt.guard bdd then
	  res
	else begin
	  let bdd = man.bddrestrict bdd elt.guard in
	  let list =
	    Elt.meet_bdd
	      ~maxdepth
	      ~join:(ListE.join ~nodoublon ~exclusive man)
	      man env cond elt bdd
	  in
	  if t.exclusive && exclusive then
	    List.fold_left (ListE.join_nodoublon man) res list
	  else
	    List.fold_left (ListE.join ~nodoublon ~exclusive man) res list
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
      ~maxdepth:man.meet_cond_depth
      man env cond t bdd
  in
  assert(check_wellformed man res);
  res

(*  ====================================================================== *)
(** {3 Assignement/Substitution} *)
(*  ====================================================================== *)

(*
let assign_lexpr_internal 
    ~nodoublon ~exclusive ~maxdepth 
    ?relational ?nodependency
    (man:'a man)
    (env:(('b,'c) #Env.O.t as 'd))
    (cond:(Cond.cond,'d) #Cond.O.t)
    (t:'a t)
    (lvar : string list) (lexpr: Expr0.t list)
    (odest:'a t option)
    :
    'a t
    =
  assert(if exclusive then nodoublon else true);
  assert(List.length lvar = List.length lexpr);
  let texpr = Array.of_list lexpr in
  let nlist =
    List.fold_left
      (begin fun res elt ->
	let texpr = Array.map (fun texpr -> man.texpr_restrict texpr elt.guard) texpr in
	let ores =
	  Elt.descend_elt_texpr
	    ~join:(ListE.join ~nodoublon ~exclusive man)
	    ~maxdepth
	    man env cond elt 
	    (Array.map (fun texpr -> man.texpr_restrict texpr bdd) texpr)
	    man env cond
	    (elt,texpr)
	    ~terminal:(fun ~depth ~newcube ~cube ~down ->
	      let lexpr = Array.to_list texpr in
	      let (lbvar,lbexpr,tavar,taexpr) = split_lvarlexpr lvar lexpr in
	      let res =
		ApronDD.asssub_texpr_array
	  ~asssub_bdd:(fun bdd ->
	    Bdd.Domain0.O.assign_lexpr ?relational ?nodependency
	      env bdd lbvar lbexpr
	  )
	  Apron.Abstract1.assign_texpr_array
	  man t tavar taexpr odest
      in
      res
    end)
    t texpr
*)
