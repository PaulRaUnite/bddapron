open Format

type 'a man = {
  apron : 'a Apron.Manager.t;
  mutable meet_canonical : bool;
  mutable join_canonical : bool;
}

type 'a elt = {
  mutable guard : Cudd.Bdd.vt;
  abs : 'a Apron.Abstract0.t
}

type 'a t = {
  mutable list : 'a elt list;
  bottom : 'a elt;
  mutable normalized: bool;
}

let iter2 f list1 list2 =
  List.iter
    (begin fun elt1 ->
      List.iter
	(begin fun elt2 ->
	  if not (Cudd.Bdd.is_inter_empty elt1.guard elt2.guard) then
	    f elt1 elt2
	end)
	list2
    end)
    list1

let fold2 f res list1 list2 =
  List.fold_left
    (begin fun res elt1 ->
      List.fold_left
	(begin fun res elt2 ->
	  if not (Cudd.Bdd.is_inter_empty elt1.guard elt2.guard) then
	    f res elt1 elt2
	  else
	    res
	end)
	res
	list2
    end)
    res
    list1

let insert_elt man elt1 list2 =
  let rec insert res = function
    | [] -> elt1::res
    | elt2::list2 ->
	if Apron.Abstract0.is_eq man.apron elt1.abs elt2.abs then begin
	  let nelt2 = { elt2 with
	    guard = Cudd.Bdd.dor elt1.guard elt2.guard
	  } in
	  List.rev_append list2 (nelt2 :: res)
	end
	else
	  insert (elt2::res) list2
  in
  insert [] list2

let merge_doublons man list =
  let nlist =
    List.fold_left
      (begin fun res elt -> insert_elt man elt res end)
      []
      list
  in
  nlist

let check_exclusive man t =
  let rec check = function
    | [] -> true
    | elt::list ->
	List.iter
	  (begin fun elt2 ->
	    if not (Cudd.Bdd.is_inter_empty elt.guard elt2.guard) then
	      failwith "Bddapron.Domain0: two non disjoint guards in a canonicalized abstract value.";
	    if Apron.Abstract0.is_eq man.apron elt.abs elt2.abs then
	      failwith "Bddapron.Domain0: two equal APRON values in a canonicalized abstract value."
	    ;
	  end)
	  list
	;
	check list
  in
  check t.list

let check_wellformed man t =
  List.iter
    (begin fun elt ->
      if Cudd.Bdd.is_false elt.guard then
	failwith "Bddapron.Domain0.check_wellformed: false guard in a list"
      ;
      if Apron.Abstract0.is_bottom man.apron elt.abs then
	failwith "Bddapron.Domain0.check_wellformed: empty APRON value in a list"
    end)
    t.list
  ;
  let guardofnonbottom =
    let cudd = Cudd.Bdd.manager t.bottom.guard in
    List.fold_left
      (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
      (Cudd.Bdd.dfalse cudd) t.list
  in
  if not (Cudd.Bdd.is_equal t.bottom.guard (Cudd.Bdd.dnot guardofnonbottom)) then
    failwith "The guard of bottom is not the complement of the other guards"
  ;
  if t.normalized then check_exclusive man t else true

let make_exclusive man list =
  if list=[] then []
  else begin
    let rec make_exclusive res = function
      | [] -> res
      | [elt] -> insert_elt man elt res
      | elta::listb ->
	  let (lista,listb,listab) =  List.fold_left
	    (begin fun (lista,listb,listab) eltb ->
	      (* A\B *)
	      let guarda = Cudd.Bdd.dand elta.guard (Cudd.Bdd.dnot eltb.guard) in
	      let (guardb,guardab) =
		if Cudd.Bdd.is_equal guarda elta.guard then
		  (* A\B=A, which means A /\ B = empty *)
		  (eltb.guard, Cudd.Bdd.dfalse (Cudd.Bdd.manager guarda))
		else if Cudd.Bdd.is_false guarda then
		  (* A\B=ff, which means A <= B *)
		  (Cudd.Bdd.dand eltb.guard (Cudd.Bdd.dnot elta.guard), elta.guard)
		else
		  (Cudd.Bdd.dand eltb.guard (Cudd.Bdd.dnot elta.guard),
		  Cudd.Bdd.dand eltb.guard elta.guard)
	      in
	      let nlista =
		if Cudd.Bdd.is_false guarda then lista
		else { guard=guarda; abs=elta.abs }::lista in
	      let nlistb =
		if Cudd.Bdd.is_false guardb then listb
		else { guard=guardb; abs=eltb.abs }::listb in
	      let nlistab =
		if Cudd.Bdd.is_false guardab then listab
		else begin
		  {
		    guard=guardab;
		    abs=Apron.Abstract0.join man.apron elta.abs eltb.abs
		  }::
		    listab
		end
	      in
	      (nlista,nlistb,nlistab)
	    end)
	    ([],[],[])
	    listb
	  in
	  let res = make_exclusive res lista in
	  let res = make_exclusive res listb in
	  let res = make_exclusive res listab in
	  res
    in
    make_exclusive [] list
  end

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


let canonicalize man t =
  if not t.normalized then begin
    let list = merge_doublons man t.list in
    let list = make_exclusive man list in
    t.list <- list;
    t.normalized <- true;
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

let abs_of_env cons man env =
  let dim = Apron.Environment.dimension env#apron_env in
  cons man.apron
    dim.Apron.Dim.intd dim.Apron.Dim.reald

let bottom man env =
  {
    list = [];
    bottom = {
      guard = Cudd.Bdd.dtrue env#cudd;
      abs = abs_of_env Apron.Abstract0.bottom man env
    };
    normalized = true;
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
    normalized = true;
  }

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
	(begin fun elt1 elt2 ->
	  if not (Apron.Abstract0.is_leq man.apron elt1.abs elt2.abs) then
	    raise Exit
	end)
	t1.list t2.list
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

let meet_canonical man t1 t2 =
  let nbottom = { t1.bottom with
    guard = Cudd.Bdd.dor t1.bottom.guard t2.bottom.guard
  }
  in
  let nlist = fold2
    (begin fun res elt1 elt2 ->
      let nabs = Apron.Abstract0.meet man.apron elt1.abs elt2.abs in
      let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
      if Apron.Abstract0.is_bottom man.apron nabs then begin
	nbottom.guard <- Cudd.Bdd.dor nbottom.guard nguard;
	res
      end
      else
	insert_elt man { guard = nguard; abs = nabs } res
    end)
    []
    t1.list t2.list
  in
  {
    list = nlist;
    bottom = nbottom;
    normalized = true;
  }

let meet_noncanonical man t1 t2 =
  let nlist = fold2
    (begin fun res elt1 elt2 ->
      let nabs = Apron.Abstract0.meet man.apron elt1.abs elt2.abs in
      if Apron.Abstract0.is_bottom man.apron nabs then
	res
      else begin
	let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
	{ guard = nguard; abs = nabs }::res
      end
    end)
    []
    t1.list t2.list
  in
  let guardnonbottom =
    List.fold_left
      (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
      (Cudd.Bdd.dfalse (Cudd.Bdd.manager t1.bottom.guard))
      nlist
  in
  {
    list = nlist;
    bottom = {
      guard = Cudd.Bdd.dnot guardnonbottom;
      abs = t1.bottom.abs
    };
    normalized = false;
  }
    
let meet man t1 t2 =
  let res =
    if t1.normalized && t2.normalized then
      meet_canonical man t1 t2
    else if man.meet_canonical then begin
      canonicalize man t1;
      canonicalize man t2;
      meet_canonical man t1 t2
    end
    else
      meet_noncanonical man t1 t2
  in
  assert(check_wellformed man res);
  res


let join_canonical man t1 t2 =
  let nbottom = { t1.bottom with
    guard = Cudd.Bdd.dand t1.bottom.guard t2.bottom.guard
  }
  in
  let nlist = fold2
    (begin fun res elt1 elt2 ->
      let nabs = Apron.Abstract0.join man.apron elt1.abs elt2.abs in
      let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
      insert_elt man { guard = nguard; abs = nabs } res
    end)
    []
    t1.list t2.list
  in
  {
    list = nlist;
    bottom = nbottom;
    normalized = true;
  }

let join_noncanonical man t1 t2 =
  let nlist = fold2
    (begin fun res elt1 elt2 ->
      let nabs = Apron.Abstract0.join man.apron elt1.abs elt2.abs in
      let nguard = Cudd.Bdd.dand elt1.guard elt2.guard in
      { guard = nguard; abs = nabs }::res
    end)
    []
    t1.list t2.list
  in
  let guardnonbottom =
    List.fold_left
      (begin fun res elt -> Cudd.Bdd.dor res elt.guard end)
      (Cudd.Bdd.dfalse (Cudd.Bdd.manager t1.bottom.guard))
      nlist
  in
  {
    list = nlist;
    bottom = {
      guard = Cudd.Bdd.dnot guardnonbottom;
      abs = t1.bottom.abs
    };
    normalized = false;
  }
    
let join man t1 t2 =
  let res =
    if t1.normalized && t2.normalized then
      join_canonical man t1 t2
    else if man.join_canonical then begin
      canonicalize man t1;
      canonicalize man t2;
      join_canonical man t1 t2
    end
    else
      join_noncanonical man t1 t2
  in
  assert(check_wellformed man res);
  res

