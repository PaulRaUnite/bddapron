type ('a,'b) dd = {
  hash : int;
  node : ('a,'b) node
}
and ('a,'b) node =
  | Leaf of 'b
  | Decision of ('a,'b) decision
and ('a,'b) decision = {
  dcond : 'a cond;
  dthen : ('a,'b) dd;
  delse : ('a,'b) dd;
}
and 'a cond = {
  hashcond : int;
  cond : 'a;
}
type ('a,'b) compare = {
  hashl : 'b Weakke.compare;
  hashc : 'a Weakke.compare;
  comparec : 'a -> 'a -> int;
}

type ('a,'b) man = {
  bman : ('a,bool) man;
  compare : ('a,'b) compare;
  compare_unique : ('a, 'b) dd Weakke.compare;
  table :  ('a,'b) dd Weakke.t;
  dtrue : ('a,bool) dd;
  dfalse : ('a,bool) dd;
}

let make_compare_unique compare = {
  Weakke.hash = begin fun dd -> dd.hash end;
  Weakke.equal = begin fun dd1 dd2 ->
    match (dd1.node, dd2.node) with
    | ((Leaf l1),(Leaf l2)) -> compare.hashl.Weakke.equal l1 l2
    | ((Decision d1),(Decision d2)) ->
	(d1.dcond.hashcond == d2.dcond.hashcond) &&
	  (compare.hashc.Weakke.equal d1.dcond.cond d2.dcond.cond) &&
	  (d1.dthen==d2.dthen) &&
	  (d1.delse==d2.delse)
    | _ -> false
  end;
}

let unique_leaf man leaf =
  let dd = {
    hash = man.compare.hashl.Weakke.hash leaf;
    node = Leaf leaf
  }
  in
  Weakke.Compare.merge man.compare_unique man.table dd

let unique_decision man decision =
  if decision.dthen==decision.delse then decision.dthen
  else
    let dd = {
      hash = decision.dcond.hashcond*617 + 29*decision.dthen.hash + 3*decision.delse.hash;
      node = Decision(decision)
    }
    in
    Weakke.Compare.merge man.compare_unique man.table dd

let make_bman ~hashcond ~comparecond =
  let bddcompare = {
    hashl = {
      Weakke.hash = (fun (b:bool) -> Obj.magic b);
      Weakke.equal = (==);
    };
    hashc = hashcond;
    comparec = comparecond
  }
  in
  let rec man = {
    bman = man;
    compare = bddcompare;
    compare_unique = make_compare_unique bddcompare;
    table = Weakke.create 31;
    dtrue = { hash = Obj.magic true; node = Leaf true };
    dfalse = { hash = Obj.magic false; node = Leaf false };
  }
  in
  ignore (unique_leaf man true);
  ignore (unique_leaf man false);
  man

let make_man ~(bman:('a,bool) man) ~hashleaf =
  let compare = {
    hashl = hashleaf;
    hashc = bman.compare.hashc;
    comparec = bman.compare.comparec;
  }
  in {
    bman = bman;
    compare = compare;
    compare_unique = make_compare_unique compare;
    table = Weakke.create 31;
    dtrue = bman.dtrue;
    dfalse = bman.dfalse;
  }

(*  ********************************************************************** *)
(** Caching operations *)
(*  ********************************************************************** *)

type entry1 = {
  op1 : int;
  arg : (unit,unit) dd;
}
type entry2 = {
  op2 : int;
  arg1 : (unit,unit) dd;
  arg2 : (unit,unit) dd;
}
type entry = {
  op : int;
  args : (unit,unit) dd array
}
let compare_entry1 = {
  Hashhe.hash = begin fun entry ->
    entry.op1 * 29 + entry.arg.hash * 3
  end;
  Hashhe.equal = begin fun entry1 entry2 ->
    entry1.op1=entry2.op1 && entry1.arg==entry2.arg
  end
}
let compare_entry2 = {
  Hashhe.hash = begin fun entry ->
    entry.op2 * 617 + entry.arg1.hash * 29 + entry.arg2.hash * 3
  end;
  Hashhe.equal = begin fun entry1 entry2 ->
    entry1.op2=entry2.op2 && entry1.arg1==entry2.arg1 && entry1.arg2==entry2.arg2
  end
}

let compare_entry = {
  Hashhe.hash = begin fun entry ->
    Array.fold_left
      (fun res arg -> res * 113 + arg.hash)
      entry.op
      entry.args
  end;
  Hashhe.equal = begin fun entry1 entry2 ->
    entry1.op=entry2.op &&
      let length = Array.length entry1.args in
      length=(Array.length entry2.args) &&
	  begin
	    try
	      for i=0 to pred length do
		if (Array.unsafe_get entry1.args i) !=
		  (Array.unsafe_get entry2.args i)
		then raise Exit;
	      done;
	      true
	    with Exit ->
	      false
	  end
  end
}

let (ddmagic : ('a,'b) dd -> ('c,'d) dd) = Obj.magic

let (cache1:(entry1,(unit,unit) dd) Hashhe.Custom.t) =
  Hashhe.Custom.create_compare compare_entry1 43
let (cache2:(entry2,(unit,unit) dd) Hashhe.Custom.t) =
  Hashhe.Custom.create_compare compare_entry2 43
let (cache:(entry,(unit,unit) dd) Hashhe.Custom.t) =
  Hashhe.Custom.create_compare compare_entry 43

let find1 (entry:entry1) : ('a,'b) dd =
  ddmagic
    (Hashhe.Custom.find cache1 entry)
let find2 (entry:entry2) : ('a,'b) dd =
  ddmagic
    (Hashhe.Custom.find cache2 entry)
let find (entry:entry) : ('a,'b) dd =
  ddmagic
    (Hashhe.Custom.find cache entry)

let add1 (entry:entry1) (res:('a,'c) dd) =
  Hashhe.Custom.add cache1 entry (ddmagic res)
let add2 (entry:entry2) (res:('a,'c) dd) =
  Hashhe.Custom.add cache2 entry (Obj.magic res)
let add (entry:entry) (res:('a,'c) dd) =
  Hashhe.Custom.add cache entry (Obj.magic res)

let clear1 () = Hashhe.Custom.clear cache1
let clear2 () = Hashhe.Custom.clear cache2
let clear () = Hashhe.Custom.clear cache
let clear_all () = clear1(); clear2(); clear()

(*  ********************************************************************** *)
(** General operations *)
(*  ********************************************************************** *)

let is_cst dd = match dd.node with
  | Leaf _ -> true
  | Decision _ -> false
let is_decision dd = not (is_cst dd)

let topvar dd = match dd.node with
  | Decision(decision) -> decision.dcond.cond
  | _ -> raise (Invalid_argument "topcond applied to constant node")

let dthen dd = match dd.node with
  | Decision(decision) -> decision.dthen
  | _ -> raise (Invalid_argument "dthen applied to constant node")
let delse dd = match dd.node with
  | Decision(decision) -> decision.delse
  | _ -> raise (Invalid_argument "delse applied to constant node")
let dval dd = match dd.node with
  | Leaf leaf -> leaf
  | _ ->  raise (Invalid_argument "dval applied to non-constant node")



(*  ********************************************************************** *)
(** Generic descends *)
(*  ********************************************************************** *)

let min_cond man cond1 cond2 =
  if man.compare.comparec cond1.cond cond2.cond <= 0 then cond1 else cond2
let min_ocond man ocond1 ocond2 =
  match ocond1, ocond2 with
  | None, None -> None
  | Some _, None -> ocond1
  | None, Some _ -> ocond2
  | (Some cond1), (Some cond2) ->
      Some (min_cond man cond1 cond2)

let decompose_cond man cond dd =
  match dd.node with
  | Leaf _ -> (dd,dd)
  | Decision(decision) ->
      if man.compare.hashc.Weakke.equal cond.cond decision.dcond.cond then
	(decision.dthen,decision.delse)
      else
	(dd,dd)

let top_ocond dd = match dd.node with
  | Decision(decision) -> Some decision.dcond
  | Leaf _ -> None

let map_op1 ~(op1:int) op man dd
    =
  let rec map dd =
    let entry = { op1=op1; arg=ddmagic dd } in
    try
      find1 entry
    with Not_found ->
      let res = match dd.node with
	| Leaf leaf -> op leaf
	| Decision(decision) ->
	    let ndecision = { decision with
	      dthen = map decision.dthen;
	      delse = map decision.delse;
	    }
	    in
	    unique_decision man ndecision
      in
      add1 entry res;
      res
  in
  map dd
;;

let exchange_commutative dd1 dd2 =
  match (dd1.node, dd2.node) with
  | ((Leaf _), (Leaf _))
  | ((Decision _), (Decision _)) ->
      dd1.hash > dd2.hash
  | ((Leaf _), (Decision _)) -> false
  | ((Decision _), (Leaf _)) -> true

let map_op2
    ?(commutative=false)
    ?(special=fun _ _ -> None)
    ~(op2:int) 
    op
    man dd1 dd2
    :
    ('a,'d) dd
    =
  let rec map (dd1:('a,'b) dd) (dd2:('a,'c) dd) : ('a,'d) dd =
    if commutative && exchange_commutative dd1 dd2 then
      map (ddmagic dd2) (ddmagic dd1)
    else match special dd1 dd2 with
    | Some res -> res
    | None ->
	let entry = { op2=op2; arg1=ddmagic dd1; arg2=ddmagic dd2 } in
	try
	  find2 entry
	with Not_found ->
	  let res =
	    let ocond1 = top_ocond dd1 and ocond2 = top_ocond dd2 in
	    let ocond = min_ocond man ocond1 ocond2 in
	    match ocond with
	    | None ->
		let l1 = dval dd1 and l2 = dval dd2 in
		op l1 l2
	    | Some cond ->
		let (dd1a,dd1b) = decompose_cond man cond dd1 in
		let (dd2a,dd2b) = decompose_cond man cond dd2 in
		let ndecision = {
		  dcond = cond;
		  dthen = map dd1a dd2a;
		  delse = map dd1b dd2b;
		}
		in
		unique_decision man ndecision
	  in
	  add2 entry res;
	  res
  in
  map dd1 dd2
;;

let map_op3
    ?(special=fun _ _ _ -> None)
    ~(op:int) opl
    (man:('a,'e) man)
    (dd1:('a,'b) dd)
    (dd2:('a,'c) dd)
    (dd3:('a,'d) dd)
    :
    ('a,'e) dd
    =
  let rec map (dd1:('a,'b) dd) (dd2:('a,'c) dd) (dd3:('a,'d) dd) : ('a,'e) dd =
    let entry = { op=op; args=[|ddmagic dd1; ddmagic dd2; ddmagic dd3|] } in
    try
      find entry
    with Not_found ->
      let res =
	let ocond1 = top_ocond dd1 and
	    ocond2 = top_ocond dd2 and
	    ocond3 = top_ocond dd3 in
	let ocond = min_ocond man ocond1 ocond2 in
	let ocond = min_ocond man ocond ocond3 in
	match ocond with
	| None ->
	    let l1 = dval dd1 and l2 = dval dd2 and l3 = dval dd3 in
	    opl l1 l2 l3
	| Some cond ->
	    let (dd1a,dd1b) = decompose_cond man cond dd1 in
	    let (dd2a,dd2b) = decompose_cond man cond dd2 in
	    let (dd3a,dd3b) = decompose_cond man cond dd3 in
	    let ndecision = {
	      dcond = cond;
	      dthen = map dd1a dd2a dd3a;
	      delse = map dd1b dd2b dd3b;
	    }
	    in
	    unique_decision man ndecision
      in
      add entry res;
      res
  in
  map dd1 dd2 dd3
;;

module Bdd = struct
  let cst man b = if b then man.dtrue else man.dfalse
  let dtrue man = man.dtrue
  let dfalse man = man.dfalse

  let is_cst bdd = match bdd with Leaf _ -> true | _ -> false
  let is_true bdd = match bdd with Leaf b -> b | _ -> false
  let is_false bdd = not (Cudd.Bdd.is_true bdd)

  let ithcond man cond b =
    unique_decision man.bman {
      dcond = cond;
      dthen = if b then man.dtrue else man.dfalse;
      delse = if b then man.dfalse else man.dtrue
    }

  let ithvar man var =
    ithcond man { cond=var; hashcond = man.compare.hashc.Weakke.hash var } true

  let dnot man bdd = map_op1 ~op1:1 (fun b -> cst man (not b)) man bdd

  let absorbant_and b = if not b then Some b else None
  let absorbant_or b = if b then Some b else None
  let n_is_true b = b
  let n_is_false b = not b
  let dand man bdd1 bdd2 =
    map_op2
      ~commutative:true
      ~special:begin fun dd1 dd2 ->
	if dd1==dd2 then Some dd1
	else match dd1.node with
	| Leaf b -> Some(if b then dd2 else dd1)
	| _ ->
	    match dd2.node with
	    | Leaf b -> Some(if b then dd1 else dd2)
	    | _ -> None
      end
      ~op2:1 (fun b1 b2 -> cst man (b1 && b2)) man.bman bdd1 bdd2
  let dor man bdd1 bdd2 =
    map_op2
      ~commutative:true
      ~special:begin fun dd1 dd2 ->
	if dd1==dd2 then Some dd1
	else match dd1.node with
	| Leaf b -> Some(if b then dd1 else dd2)
	| _ ->
	    match dd2.node with
	    | Leaf b -> Some(if b then dd2 else dd1)
	    | _ -> None
      end
      ~op2:2 (fun b1 b2 -> cst man (b1 || b2)) man.bman bdd1 bdd2

  let dxor man bdd1 bdd2 =
    map_op2
      ~commutative:true
      ~special:begin fun dd1 dd2 ->
	if dd1==dd2 then Some man.dfalse
	else match dd1.node with
	| Leaf b -> Some(if b then dnot man dd2 else dd2)
	| _ ->
	    match dd2.node with
	    | Leaf b -> Some(if b then dnot man dd1 else dd1)
	    | _ -> None
      end
      ~op2:3 (fun b1 b2 -> cst man (b1 != b2)) man.bman bdd1 bdd2

  let eq man bdd1 bdd2 =
    map_op2
      ~commutative:true
      ~special:begin fun dd1 dd2 ->
	if dd1==dd2 then Some man.dtrue
	else match dd1.node with
	| Leaf b -> Some(if b then dd2 else dnot man dd2)
	| _ ->
	    match dd2.node with
	    | Leaf b -> Some(if b then dd1 else dnot man dd1)
	    | _ -> None
      end
      ~op2:4 (fun b1 b2 -> cst man (b1 == b2)) man.bman bdd1 bdd2

  let ite man bdd1 bdd2 bdd3 =
    map_op3
      ~special:begin fun dd1 dd2 dd3 ->
	if dd2==dd3 then Some dd2
	else if dd1==dd2 then Some(dor man dd1 dd3)
	else if dd1==dd3 then Some(dand man dd1 dd2)
	else match dd1.node with
	| Leaf b -> Some(if b then dd2 else dd3)
	| _ ->
	    match dd2.node with
	    | Leaf true -> Some(dor man dd1 dd3)
	    | _ -> match dd3.node with
	      | Leaf false -> Some(dand man dd1 dd2)
	      | _ -> None
      end
      ~op:1 (fun b1 b2 b3 -> cst man (if b1 then b2 else b3)) man.bman bdd1 bdd2 bdd3

  let support_union = dand

  let rec cube_union man bdd1 bdd2 =
    if bdd1==bdd2 then bdd1
    else if exchange_commutative bdd1 bdd2 then cube_union man bdd1 bdd2
    else match (bdd1.node, bdd2.node) with
    | (Leaf _, _)
    | (_, Leaf _) -> man.dtrue
    | (Decision d1, Decision d2) ->
	assert(d1.dthen==man.dfalse || d1.delse==man.dfalse);
	assert(d2.dthen==man.dfalse || d2.delse==man.dfalse);
	let cmp = man.compare.comparec d1.dcond.cond d2.dcond.cond in
	if cmp<0 then begin
	  cube_union man
	    (if d1.dthen != man.dfalse then d1.dthen else d1.delse)
	    bdd2
	end
	else if cmp>0 then begin
	  cube_union man
	    bdd1
	    (if d2.dthen != man.dfalse then d2.dthen else d2.delse)
	end
	else begin
	  let entry = { op2=5; arg1=ddmagic bdd1; arg2=ddmagic bdd2 } in
	  try
	    find2 entry
	  with Not_found ->
	    let res =
	    if d1.delse==man.dfalse && d2.delse==man.dfalse then
	      let res = cube_union man d1.dthen d2.dthen in
	      unique_decision man
		{ d1 with dthen = res; delse = man.dfalse }
	    else if d1.dthen==man.dfalse && d2.dthen==man.dfalse then
	      let res = cube_union man d1.delse d2.delse in
	      unique_decision man
		{ d1 with dthen = man.dfalse; delse = res }
	    else if d1.delse==man.dfalse then
	      cube_union man d1.dthen d2.delse
	    else
	      cube_union man d1.delse d2.dthen
	    in
	    add2 entry res;
	    res
	end

  let support_inter = cube_union

end

let cst = unique_leaf

let ite man bdd1 dd2 dd3 =
  map_op3
    ~special:begin fun dd1 dd2 dd3 ->
      if dd2==dd3 then Some dd2
      else match dd1.node with
      | Leaf b -> Some(if b then dd2 else dd3)
      | _ -> None
    end
    ~op:1 (fun b1 l2 l3 -> cst man (if b1 then l2 else l3)) man bdd1 dd2 dd3

let rec support
    (man:('a,'c) man)
    (dd:('a,'b) dd)
    :
    ('a,bool) dd
    =
  match dd.node with
  | Leaf _ -> man.dtrue
  | Decision d ->
      let entry = { op1=2; arg=dd } in
      try
	find1 entry
      with Not_found ->
	let resa = support man d.dthen in
	let resb = support man d.delse in
	let res = Cudd.Bdd.dor man resa resb in
	let res =
	  unique_decision man
	    { d with dthen=res; delse=man.dfalse }
	in
	add1 entry res;
	res

let is_var_in
    (man:('a,'c) man)
    (cond:'a)
    (dd:('a,'b) dd)
    :
    bool
    =
  let bdd1 = Cudd.Bdd.ithvar man cond in

  let rec map dd =
    match dd.node with
    | Leaf _ -> man.dfalse
    | Decision d ->
	let cmp = man.compare.comparec cond d.dcond.cond in
	if cmp < 0 then man.dfalse
	else if cmp=0 then man.dtrue
	else begin
	  let entry = { op2=6; arg1=ddmagic bdd1; arg2=ddmagic dd } in
	  try
	    find2 entry
	  with Not_found ->
	    let res =
	      if (map d.dthen) != man.dfalse ||
		(map d.delse) != man.dfalse then
		  man.dtrue
	      else
		man.dfalse
	    in
	    add2 entry res;
	    res
	end
  in
  let res = map dd in
  (res==man.dtrue)

let rec cofactor
    (man:('a,'b) man)
    (dd1:('a,'b) dd)
    (dd2:('a,bool) dd)
    :
    ('a,'b) dd
    =
  if (Obj.magic dd1)==dd2 then Obj.magic man.dtrue
  else match dd1.node with
  | Leaf _ -> dd1
  | Decision d1 ->
      match dd2.node with
      | Leaf true -> dd1
      | Leaf false -> failwith "the BDD argument is not a cube"
      | Decision d2 ->
	  let entry = { op2=7; arg1=ddmagic dd1; arg2=ddmagic dd2 } in
	  try
	    find2 entry
	  with Not_found ->
	    let res =
	      let cmp = man.compare.comparec d1.dcond.cond d2.dcond.cond in
	      if cmp<0 then
		let ndecision = {
		  dcond = d1.dcond;
		  dthen = cofactor man d1.dthen dd2;
		  delse = cofactor man d1.delse dd2;
		}
		in
		unique_decision man ndecision
	      else
		let (dd1a,dd1b) =
		  if cmp=0 then (d1.dthen,d1.delse) else (dd1,dd1)
		in
		if d2.delse==man.dfalse then
		  cofactor man dd1a d2.dthen
		else if d2.dthen==man.dfalse then
		  cofactor man dd1b d2.delse
		else
		  failwith "the BDD argument is not a cube"
	    in
	    add2 entry res;
	    res

let cofactors man cond dd =
  let cond = Cudd.Bdd.ithvar man cond in
  (cofactor man dd cond, cofactor man dd (Cudd.Bdd.dnot man cond))

let compose man cond bdd dd =
  let var = Cudd.Bdd.ithvar man cond in

  let rec map dd = match dd.node with
    | Leaf _ -> dd
    | Decision d ->
	let cmp = man.compare.comparec cond d.dcond.cond in
	if cmp<0 then
	  dd
	else if cmp=0 then
	  ite man bdd d.dthen d.delse
	else
	  let entry = { op=2; args=[|ddmagic var; ddmagic bdd; ddmagic dd|] } in
	  try
	    find entry
	  with Not_found ->
	    let res = 
	      unique_decision man 
		{ d with 
		  dthen = map d.dthen;
		  delse = map d.delse
		}
	    in
	    add entry res;
	    res
  in
  map dd

let vectorcompose ?(sorted=false) ?term man lcondbdd dd =
  let cache = 
    let compare = {
      Hashhe.hash = man.compare_unique.Weakke.hash;
      Hashhe.equal = man.compare_unique.Weakke.equal;
    }
    in
    Hashhe.Custom.create_compare compare 31
  in
  
  let rec map lcondbdd dd = 
    try
      Hashhe.Custom.find cache dd
    with Not_found ->
      let res = 
	match dd.node with
	| Leaf l -> 
	    begin match term with
	    | None -> dd
	    | Some term -> term l
	    end
	| Decision d ->
	    match lcondbdd with
	    | [] -> 
		if term=None then
		  dd
		else 
		  unique_decision man { d with
		    dthen = map [] d.dthen;
		    delse = map [] d.delse
		  }
	    | (cond,bdd)::rest ->
		let cmp = man.compare.comparec cond d.dcond.cond in
		if cmp<0 then
		  unique_decision man { d with
		    dthen = map rest d.dthen;
		    delse = map rest d.delse
		  }
		else if cmp=0 then
		  ite man bdd (map rest d.dthen) (map rest d.delse)
		else
		  unique_decision man 
		    { d with 
		      dthen = map lcondbdd d.dthen;
		      delse = map lcondbdd d.delse
		    }
      in
      Hashhe.Custom.add cache dd res;
      res
  in
  let lcondbdd = 
    if not sorted then
      List.sort (fun (c1,_) (c2,_) -> man.compare.comparec c1 c2) lcondbdd 
    else
      lcondbdd
  in
  map lcondbdd dd
    
let restrict ?term man dd1 bdd2 =
  let rec map dd1 bdd2 =
    match bdd2.node with
    | Leaf b -> if b then dd1 else failwith "false careset"
    | Decision d2 ->
	match dd1.node with
	| Leaf _ -> dd1
	| Decision d1 ->
	    let entry = { op2=8; arg1=dd1; arg2=ddmagic bdd2 } in
	    try 
	      find2 entry
	    with Not_found ->
	      let res =
		let cmp = man.compare.comparec d1.dcond.cond d2.dcond.cond in
		if cmp>0 then
		  map dd1 (Cudd.Bdd.dor man d2.dthen d2.delse)
		else begin
		  let cond = d1.dcond.cond in
		  let (bdd2then,bdd2else) = decompose_cond man cond bdd2 in
		  if bdd2then==man.dfalse then
		    map d1.delse bdd2delse  
		  else if bdd2else==man.dfalse then
		    map d1.dthen bdd2then
		  else
		    let resthen = map d1.dthen bdd2then in
		    let reselse = map d1.delse bdd2delse in
		    unique_decision man
		      { d1 with dthen=resthen; delse=reselse }
		end
	      in 
	      add2 entry res;
	      res
  in
  map dd1 bdd2
