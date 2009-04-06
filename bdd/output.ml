(** Output of BDDs/MTBDDs *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** BDD node *)
type bnode =
  | BIte of int * int * bool * int
      (** [BIte(idcond,idnodeThen,signElse,idnodeElse)] *)
  | BTrue
      (** Terminal case. Not needed in principle *)

type 'a bdd = {
  cond : SetteI.t ref;
    (** Reachable conditions *)
  mutable bdef : bnode MappeI.t;
    (** Global BDDs graph *)
  bhash : ('a Cudd.Bdd.t, int) Hashhe.t;
  mutable blastid : int;
    (** Hashtables and Counters for resp. first free BDD or IDD node *)
}

(*  ********************************************************************** *)
(** {2 BDDs} *)
(*  ********************************************************************** *)

let make_bdd ~cond = {
  cond = cond;
  bdef = MappeI.empty;
  bhash = Hashhe.create 23;
  blastid = -1;
}

let signid_of_bdd (db:'a bdd) (bdd:'a Cudd.Bdd.t)
    :
    bool * int
    =
  let rec iter (bdd:'a Cudd.Bdd.t) =
    let res =
      let cmpl = Cudd.Bdd.is_complement bdd in
      let bdd = if cmpl then Cudd.Bdd.dnot bdd else bdd in
      try
	let id = Hashhe.find db.bhash bdd in
	(cmpl,id)
      with Not_found ->
	let bdef =
	  begin match Cudd.Bdd.inspect bdd with
	  | Cudd.Bdd.Ite(cond,dthen,delse) ->
	      db.cond := SetteI.add cond !(db.cond);
	      let (b1,id1) = iter dthen in
	      let (b2,id2) = iter delse in
	      assert(not b1);
	      BIte(cond,id1,b2,id2)
	  | Cudd.Bdd.Bool(b) ->
	      assert(b);
	      BTrue
	  end
	in
	db.blastid <- db.blastid + 1;
	Hashhe.add db.bhash bdd db.blastid;
	db.bdef <- MappeI.add db.blastid bdef db.bdef;
	(cmpl,db.blastid)
    in
    res
  in
  iter bdd

(*  ********************************************************************** *)
(** {2 MTBDDs} *)
(*  ********************************************************************** *)

(** MTBDD node *)
type 'a mnode =
  | MIte of int * int * int
      (** VIte(idcond,idnodeThen,idnodeElse) *)
  | MCst of 'a
      (** Leaf *)

(** Database *)
type 'a mtbdd = {
  cond : SetteI.t ref;
    (** Reachable conditions *)
  mutable mdef : 'a mnode MappeI.t;
    (** Global IDDs graph *)
  lhash : ('a Cudd.Mtbdd.unique, unit) Hashhe.Custom.t;
    (** Reachable MTBDD leafs *)
  mhash : ('a Cudd.Mtbdd.t, int) Hashhe.t;
  mutable mlastid : int;
    (** Hashtables and Counters for MTBDD nodes. *)
}

let make_mtbdd ~(table:'a Cudd.Mtbdd.table) ~cond = 
  let compare = table.Cudd.Weakke.Custom.compare in
  let hash = compare.Cudd.Weakke.hash in
  let equal = compare.Cudd.Weakke.equal in
  {
    cond = cond;
    mdef = MappeI.empty;
    lhash = Hashhe.Custom.create hash equal 23;
    mhash = Hashhe.create 23;
    mlastid = -1;
  }

let id_of_mtbdd 
    (db:'a mtbdd) (mtbdd:'a Cudd.Mtbdd.t)
    :
    int
    =
  let rec iter (mtbdd:'a Cudd.Mtbdd.t) : int
      =
    try
      Hashhe.find db.mhash mtbdd
    with Not_found ->
      let mdef =
	begin match Cudd.Mtbdd.inspect mtbdd with
	| Cudd.Mtbdd.Ite(cond,dthen,delse) ->
	    db.cond := SetteI.add cond !(db.cond);
	    let idthen = iter dthen in
	    let idelse = iter delse in
	    MIte(cond,idthen,idelse)
	| Cudd.Mtbdd.Leaf(cst) ->
	    Hashhe.Custom.replace db.lhash cst ();
	    MCst(Cudd.Mtbdd.get cst)
	end
      in
      db.mlastid <- db.mlastid + 1;
      Hashhe.add db.mhash mtbdd db.mlastid;
      db.mdef <- MappeI.add db.mlastid mdef db.mdef;
      db.mlastid
  in
  iter mtbdd

(*  ********************************************************************** *)
(** {3 RDDs} *)
(*  ********************************************************************** *)

(** RDD node *)
type rnode = 
  | RIte of int * int * int 
      (** RIte(idcond,idnodeThen,idnodeElse) *)
  | RCst of float

(** Database *)
type rdd = {
  cond : SetteI.t ref;
  mutable rdef : rnode MappeI.t;
  mutable lset : float Sette.t;
  rhash : (Cudd.Rdd.t, int) Hashhe.t;
  mutable rlastid : int;
}

let make_rdd ~cond = 
  {
    cond = cond;
    rdef = MappeI.empty;
    lset = Sette.empty;
    rhash = Hashhe.create 23;
    rlastid = -1;
  }

let id_of_rdd 
    (db:rdd) (rdd:Cudd.Rdd.t)
    :
    int
    =
  let rec iter (rdd:Cudd.Rdd.t) : int
      =
    try
      Hashhe.find db.rhash rdd
    with Not_found ->
      let mdef =
	begin match Cudd.Rdd.inspect rdd with
	| Cudd.Rdd.Ite(cond,dthen,delse) ->
	    db.cond := SetteI.add cond !(db.cond);
	    let idthen = iter dthen in
	    let idelse = iter delse in
	    RIte(cond,idthen,idelse)
	| Cudd.Rdd.Leaf(cst) ->
	    db.lset <- Sette.add cst db.lset;
	    RCst(cst)
	end
      in
      db.rlastid <- db.rlastid + 1;
      Hashhe.add db.rhash rdd db.rlastid;
      db.rdef <- MappeI.add db.rlastid mdef db.rdef;
      db.rlastid
  in
  iter rdd

(*  ********************************************************************** *)
(** {2 Iterators} *)
(*  ********************************************************************** *)

let iter_cond_ordered (cond:SetteI.t) (manager:'a Cudd.Man.t) (f:int -> unit) : unit
  =
  let size = Cudd.Man.get_bddvar_nb manager in
  for level=0 to pred size do
    let var = Cudd.Man.var_of_level manager level in
    if SetteI.mem var cond then
      f var
  done;
  ()

let iter_bdef_ordered (db:'a bdd) (f:int -> bnode -> unit) : unit
  =
  MappeI.iter f db.bdef

let iter_mdef_ordered (db:'a mtbdd) (f:int -> 'a mnode -> unit) : unit
  =
  MappeI.iter f db.mdef
