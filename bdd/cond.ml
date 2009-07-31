(** Normalized condition environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Datatypes } *)
(*  ********************************************************************** *)

type ('a,'b,'c) t = {
  compare_cond : 'a -> 'a -> int;
  negate_cond : 'b -> 'a -> 'a;
  support_cond : 'b -> 'a -> string PSette.t;
  mutable print_cond : 'b -> Format.formatter -> 'a -> unit;

  cudd : 'c Cudd.Man.t;
    (** CUDD manager *)
  mutable bddindex0 : int;
    (** First index for finite-type variables *)
  mutable bddsize : int;
    (** Number of indices dedicated to finite-type variables *)
  mutable bddindex : int;
    (** Next free index in BDDs used by [self#add_var]. *)
  bddincr : int;
  mutable condidb : ('a,int*bool) PDMappe.t;
    (** Two-way association between a condition and a pair of a
	BDD index and a polarity *)
  mutable cond_supp : 'c Cudd.Bdd.t;
    (** Support of conditions *)
  mutable careset : 'c Cudd.Bdd.t;
    (** Boolean formula indicating which logical
	combination known as true could be exploited for simplification.
	For instance, [x>=1 => x>=0]. *)
}

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

let print (env:'b) fmt (cond:('a,'b,'c) t) =
  fprintf fmt
    "{@[<v>bddindex0 = %i; bddindex = %i; cond = %a;@ cond_supp = %a@ careset = %a@]}"
    cond.bddindex0 cond.bddindex
    (PDMappe.print
      (cond.print_cond env)
      (fun fmt (id,b) -> fprintf fmt "(%i,%b)" id b))
    cond.condidb
    (Cudd.Bdd.print_minterm pp_print_int) cond.cond_supp
    (Cudd.Bdd.print_minterm pp_print_int) cond.careset

(*  ********************************************************************** *)
(** {2 Constructors} *)
(*  ********************************************************************** *)

let compare_idb = Env.compare_idb

let make
    ?(bddindex0=100)
    ?(bddsize=300)
    (cudd:'c Cudd.Man.t)
    ~(compare_cond: 'a -> 'a -> int)
    ~(negate_cond:'b -> 'a -> 'a)
    ~(support_cond:('b -> 'a -> string PSette.t))
    ~(print_cond: 'b -> Format.formatter -> 'a -> unit)
    :
    ('a,'b,'c) t
    =
  {
    compare_cond = compare_cond;
    negate_cond = negate_cond;
    support_cond = support_cond;
    print_cond = print_cond;
    cudd = cudd;
    bddindex0 = bddindex0;
    bddsize = bddsize;
    bddindex = bddindex0;
    bddincr = 1;
    condidb = PDMappe.empty compare_cond compare_idb;
    cond_supp = Cudd.Bdd.dtrue cudd;
    careset = Cudd.Bdd.dtrue cudd;
  }

let copy t = { t with cudd = t.cudd }

(*  ********************************************************************** *)
(** {2 Internal functions} *)
(*  ********************************************************************** *)

let permutation t =
  let perm = Array.init (Cudd.Man.get_bddvar_nb t.cudd) (fun i -> i) in
  let index = ref 0 in
  PDMappe.iter
    (begin fun cond (id,b) ->
      if b then begin
	perm.(id) <- !index;
	index := !index + t.bddincr;
      end
    end)
    t.condidb
  ;
  perm

let permute_with t (perm:int array) : unit
    =
  t.condidb <-
    (PDMappe.fold
      (begin fun cond (id,b) res ->
	PDMappe.add cond (perm.(id),b) res
      end)
      t.condidb
      (PDMappe.empty t.compare_cond compare_idb))
  ;
  t.cond_supp <- (Cudd.Bdd.permute t.cond_supp perm);
  t.careset <- (Cudd.Bdd.permute t.careset perm);
  ()

let normalize_with t : int array =
  let perm = permutation t in
  permute_with t perm;
  perm

let reduce_with t supp =
  let suppr = Cudd.Bdd.support_diff t.cond_supp supp in
  t.careset <- Cudd.Bdd.exist suppr t.careset;
  t.cond_supp <- Cudd.Bdd.cofactor t.cond_supp suppr;
  let suppr = Cudd.Bdd.list_of_support suppr in
  List.iter
    (begin fun id ->
      t.condidb <- PDMappe.removey (id,true) t.condidb;
      t.condidb <- PDMappe.removey (id,false) t.condidb;
    end)
    suppr
  ;
(*
  let setidb =
    List.fold_left
      (fun res id ->
	PSette.add (id,true) (PSette.add (id,false) res)
      )
      (PSette.empty compare_idb)
      suppr
  in
  t.condidb <- PDMappe.diffsety t.condidb setidb;
*)
  ()

let clear t =
  let dtrue = Cudd.Bdd.dtrue t.cudd in
  t.condidb <- (PDMappe.empty t.compare_cond compare_idb);
  t.cond_supp <- dtrue;
  t.careset <- dtrue;
  t.bddindex <- t.bddindex0

let check_normalized (env:'b) (cond:('a,'b,'c) t) : bool
    =
  try
    let index = ref cond.bddindex0 in
    PDMappe.iter
      (begin fun _ (id,b) ->
	if b then begin
	  if id <> !index then begin
	    printf
	      "Bdd.Cond.check_normalized: not normalized at index %i@.env=%a@."
	      !index
	      (print env) cond
	    ;
	    raise Exit
	  end;
	  index := !index + cond.bddincr;
	end
      end)
      cond.condidb
    ;
    true
  with Exit ->
    false

(*  ********************************************************************** *)
(** {2 Operations} *)
(*  ********************************************************************** *)

let cond_of_idb t idb = PDMappe.x_of_y idb t.condidb
let idb_of_cond (env:'b) t cond : int*bool =
  try PDMappe.y_of_x cond t.condidb
  with Not_found ->
    let ncond = t.negate_cond env cond in
    let id = t.bddindex in
    if id>=t.bddindex0+t.bddsize then raise Env.Bddindex;
    let b = (t.compare_cond cond ncond) < 0 in
    t.condidb <- PDMappe.add cond (id,b) t.condidb;
    t.condidb <- PDMappe.add ncond (id,not b) t.condidb;
    t.bddindex <- t.bddindex + t.bddincr;
    let bdd = (Cudd.Bdd.ithvar t.cudd id) in
    t.cond_supp <- Cudd.Bdd.dand bdd t.cond_supp;
    if t.bddindex >= t.bddindex0+t.bddsize then raise Env.Bddindex;
    (id,b)

let compute_careset
    (t:('a,'b,'c) t)
    ~(normalized:bool)
    :
    unit
    =
  t.careset <- (Cudd.Bdd.dtrue t.cudd);
  let list =
    PMappe.fold
      (begin fun cons (id,b) res ->
	if b then (cons,id) :: res else res
      end)
      (PDMappe.mapx t.condidb)
      []
  in
  let list =
    if normalized then list
    else
      List.fast_sort
	(fun (cons2,id2) (cons1,id1) -> t.compare_cond cons1 cons2)
	list
  in
  let rec parcours = function
    | (cons2,id2)::( ((cons1,id1)::_) as rest) ->
	let cmp = t.compare_cond cons1 cons2 in
	if cmp = (-1) then begin
	  t.careset <-
	    Cudd.Bdd.dand t.careset
	    (Cudd.Bdd.dor (Cudd.Bdd.ithvar t.cudd id2)
	      (Cudd.Bdd.dnot (Cudd.Bdd.ithvar t.cudd id1)))
	end;
	parcours rest
    | _ -> ()
  in
  parcours list;
  ()

let is_leq (cond1:('a,'b1,'c) t) (cond2:('a,'b2,'c) t) : bool =
  cond1==cond2 ||
    cond1.cudd = cond2.cudd &&
      (cond1.bddindex - cond1.bddindex0 <= cond2.bddindex - cond2.bddindex0) &&
      (cond1.condidb==cond2.condidb || PMappe.subset (fun _ _ -> true)
	(PDMappe.mapx cond1.condidb)
	(PDMappe.mapx cond2.condidb))

let is_eq (cond1:('a,'b,'c) t) (cond2:('a,'b,'c) t) : bool =
  cond1==cond2 ||
    cond1.cudd = cond2.cudd &&
      (cond1.bddindex - cond1.bddindex0 = cond2.bddindex - cond2.bddindex0) &&
      (cond1.condidb==cond2.condidb || (PDMappe.equaly cond1.condidb cond2.condidb))

let shift (cond:('a,'b,'c) t) (offset:int) : ('a,'b,'c) t =
  let perm = Env.permutation_of_offset cond.bddindex offset in
  let ncond = copy cond in
  ncond.bddindex0 <- ncond.bddindex0 + offset;
  permute_with ncond perm;
  ncond

let lce (cond1:('a,'b,'c) t) (cond2:('a,'b,'c) t) : ('a,'b,'c) t =
  if is_leq cond2 cond1 then
    let offset = cond1.bddindex0 - cond2.bddindex0 in
    if offset>=0 then
      cond1
    else
      shift cond1 (-offset)
  else if is_leq cond1 cond2 then
    let offset = cond2.bddindex0 - cond1.bddindex0 in
    if offset>=0 then
      cond2
    else
      shift cond2 (-offset)
  else begin
    let mapcondkid =
      let add k cond (id,b) res =
	if b then PMappe.add cond (k,id) res else res
      in
      let map1 =
	PDMappe.fold (add 1) cond1.condidb (PMappe.empty cond1.compare_cond)
      in
      let map12 =
	PDMappe.fold (add 2) cond2.condidb map1
      in
      map12
    in
    let cond = copy cond1 in
    cond.bddindex0 <- Pervasives.max cond1.bddindex0 cond2.bddindex0;
    cond.bddsize <- Pervasives.max cond1.bddsize cond2.bddsize;
    clear cond;
    PMappe.iter
      (begin fun pcond (k,id) ->
	let ncond = cond_of_idb (if k=1 then cond1 else cond2) (id,false) in
	if cond.bddindex >= cond.bddindex0+cond.bddsize then raise Env.Bddindex;
	cond.condidb <- PDMappe.add pcond (cond.bddindex,true) cond.condidb;
	cond.condidb <- PDMappe.add ncond (cond.bddindex,false) cond.condidb;
	let bdd = (Cudd.Bdd.ithvar cond.cudd id) in
	cond.cond_supp <- Cudd.Bdd.dand bdd cond.cond_supp;
	cond.bddindex <- cond.bddindex + cond.bddincr
      end)
      mapcondkid
    ;
    compute_careset cond ~normalized:true;
    cond
  end

let permutation12 (cond1:('a,'b,'c) t) (cond2:('a,'b,'c) t) : int array
  =
  assert(is_leq cond1 cond2);
  let perm = Array.init cond1.bddindex (fun i -> i) in
  let offset = ref (cond2.bddindex0 - cond1.bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1.condidb in
	  assert b1;
	  perm.(id1) <- id1 + !offset
	with Not_found ->
	  offset := !offset + cond2.bddincr
      end
    end)
    (PDMappe.mapx cond2.condidb)
  ;
  perm

let permutation21 (cond2:('a,'b,'c) t) (cond1:('a,'b,'c) t) : int array
    =
  assert(is_leq cond1 cond2);
  let perm = Array.init cond2.bddindex (fun i -> i) in
  let offset = ref (cond2.bddindex0 - cond1.bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1.condidb in
	  assert b1;
	  perm.(id1 + !offset) <- id1
	with Not_found ->
	  offset := !offset + cond2.bddincr;
      end
    end)
    (PDMappe.mapx cond2.condidb)
  ;
  perm

(*  ********************************************************************** *)
(** {2 Level 2} *)
(*  ********************************************************************** *)

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}

let make_value cond val1 =
  { cond=cond; val1=val1 }
