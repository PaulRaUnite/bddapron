(** Normalized condition environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

let compare_idb = Env.compare_idb

class type ['a,'b,'c] t = object('d)
  constraint 'b = < bddindex : int; bddindex0 : int; .. >

  method permutation : int array
    (** Compute the permutation for normalizing the environment *)
  method permute : int array -> unit
    (** Apply the given permutation to the environment *)
  method normalize : int array
    (** Combine the two previous functions, and return the permutation *)

  method compute_careset : normalized:bool -> unit
    (** Computes the careset, using [self#compare_cond] *)
  method reduce : 'c Cudd.Bdd.t -> unit
    (** Remove from the environment all conditions that do not
	belong to the given support. Does not perform
	normalization (so there may be "holes" in the allocation
	of indices *)
  method clear : unit
    (** Clear all the conditions (results in a normalized environments) *)

  method cond_of_idb : int*bool -> 'a
  method idb_of_cond : 'b -> 'a -> int*bool

  val v_compare_cond : 'a -> 'a -> int
  val v_negate_cond : 'b -> 'a -> 'a
  val v_support_cond : 'b -> 'a -> string PSette.t
  val mutable v_print_cond : 'b -> Format.formatter -> 'a -> unit
  val v_cudd : 'c Cudd.Man.t
    (** CUDD manager *)

  val mutable v_bddindex0 : int
    (** First index for finite-type variables *)
  val mutable v_bddsize : int
    (** Number of indices dedicated to finite-type variables *)
  val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
  val v_bddincr : int
  val mutable v_cond : ('a,int*bool) PDMappe.t
    (** Two-way association between a condition and a pair of a BDD index and a polarity *)
  val mutable v_cond_supp : 'c Cudd.Bdd.t
    (** Support of conditions *)
  val mutable v_careset : 'c Cudd.Bdd.t
    (** Boolean formula indicating which logical
	combination known as true could be exploited for simplification.
	For instance, [x>=1 => x>=0]. *)

  method compare_cond : 'a -> 'a -> int
  method negate_cond : 'b -> 'a -> 'a
  method support_cond : 'b -> 'a -> string PSette.t
  method print_cond : 'b -> Format.formatter -> 'a -> unit

  method cudd : 'c Cudd.Man.t
  method bddindex0 : int
  method bddsize : int
  method bddindex : int
  method bddincr : int
  method cond : ('a,int*bool) PDMappe.t
  method cond_supp : 'c Cudd.Bdd.t
  method careset : 'c Cudd.Bdd.t

  method set_bddindex0 : int -> unit
  method set_bddsize : int -> unit
  method set_bddindex : int -> unit
  method set_cond : ('a,int*bool) PDMappe.t -> unit
  method set_cond_supp : 'c Cudd.Bdd.t -> unit
  method set_careset : 'c Cudd.Bdd.t -> unit
end

  (*  ============================================================ *)
  (** {3 Creating the class [env]} *)
  (*  ============================================================ *)

class ['a,'b,'c] make
  ?(bddindex0=100)
  ?(bddsize=300)
  (man:'c Cudd.Man.t)
  ~(compare_cond: 'a -> 'a -> int)
  ~(negate_cond:'b -> 'a -> 'a)
  ~(support_cond:('b -> 'a -> string PSette.t))
  ~(print_cond: 'b -> Format.formatter -> 'a -> unit)
  :
  ['a,'b,'c] t
  =
object(self)

  val v_compare_cond = compare_cond
  val v_negate_cond = negate_cond
  val v_support_cond = support_cond
  val mutable v_print_cond = print_cond

  val v_cudd = man
  val mutable v_bddindex0 = bddindex0
  val mutable v_bddsize = bddsize
  val mutable v_bddindex = bddindex0
  val v_bddincr = 1
  val mutable v_cond = PDMappe.empty compare_cond compare_idb
  val mutable v_cond_supp = Cudd.Bdd.dtrue man
  val mutable v_careset = Cudd.Bdd.dtrue man

  method compare_cond = compare_cond
  method negate_cond = negate_cond
  method support_cond = support_cond
  method print_cond = print_cond

  method cudd = v_cudd
  method bddindex0 = v_bddindex0
  method bddsize = v_bddsize
  method bddindex = v_bddindex
  method bddincr = v_bddincr
  method cond = v_cond
  method cond_supp = v_cond_supp
  method careset = v_careset
  method set_bddindex0 x = v_bddindex0 <- x
  method set_bddsize x = v_bddsize <- x
  method set_bddindex x = v_bddindex <- x
  method set_cond cond = v_cond <- cond
  method set_cond_supp supp = v_cond_supp <- supp
  method set_careset x = v_careset <- x

  method permutation : int array
    =
    let perm = Array.init (Cudd.Man.get_bddvar_nb v_cudd) (fun i -> i) in
    let index = ref 0 in
    PDMappe.iter
      (begin fun cond (id,b) ->
	if b then begin
	  perm.(id) <- !index;
	  index := !index + v_bddincr;
	end
      end)
      v_cond
    ;
    perm

  method permute (perm:int array) : unit
    =
    v_cond <-
      (PDMappe.fold
	(begin fun cond (id,b) res ->
	  PDMappe.add cond (perm.(id),b) res
	end)
	v_cond
	(PDMappe.empty v_compare_cond compare_idb))
    ;
    v_cond_supp <- (Cudd.Bdd.permute v_cond_supp perm);
    v_careset <- (Cudd.Bdd.permute v_careset perm);
    ()

  method normalize : int array =
    let perm = self#permutation in
    self#permute perm;
    perm

  method reduce supp = 
    let suppr = Cudd.Bdd.support_diff v_cond_supp supp in
    v_careset <- Cudd.Bdd.exist suppr v_careset;
    v_cond_supp <- Cudd.Bdd.cofactor v_cond_supp suppr;
    let suppr = Cudd.Bdd.list_of_support suppr in
    let setidb = 
      List.fold_left 
	(fun res id -> 
	  PSette.add (id,true) (PSette.add (id,false) res)
	)
	(PSette.empty compare_idb)
	suppr
    in
    v_cond <- PDMappe.diffsety v_cond setidb;
    ()

  method clear =
    let dtrue = Cudd.Bdd.dtrue v_cudd in
    v_cond <- (PDMappe.empty self#compare_cond compare_idb);
    v_cond_supp <- dtrue;
    v_careset <- dtrue;
    v_bddindex <- v_bddindex0

  method compute_careset
    ~(normalized:bool)
    :
    unit
    =
    v_careset <- (Cudd.Bdd.dtrue v_cudd);
    let list =
      PMappe.fold
	(begin fun cons (id,b) res ->
	  if b then (cons,id) :: res else res
	end)
	(PDMappe.mapx v_cond)
	[]
    in
    let list =
      if normalized then list
      else
	List.fast_sort
	  (fun (cons2,id2) (cons1,id1) -> self#compare_cond cons1 cons2)
	  list
    in
    let rec parcours = function
      | (cons2,id2)::( ((cons1,id1)::_) as t) ->
	  let cmp = self#compare_cond cons1 cons2 in
	  if cmp = (-1) then begin
	    v_careset <-
	      Cudd.Bdd.dand v_careset
	      (Cudd.Bdd.dor (Cudd.Bdd.ithvar v_cudd id2)
		(Cudd.Bdd.dnot (Cudd.Bdd.ithvar v_cudd id1)))
	  end;
	  parcours t
      | _ -> ()
    in
    parcours list;
    ()

  method cond_of_idb idb = PDMappe.x_of_y idb v_cond
  method idb_of_cond (env:'b) cond : int*bool =
    try PDMappe.y_of_x cond v_cond
    with Not_found ->
      let ncond = v_negate_cond env cond in
      let id = v_bddindex in
      if id>=v_bddindex0+v_bddsize then raise Env.Bddindex;
      let b = (v_compare_cond cond ncond) < 0 in
      v_cond <-
	(PDMappe.add
	  cond (id,b)
	  (PDMappe.add
	    ncond (id,not b)
	    v_cond));
      v_bddindex <- v_bddindex + v_bddincr;
      let bdd = (Cudd.Bdd.ithvar v_cudd id) in
      v_cond_supp <- Cudd.Bdd.dand bdd v_cond_supp;
      (id,b)

end

let make = new make

let print (env:'b) fmt (cond:('a,'b,'c) #t) =
  fprintf fmt
    "{@[<v>bddindex0 = %i; bddindex = %i; cond = %a;@ cond_supp = %a@ careset = %a@]}"
    cond#bddindex0 cond#bddindex
    (PDMappe.print
      (cond#print_cond env)
      (fun fmt (id,b) -> fprintf fmt "(%i,%b)" id b))
    cond#cond
    (Cudd.Bdd.print_minterm pp_print_int) cond#cond_supp
    (Cudd.Bdd.print_minterm pp_print_int) cond#careset

let check_normalized (env:'b) (cond:('a,'b,'c) #t) : bool
    =
  try
    let index = ref 0 in
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
	  index := !index + cond#bddincr;
	end
      end)
      cond#cond
    ;
    true
  with Exit ->
    false

let is_leq (cond1:('a,'b1,'c) #t) (cond2:('a,'b2,'c) #t) : bool =
  cond1==(Obj.magic cond2) ||
    cond1#cudd = cond2#cudd &&
      (cond1#bddindex - cond1#bddindex0 <= cond2#bddindex - cond2#bddindex0) &&
      (cond1#cond==Obj.magic cond2#cond || PMappe.subset (fun _ _ -> true)
	(PDMappe.mapx cond1#cond)
	(PDMappe.mapx cond2#cond))

let is_eq (cond1:('a,'b1,'c) #t) (cond2:('a,'b2,'c) #t) : bool =
  cond1==(Obj.magic cond2) ||
    cond1#cudd = cond2#cudd &&
      (cond1#bddindex - cond1#bddindex0 = cond2#bddindex - cond2#bddindex0) &&
      (cond1#cond==Obj.magic cond2#cond || (PDMappe.equaly cond1#cond cond2#cond))

let shift (cond:(('a,'b,'c) #t as 'd)) (offset:int) : 'd =
  let perm = Env.permutation_of_offset cond#bddindex offset in
  let ncond = Oo.copy cond in
  ncond#set_bddindex0 (ncond#bddindex0 + offset);
  ncond#permute perm;
  ncond

let lce (cond1:(('a,'b,'c) #t as 'd)) (cond2:'d) : 'd =
  if is_leq cond2 cond1 then
    let offset = cond1#bddindex0 - cond2#bddindex0 in
    if offset>=0 then 
      cond1 
    else  
      shift cond1 (-offset)
  else if is_leq cond1 cond2 then
    let offset = cond2#bddindex0 - cond1#bddindex0 in
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
	PDMappe.fold (add 1) cond1#cond (PMappe.empty cond1#compare_cond)
      in
      let map12 =
	PDMappe.fold (add 2) cond2#cond map1
      in
      map12
    in
    let index0 = Pervasives.max cond1#bddindex0 cond2#bddindex0 in
    let size = Pervasives.max cond1#bddsize cond2#bddsize in
    let index = ref index0 in
    let incr = cond1#bddincr in
    let newcond = ref (PDMappe.empty cond1#compare_cond compare_idb) in
    PMappe.iter
      (begin fun pcond (k,id) ->
	let ncond =
	  (if k=1 then cond1 else cond2)#cond_of_idb (id,false)
	in
	if !index >= index0+size then raise Env.Bddindex;
	newcond :=
	  PDMappe.add pcond (!index,true)
	    (PDMappe.add ncond (!index,false) !newcond);
	index := !index + incr;
      end)
      mapcondkid
    ;
    let cond = Oo.copy cond1 in
    cond#set_bddindex0 index0;
    cond#set_bddsize size;
    cond#set_bddindex !index;
    cond#set_cond !newcond;
    cond#set_cond_supp (Cudd.Bdd.dtrue cond#cudd);
    cond#set_careset (Cudd.Bdd.dtrue cond#cudd);
    cond#compute_careset ~normalized:true;
    cond
  end

let permutation12 (cond1:('a,'b1,'c) #t) (cond2:('a,'b2,'c) #t) : int array
  =
  assert(is_leq cond1 cond2);
  let perm = Array.init cond1#bddindex (fun i -> i) in
  let offset = ref (cond2#bddindex0 - cond1#bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1#cond in
	  assert b1;
	  perm.(id1) <- id1 + !offset
	with Not_found ->
	  offset := !offset + cond2#bddincr
      end
    end)
    (PDMappe.mapx cond2#cond)
  ;
  perm

let permutation21 (cond2:('a,'b2,'c) #t) (cond1:('a,'b1,'c) #t) : int array
    =
  assert(is_leq cond1 cond2);
  let perm = Array.init cond2#bddindex (fun i -> i) in
  let offset = ref (cond2#bddindex0 - cond1#bddindex0) in
  PMappe.iter
    (begin fun cons2 (id2,b2) ->
      if b2 then begin
	try
	  let (id1,b1) = PDMappe.y_of_x cons2 cond1#cond in
	  assert b1;
	  perm.(id1 + !offset) <- id1
	with Not_found ->
	  offset := !offset + cond2#bddincr;
      end
    end)
    (PDMappe.mapx cond2#cond)
  ;
  perm

type ('a,'b) value = {
  cond : 'a;
  val1 : 'b
}

let make_value cond val1 =
  { cond=cond; val1=val1 }
    
