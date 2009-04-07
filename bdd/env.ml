(** Normalized managers/environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Datatypes } *)
(*  ********************************************************************** *)

(** Type defintion *)
type typdef = [
  | `Benum of string array
]

(** Types *)
type typ = [
  | `Bool
  | `Bint of (bool * int)
  | `Benum of string
]

(** (Internal type:) Expressions *)
type 'a expr = [
  | `Bool of 'a Cudd.Bdd.t
      (** Boolean *)
  | `Bint of 'a Int.t
      (** Bounded integer *)
  | `Benum of 'a Enum.t
      (** Enumerated *)
]

let print_typ (fmt:Format.formatter) typ = match typ with
  | `Bool -> pp_print_string fmt "bool"
  | `Bint(sign,size) -> fprintf fmt "bint(%b,%i)" sign size
  | `Benum s -> pp_print_string fmt s
  | _ -> pp_print_string fmt "Bddexpr.print_typ: unknown type"

let print_typdef (fmt:Format.formatter) typdef = match typdef with
  | `Benum array ->
      fprintf fmt "benum{%a}"
	(Print.array ~first:"" ~sep:"," ~last:"" pp_print_string)
	array
  | _ -> pp_print_string fmt "Bddexpr.print_typdef: unknown type definition"


let compare_idb (id1,b1) (id2,b2) =
  let res = id1-id2 in
  if res!=0 then
    res
  else
    (if b1 then 1 else 0) - (if b2 then 1 else 0)

let print_tid (fmt:Format.formatter) (tid:int array) : unit =
  Print.array Format.pp_print_int fmt tid

(*  ********************************************************************** *)
(** {2 Opened signature} *)
(*  ********************************************************************** *)

module O = struct
  class type ['a,'b,'c,'d] t = object('e)
    constraint 'a = [> typ]
    constraint 'b = [> typdef]
    constraint 'c = [> ]

    method add_typ : string -> 'b -> unit
    (** Declaration of a new type *)
    method mem_typ : string -> bool
    (** Is the type defined in the database ? *)
    method typdef_of_typ : string -> 'b
    (** Return the definition of the type *)
    method add_vars : (string * 'a) list -> int array option
    (** Add the set of variables, possibly normalize the
	environment and return the applied permutation (that
	should also be applied to expressions defined in this
	environment) *)
    method remove_vars : string list -> int array option
    (** Remove the set of variables, possibly normalize the
	environment and return the applied permutation. *)
    method rename_vars : (string * string) list -> int array option
    (** Rename the variables, possibly normalize the environment
	and return the applied permutation. *)
    method mem_var : string -> bool
    (** Is the label/var defined in the database ? *)
    method typ_of_var : string -> 'a
    (** Return the type of the label/variable *)
    method vars : string PSette.t
    (** Return the list of variables *)

    method permutation : int array
    (** Compute the permutation fornormalizing the environment *)
    method permute : int array -> unit
    (** Apply the given permutation to the environment *)
    method normalize : int array
    (** Combine the two previous functions, and return the permutation *)

    method compute_careset : normalized:bool -> unit
    (** Computes the careset, using [self#compare_cond] *)
    method clear_cond : int array option
    (** Remove the condition of an environment *)
    method pair : unit
    (** Assuming [bddindex] is odd and [bddincr=2]), if [v_paired=false],
	groups variable by pair *)

    method add_var : string -> 'a -> unit
    (** Private function *)

    method cond_of_idb : int*bool -> 'c
    method idb_of_cond : 'c -> int*bool

    method print_idcondb : Format.formatter -> int*bool -> unit
    method print_order : Format.formatter -> unit
    (** Print the BDD variable ordering *)

    val v_cudd : 'd Cudd.Man.t
    (** CUDD manager *)
    val mutable v_paired : bool
    (** Are pair groups active (default false) ? *)
    val mutable v_typdef : (string, 'b) PMappe.t
    (** Named types definitions *)
    val mutable v_vartyp : (string, 'a) PMappe.t
    (** Associate to a var/label its type *)
    val mutable v_boolfirst : bool
    (** If true, when normalizing,
	Boolean variables are putabove conditions, otherwise below *)
    val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
    val mutable v_bddincr : int
    (** Increment used by [self#add_var] for incrementing
	[self#_v_bddindex] *)
    val mutable v_idcondvar : (int, string) PMappe.t
    (** Associates to a BDD index the variable involved by it *)
    val mutable v_vartid : (string, int array) PMappe.t
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
    val mutable v_varset : (string, 'd Cudd.Bdd.t) PMappe.t
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
    val v_compare_cond : 'c -> 'c -> int
    val v_negate_cond : ('a,'b,'c,'d) t -> 'c -> 'c
    val v_support_cond : ('a,'b,'c,'d) t -> 'c -> string PSette.t
    val mutable v_print_cond : ('a,'b,'c,'d) t -> Format.formatter -> 'c -> unit
    val mutable v_cond : ('c,int*bool) PDMappe.t
    (** Two-way association between a condition and a pair of a BDD index and a polarity *)
    val mutable v_cond_supp : 'd Cudd.Bdd.t
    (** Support of conditions *)
    val mutable v_careset : 'd Cudd.Bdd.t
    (** Boolean formula indicating which logical
	combination known as true could be exploited for simplification.
	For instance, [x>=1 => x>=0]. *)
    val mutable v_print_external_idcondb : Format.formatter -> int*bool -> unit
    (** Printing conditions not managed by [Bddvar].
	By default, [pp_print_int]. *)

    method cudd : 'd Cudd.Man.t
    method typdef : (string, 'b) PMappe.t
    method vartyp : (string, 'a) PMappe.t
    method boolfirst : bool
    method bddindex : int
    method bddincr : int
    method idcondvar : (int, string) PMappe.t
    method vartid : (string, int array) PMappe.t
    method varset : (string, 'd Cudd.Bdd.t) PMappe.t
    method compare_cond : 'c -> 'c -> int
    method negate_cond : 'c -> 'c
    method support_cond : 'c -> string PSette.t
    method print_cond : Format.formatter -> 'c -> unit
    method cond : ('c,int*bool) PDMappe.t
    method cond_supp : 'd Cudd.Bdd.t
    method careset : 'd Cudd.Bdd.t
    method print_external_idcondb : Format.formatter -> int*bool -> unit
    method set_bddindex : int -> unit
    method set_bddincr : int -> unit
    method set_boolfirst : bool -> unit
    method set_idcondvar : (int, string) PMappe.t -> unit
    method set_vartid : (string, int array) PMappe.t -> unit
    method set_varset : (string, 'd Cudd.Bdd.t) PMappe.t -> unit
    method set_typdef : (string, 'b) PMappe.t -> unit
    method set_vartyp : (string, 'a) PMappe.t -> unit
    method set_cond : ('c,int*bool) PDMappe.t -> unit
    method set_cond_supp : 'd Cudd.Bdd.t -> unit
    method set_careset : 'd Cudd.Bdd.t -> unit
    method set_print_external_idcondb :
      (Format.formatter -> int*bool -> unit) -> unit
  end

  (*  ============================================================ *)
  (** {3 Creating the class [env]} *)
  (*  ============================================================ *)

  class ['a,'b,'c,'d] make
    (man:'d Cudd.Man.t)
    ?(boolfirst=true)
    ?(relational=false)
    ~(compare_cond: 'c -> 'c -> int)
    ~(negate_cond:('a,'b,'c,'d) t -> 'c -> 'c)
    ~(support_cond:(('a,'b,'c,'d) t -> 'c -> string PSette.t))
    ~(print_cond: ('a,'b,'c,'d) t -> Format.formatter -> 'c -> unit)
    :
    ['a,'b,'c,'d] t
    =
  object(self:'e)

    val v_cudd = man
    val mutable v_paired = false
    val mutable v_typdef = PMappe.empty String.compare
    val mutable v_vartyp = PMappe.empty String.compare
    val mutable v_boolfirst = boolfirst
    val mutable v_bddindex = 0
    val mutable v_bddincr = if relational then 2 else 1
    val mutable v_idcondvar = PMappe.empty (-)
    val mutable v_vartid = PMappe.empty String.compare
    val mutable v_varset = PMappe.empty String.compare
    val v_compare_cond = compare_cond
    val v_negate_cond = negate_cond
    val v_support_cond = support_cond
    val mutable v_print_cond = print_cond
    val mutable v_cond = PDMappe.empty compare_cond compare_idb
    val mutable v_cond_supp = Cudd.Bdd.dtrue man
    val mutable v_careset = Cudd.Bdd.dtrue man
    val mutable v_print_external_idcondb =
      begin fun fmt (id,b) ->
	fprintf fmt "%s%i" (if b then "not " else "") id
      end

    method cudd = v_cudd
    method typdef = v_typdef
    method vartyp = v_vartyp
    method boolfirst = v_boolfirst
    method bddindex = v_bddindex
    method bddincr = v_bddincr
    method idcondvar = v_idcondvar
    method vartid = v_vartid
    method varset = v_varset
    method compare_cond = compare_cond
    method negate_cond c = negate_cond (self:>('a,'b,'c,'d) t) c
    method support_cond c = support_cond (self:>('a,'b,'c,'d) t) c
    method print_cond = print_cond (self:>('a,'b,'c,'d) t)
    method cond = v_cond
    method cond_supp = v_cond_supp
    method careset = v_careset
    method print_external_idcondb = v_print_external_idcondb
    method set_typdef x = v_typdef <- x
    method set_vartyp x = v_vartyp <- x
    method set_boolfirst x = v_boolfirst <- x
    method set_bddindex x = v_bddindex <- x
    method set_bddincr x = v_bddincr <- x
    method set_idcondvar x = v_idcondvar <- x
    method set_vartid x = v_vartid <- x
    method set_varset x = v_varset <- x
    method set_cond cond = v_cond <- cond
    method set_cond_supp supp = v_cond_supp <- supp
    method set_careset x = v_careset <- x
    method set_print_external_idcondb x = v_print_external_idcondb <- x

    method permutation : int array
      =
      let perm = Array.init v_bddindex (fun i -> i) in
      let index = ref 0 in
      let bddvar () =
	PMappe.iter
	  (begin fun var tid ->
	    Array.iter
	      (begin fun id ->
		perm.(id) <- !index;
		index := !index + v_bddincr;
	      end)
	      tid
	  end)
	  v_vartid
      and bddcond () =
	PDMappe.iter
	  (begin fun cond (id,b) ->
	    if b then begin
	      perm.(id) <- !index;
	      index := !index + v_bddincr;
	    end
	  end)
	  v_cond
      in
      if v_boolfirst
      then (bddvar (); bddcond ())
      else (bddcond (); bddvar ())
      ;
      perm

    method permute (perm:int array) : unit
      =
      v_idcondvar <-
	(PMappe.fold
	  (begin fun idcond var res ->
	    PMappe.add perm.(idcond) var res
	  end)
	  v_idcondvar
	  (PMappe.empty (-)))
      ;
      v_vartid <-
	(PMappe.map
	  (begin fun tid ->
	    Array.map (fun id -> perm.(id)) tid
	  end)
	  v_vartid)
      ;
      v_varset <-
	(PMappe.map
	  (begin fun set -> Cudd.Bdd.permute set perm end)
	  v_varset)
      ;
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



    method add_typ (typ:string) (typdef:'b) : unit
      =
      if PMappe.mem typ v_typdef then
	failwith (sprintf "Bdd.Env.t#add_typ: type %s already defined" typ)
      ;
      v_typdef <- PMappe.add typ typdef v_typdef;
      begin match typdef with
      | `Benum labels ->
	  let t = `Benum typ in
	  Array.iter
	    (begin fun label -> v_vartyp <- PMappe.add label t v_vartyp end)
	    labels
      | _ -> ()
      end

    method mem_typ (typ:string) : bool =
      PMappe.mem typ v_typdef

    method typdef_of_typ (typ:string) : 'b
      =
      try
	PMappe.find typ v_typdef
      with Not_found ->
	failwith ("Bdd.Env.t#typdef_of_typ: unknown type "^typ)

    method mem_var (label:string) : bool =
      PMappe.mem label v_vartyp
    method typ_of_var (label:string) : 'a
      =
      try
	PMappe.find label v_vartyp
      with Not_found ->
	failwith ("Bdd.Env.t#typ_of_var: unknwon label/variable "^label)

    method vars =
      PMappe.maptoset v_vartid

    method add_var var typ : unit
      =
      if PMappe.mem var v_vartyp then
	failwith (sprintf "Bddenv.env#add_var: label/var %s already defined" var)
      ;
      v_vartyp <- PMappe.add var typ v_vartyp;
      begin match typ with
      | #typ as typ ->
	  let tid = match typ with
	    | `Bool -> [| v_bddindex |]
	    | `Bint(b,n) ->
		Array.init n (fun i -> v_bddindex+(v_bddincr*i))
	    | `Benum s ->
		Array.init
		  (Enum.size_of_typ self s)
		  (fun i -> v_bddindex+(v_bddincr*i))
	  in
	  if tid<>[||] then begin
	    v_paired <- false;
	    let oldindex = v_bddindex in
	    v_bddindex <- v_bddindex + v_bddincr*(Array.length tid);
	    Array.iter
	      (fun id -> v_idcondvar <- PMappe.add id var v_idcondvar)
	      tid;
	    for i=oldindex to pred(v_bddindex) do 
	      ignore (Cudd.Bdd.ithvar v_cudd i) 
	    done;
	  end;
	  v_vartid <- PMappe.add var tid v_vartid;
	  ()
      | _ ->
	  ()
      end
	
    method add_vars lvartyp
      :
      int array option
      =
      let oldindex = v_bddindex in 
      List.iter
	(begin fun (var,typ) ->
	  self#add_var var typ
	end)
	lvartyp
      ;
      if oldindex = v_bddindex then
	None
      else
	Some(self#normalize)
	  
    method remove_vars (lvar:string list) : int array option
      =
      let length = ref 0 in
      List.iter
	(begin fun var ->
	  let typ = PMappe.find var v_vartyp in
	  begin match typ with
	  | #typ ->
	      begin try
		let tid = PMappe.find var v_vartid in
		v_vartid <- PMappe.remove var v_vartid;
		length := !length + (Array.length tid)*v_bddincr;
		Array.iter
		  (fun id -> v_idcondvar <- PMappe.remove id v_idcondvar)
		  tid
		;
		begin match typ with
		| `Benum _ ->
		    v_varset <- PMappe.remove var v_varset
		| _ -> ()
		end;
	      with Not_found ->
		failwith
		  (Format.sprintf
		    "Bddenv.remove: trying to remove the label %s of an enumerated type"
		    var)
	      end
	  | _ -> ()
	  end;
	  v_vartyp <- PMappe.remove var v_vartyp;
	end)
	lvar
      ;
      (* conditions *)
      let svar = List.fold_left
	(fun res x -> PSette.add x res)
	(PSette.empty String.compare) lvar
      in
      let supp_removed = ref (Cudd.Bdd.dtrue v_cudd) in
      PDMappe.iter
	(begin fun condition (id,b) ->
	  if b then begin
	    let supp = self#support_cond condition in
	    if not (PSette.is_empty (PSette.inter supp svar)) then begin
	      length := !length + v_bddincr;
	      supp_removed := Cudd.Bdd.dand !supp_removed (Cudd.Bdd.ithvar v_cudd id);
	      v_cond <- PDMappe.remove condition v_cond;
	      let ncondition = PDMappe.x_of_y (id,false) v_cond in
	      v_cond <- PDMappe.remove ncondition v_cond;
	    end
	  end
	end)
	v_cond
      ;
      v_cond_supp <- (Cudd.Bdd.exist !supp_removed v_cond_supp);
      v_careset <- (Cudd.Bdd.exist !supp_removed v_careset);
      if !length = 0 then
	None
      else 
	let perm = self#normalize in
	v_bddindex <- (v_bddindex - !length);
	Some perm

    method rename_vars (lvarvar:(string*string) list)
      :
      int array option
      =
      let length = ref 0 in
      (* we need to distinguish variables without indices from the
	 other one. *)
      let (lvarvartyptidoset,lvarvartyp) =
	List.fold_left
	  (begin fun (res1,res2) (var,nvar) ->
	    let typ = PMappe.find var v_vartyp in
	    v_vartyp <- PMappe.remove var v_vartyp;
	    try
	      let tid = PMappe.find var v_vartid in
	      let oset =
		try Some (PMappe.find var v_varset)
		with Not_found -> None
	      in
	      v_vartid <- PMappe.remove var v_vartid;
	      if oset<>None then
		v_varset <- PMappe.remove var v_varset;
	      Array.iter
		(begin fun id ->
		  v_idcondvar <- PMappe.remove id v_idcondvar
		end)
		tid
	      ;
	      ((var,nvar,typ,tid,oset)::res1,res2)
	    with Not_found ->
	      (res1, (var,nvar,typ)::res2)
	  end)
	  ([],[])
	  lvarvar
      in
      List.iter
	(begin fun (var,nvar,typ) ->
	  if PMappe.mem nvar v_vartyp then
	    failwith
	      (Format.sprintf
		"Bddenv.rename_vars: error, variable %s renamed in already existing %s"
		var nvar)
	  ;
	  v_vartyp <- PMappe.add nvar typ v_vartyp;
	end)
	lvarvartyp
      ;
      List.iter
	(begin fun (var,nvar,typ,tid,oset) ->
	  if PMappe.mem nvar v_vartyp then
	    failwith
	      (Format.sprintf
		"Bddenv.rename_vars: error, variable %s renamed in already existing %s"
		var nvar)
	  ;
	  v_vartyp <- PMappe.add nvar typ v_vartyp;
	  v_vartid <- PMappe.add nvar tid v_vartid;
	  begin match oset with
	  | None -> ()
	  | Some set -> v_varset <- PMappe.add nvar set v_varset
	  end;
	  Array.iter
	    (begin fun id ->
	      v_idcondvar <- PMappe.add id nvar v_idcondvar
	    end)
	    tid
	  ;
	end)
	lvarvartyptidoset
      ;
      (* conditions *)
      let svar = List.fold_left
	(fun res (var,nvar) -> PSette.add var res)
	(PSette.empty String.compare) lvarvar
      in
      let supp_removed = ref (Cudd.Bdd.dtrue v_cudd) in
      PDMappe.iter
	(begin fun condition (id,b) ->
	  if b then begin
	    let supp = self#support_cond condition in
	    if not (PSette.is_empty (PSette.inter supp svar)) then begin
	      length := !length + v_bddincr;
	      supp_removed := Cudd.Bdd.dand !supp_removed (Cudd.Bdd.ithvar v_cudd id);
	      v_cond <- PDMappe.remove condition v_cond;
	      let ncondition = PDMappe.x_of_y (id,false) v_cond in
	      v_cond <- PDMappe.remove ncondition v_cond;
	    end
	  end
	end)
	v_cond
      ;
      v_cond_supp <- (Cudd.Bdd.exist !supp_removed v_cond_supp);
      v_careset <- (Cudd.Bdd.exist !supp_removed v_careset);
      let perm = self#normalize in
      v_bddindex <- (v_bddindex - !length);
      Some perm

    method clear_cond : int array option =
      let dtrue = Cudd.Bdd.dtrue v_cudd in
      let oldcond = v_cond in
      v_cond <- (PDMappe.empty self#compare_cond compare_idb);
      v_cond_supp <- dtrue;
      v_careset <- dtrue;
      if v_boolfirst then begin
	begin
	  try
	    let (id,b) = PMappe.min_key (PDMappe.mapy oldcond) in
	    v_bddindex <- id
	  with Not_found ->
	    ()
	end;
	None
      end
      else begin
	let firstbool =
	  try PMappe.min_key v_idcondvar
	  with Not_found -> (-1)
	in
	v_bddindex <- begin
	  if firstbool = -1
	  then 0
	  else v_bddindex - firstbool
	end
	;
	if firstbool<=0 then
	  None
	else begin
	  let perm = Array.init v_bddindex
	    (begin fun i ->
	      if i*v_bddincr < firstbool then
		i + firstbool
	      else
		i - firstbool
	    end)
	  in
	  self#permute perm;
	  Some perm
	end
      end
	
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

    method pair : unit =
      assert(v_bddindex mod 2 = 0 && v_bddincr=2);
      if not v_paired then begin
	let nb = Cudd.Man.get_bddvar_nb v_cudd in
	for i=0 to max (nb/2) ((nb-1)/2) do
	  Cudd.Man.group v_cudd (i*2) 2 Cudd.Man.MTR_FIXED
	done;
	v_paired <- true;
      end

    method cond_of_idb idb = PDMappe.x_of_y idb v_cond
    method idb_of_cond (cond:'c) : int*bool =
      try PDMappe.y_of_x cond self#cond
      with Not_found ->
	let (ncond:'c) = self#negate_cond cond in
	let id = v_bddindex in
	let b = (v_compare_cond cond ncond) < 0 in
	v_cond <-
	  (PDMappe.add
	    cond (id,b)
	    (PDMappe.add
	      ncond (id,not b)
	      v_cond));
	v_paired <- false;
	v_bddindex <- v_bddindex + v_bddincr;
	let bdd = (Cudd.Bdd.ithvar v_cudd id) in
	v_cond_supp <- Cudd.Bdd.dand bdd v_cond_supp;
	(id,b)

    method print_idcondb fmt ((id,b) as idb) =
      try
	let var = PMappe.find id v_idcondvar in
	let tid = PMappe.find var v_vartid in
	begin match self#typ_of_var var with
	| `Bool -> pp_print_string fmt var
	| _ ->
	    begin
	      try
		for i=0 to pred(Array.length tid) do
		  if id = tid.(i) then begin
		    fprintf fmt "%s%i" var i;
		    raise Exit
		  end
		done;
	      with Exit -> ()
	    end;
	end
      with Not_found ->
	begin try
	  let cond = PDMappe.x_of_y idb v_cond in
	  (print_cond (self:>('a,'b,'c,'d) t)) fmt cond
	with Not_found ->
	  v_print_external_idcondb fmt idb
	end

    method print_order (fmt:Format.formatter) : unit
      =
      let cudd = v_cudd in
      let nb = Cudd.Man.get_bddvar_nb cudd in
      let tab =
	Array.init nb
	  (begin fun var ->
	    let level = Cudd.Man.level_of_var cudd var in
	    (var,level)
	  end)
      in
      Array.sort (fun (v1,l1) (v2,l2) -> Pervasives.compare l1 l2) tab;
      Print.array
	~first:"@[<v>"
	~sep:"@ "
	~last:"@]"
	(begin fun fmt (id,level) ->
	  fprintf fmt "%3i => %3i, %a"
	    level id
	    self#print_idcondb (id,true)
	end)
	fmt
	tab;
      ()

  end

  let print print_typ print_typdef fmt (env:('a,'b,'c,'d) #t) =
    fprintf fmt
      "{@[<v>typdef = %a;@ vartyp = %a;@ bddindex = %i; bddincr = %i;@ idcondvar = %a;@ vartid = %a;@ cond = %a;@ cond_supp = %a@ careset = %a@]}"
      (PMappe.print pp_print_string print_typdef) env#typdef
      (PMappe.print ~first:"[@[" pp_print_string print_typ) env#vartyp
      env#bddindex env#bddincr
      (PMappe.print pp_print_int pp_print_string) env#idcondvar
      (PMappe.print pp_print_string print_tid) env#vartid
      (PDMappe.print
	env#print_cond
	(fun fmt (id,b) -> fprintf fmt "(%i,%b)" id b))
      env#cond
      (Cudd.Bdd.print_minterm pp_print_int) env#cond_supp
      (Cudd.Bdd.print_minterm pp_print_int) env#careset
end


(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type ['a] t = [typ, typdef, [`Unit], 'a] O.t
class ['a] make ?boolfirst ?relational man =
  [typ,typdef,[`Unit],'a] O.make
    man
    ?boolfirst ?relational
    ~compare_cond:(fun _ _ -> 0)
    ~negate_cond:(fun _ x -> x)
    ~support_cond:(fun _ x -> (PSette.empty String.compare))
    ~print_cond:(fun _ _ _ -> ())

let make = new make

(*  ====================================================================== *)
(** {3 Iterator on variables} *)
(*  ====================================================================== *)

let iter_ordered (env:('a,'b,'c,'d) #O.t) (f:(string -> int array -> unit))
  :
  unit
  =
  let cudd = env#cudd in
  let processed = ref (PSette.empty String.compare) in
  let size = Cudd.Man.get_bddvar_nb cudd in
  for level=0 to size-1 do
    let (id:int) = Cudd.Man.var_of_level cudd level in
    let (var:string) = PMappe.find id env#idcondvar in
    if not (PSette.mem var !processed) then begin
      let tid = PMappe.find var env#vartid in
      f var tid;
      processed := PSette.add var !processed;
    end;
  done;
  ()

(*  ====================================================================== *)
(** {3 Functional version of some methods} *)
(*  ====================================================================== *)

let print fmt env =
    O.print print_typ print_typdef fmt env

let add_typ env typ typdef =
  let nenv = Oo.copy env in
  nenv#add_typ typ typdef;
  nenv
let add_vars env lvartyp =
  let nenv = Oo.copy env in
  ignore (nenv#add_vars lvartyp);
  nenv
let remove_vars env lvars =
  let nenv = Oo.copy env in
  ignore (nenv#remove_vars lvars);
  nenv
let rename_vars env lvarvar =
  let nenv = Oo.copy env in
  ignore (nenv#rename_vars lvarvar);
  nenv


(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)

let permute_expr (expr:'d expr) (permutation:int array) : 'd expr
  =
  match expr with
  | `Bool(x) -> `Bool(Cudd.Bdd.permute x permutation)
  | `Bint(x) -> `Bint(Int.permute x permutation)
  | `Benum(x) -> `Benum(Enum.permute x permutation)

let check_normalized (env:('a,'b,'c,'d) #O.t) : bool
  =
  let index = ref 0 in
  let bddvar () =
    PMappe.iter
      (begin fun var tid ->
	Array.iter
	  (begin fun id ->
	    if id <> !index then begin
	      printf
		"Bddenv.check_normalized: not normalized at index %i@.env=%a@."
		!index
		print (env:>('a,'b,'c,'d) O.t)
	      ;
	      raise Exit
	    end;
	    index := !index + env#bddincr;
	  end)
	  tid
      end)
      env#vartid
  and bddcond () =
    PDMappe.iter
      (begin fun cond (id,b) ->
	if b then begin
	  if id <> !index then begin
	    printf
	      "Bddenv.check_normalized: not normalized at index %i@.env=%a@."
	      !index
	      print (env:>('a,'b,'c,'d) O.t)
	    ;
	    raise Exit
	  end;
	  index := !index + env#bddincr;
	end
      end)
      env#cond
  in
  try
    if env#boolfirst
    then (bddvar (); bddcond (); true)
    else (bddcond (); bddvar (); true)
  with Exit ->
    false

let compose_permutation (perm1:int array) (perm2:int array) : int array =
  let l1 = Array.length perm1 in
  let l2 = Array.length perm2 in
  let l = max l1 l2 in
  let perm = Array.init l (fun i -> i) in
  for i=0 to l1 - 1 do
    let j = perm1.(i) in
    if j<l2 then
      let k = perm2.(j) in
      perm.(i) <- k
  done;
  perm

let compose_opermutation (operm1:int array option) (operm2:int array option) 
    : 
    int array option 
    =
  match operm1 with
  | None -> operm2
  | Some perm1 ->
      match operm2 with
      | None -> operm1
      | Some perm2 ->
	  Some (compose_permutation perm1 perm2)

let is_leq (env1:('a,'b,'c,'d) #O.t) (env2:('a,'b,'c,'d) #O.t) : bool =
  env1==(Obj.magic env2) ||
  begin
    env1#cudd = env2#cudd &&
    env1#boolfirst == env2#boolfirst &&
    env1#bddindex <= env2#bddindex &&
    env1#bddincr == env2#bddincr &&
    PMappe.equal (=) env1#typdef env2#typdef &&
    (PMappe.subset (=) env1#vartyp env2#vartyp) &&
    (PMappe.subset (fun _ _ -> true)
      (PDMappe.mapx env1#cond)
      (PDMappe.mapx env2#cond))
  end

let is_eq (env1:('a,'b,'c,'d) #O.t) (env2:('a,'b,'c,'d) #O.t) : bool =
  env1==(Obj.magic env2) ||
  begin
    env1#cudd = env2#cudd &&
    env1#boolfirst == env2#boolfirst &&
    env1#bddindex == env2#bddindex &&
    env1#bddincr == env2#bddincr &&
    (env1#typdef==Obj.magic env2#typdef || (PMappe.equal (=) env1#typdef env2#typdef)) &&
    (env1#vartyp==Obj.magic env2#vartyp || (PMappe.equal (=) env1#vartyp env2#vartyp)) &&
    (env1#cond==Obj.magic env2#cond || (PDMappe.equaly env1#cond env2#cond))
  end

let lce (env1:(('a,'b,'c,'d) #O.t as 'e)) (env2:'e) : 'e =
  if is_leq env2 env1 then
    env1
  else if is_leq env1 env2 then
    env2
  else begin
    let typdef =
      PMappe.mergei
	(begin fun typ typdef1 typdef2 ->
	  if typdef1<>typdef2 then
	    failwith
	      (Format.sprintf
		"Bdd.Env.lce: two different definitions for (enumerated) type %s" typ)
	  ;
	  typdef1
	end)
	env1#typdef env2#typdef
    in
    let vartyp =
      PMappe.mergei
	(begin fun var typ1 typ2 ->
	  if typ1<>typ2 then
	    failwith
	      (Format.sprintf
		"Bdd.Env.lce: two different types for label/variable %s" var)
	  ;
	  typ1
	end)
	env1#vartyp env2#vartyp
    in
    let (labeltyp,vartyp) =
      PMappe.partition
	(begin fun varlabel typ ->
	  match typ with
	  | `Benum _ ->
	      not
	      ((PMappe.mem varlabel env1#vartid) ||
	      (PMappe.mem varlabel env2#vartid))
	  | _ -> false
	end)
	vartyp
    in
    let setcond =
      let add cond (id,b) res =
	if b then PSette.add cond res else res
      in
      let setcond1 =
	PDMappe.fold add env1#cond (PSette.empty env1#compare_cond)
      in
      let setcond12 =
	PDMappe.fold add env2#cond setcond1
      in
      setcond12
    in
    let env = Oo.copy env1 in
    env#set_typdef typdef;
    env#set_vartyp labeltyp;
    env#set_bddindex 0;
    env#set_vartid (PMappe.empty String.compare);
    env#set_varset (PMappe.empty String.compare);
    env#set_idcondvar (PMappe.empty (-));
    env#set_cond (PDMappe.empty env#compare_cond compare_idb);
    env#set_cond_supp (Cudd.Bdd.dtrue env#cudd);
    env#set_careset (Cudd.Bdd.dtrue env#cudd);
    let bddindex = ref 0 in
    if not env#boolfirst then
      env#set_bddindex ((PSette.cardinal setcond)*env#bddincr)
    ;
    PMappe.iter
      (begin fun var typ ->
	env#add_var var typ
      end)
      vartyp
    ;
    if not env#boolfirst then begin
      bddindex := env#bddindex;
      env#set_bddindex 0;
    end
    ;
    PSette.iter
      (begin fun cond ->
	ignore(env#idb_of_cond cond)
      end)
      setcond
    ;
    if not env#boolfirst then
      env#set_bddindex !bddindex
    ;
    env#compute_careset ~normalized:true;
    env
  end

let lce2 (env1:(('a,'b,'c,'d) #O.t as 'e)) (env2:('a,'b,'c,'d) #O.t) : 'e =
  if is_leq env2 env1 then
    env1
  else if is_leq env1 env2 then
    let nenv = Oo.copy env1 in
    nenv#set_vartyp env2#vartyp;
    nenv#set_bddindex env2#bddindex;
    nenv#set_bddincr env2#bddincr;
    nenv#set_idcondvar env2#idcondvar;
    nenv#set_vartid env2#vartid;
    nenv#set_varset env2#varset;
    nenv#set_cond env2#cond;
    nenv#set_cond_supp env2#cond_supp;
    nenv#set_careset env2#careset;
    nenv
  else begin
    let typdef =
      PMappe.mergei
	(begin fun typ typdef1 typdef2 ->
	  if typdef1<>typdef2 then
	    failwith
	      (Format.sprintf
		"Bdd.Env.lce: two different definitions for (enumerated) type %s" typ)
	  ;
	  typdef1
	end)
	env1#typdef env2#typdef
    in
    let vartyp =
      PMappe.mergei
	(begin fun var typ1 typ2 ->
	  if typ1<>typ2 then
	    failwith
	      (Format.sprintf
		"Bdd.Env.lce: two different types for label/variable %s" var)
	  ;
	  typ1
	end)
	env1#vartyp env2#vartyp
    in
    let (labeltyp,vartyp) =
      PMappe.partition
	(begin fun varlabel typ ->
	  match typ with
	  | `Benum _ ->
	      not
	      ((PMappe.mem varlabel env1#vartid) ||
	      (PMappe.mem varlabel env2#vartid))
	  | _ -> false
	end)
	vartyp
    in
    let setcond =
      let add cond (id,b) res =
	if b then PSette.add cond res else res
      in
      let setcond1 =
	PDMappe.fold add env1#cond (PSette.empty env1#compare_cond)
      in
      let setcond12 =
	PDMappe.fold add env2#cond setcond1
      in
      setcond12
    in
    let env = Oo.copy env1 in
    env#set_typdef typdef;
    env#set_vartyp labeltyp;
    env#set_bddindex 0;
    env#set_vartid (PMappe.empty String.compare);
    env#set_varset (PMappe.empty String.compare);
    env#set_idcondvar (PMappe.empty (-));
    env#set_cond (PDMappe.empty env#compare_cond compare_idb);
    env#set_cond_supp (Cudd.Bdd.dtrue env#cudd);
    env#set_careset (Cudd.Bdd.dtrue env#cudd);
    let bddindex = ref 0 in
    if not env#boolfirst then
      env#set_bddindex ((PSette.cardinal setcond)*env#bddincr)
    ;
    PMappe.iter
      (begin fun var typ ->
	env#add_var var typ
      end)
      vartyp
    ;
    if not env#boolfirst then begin
      bddindex := env#bddindex;
      env#set_bddindex 0;
    end
    ;
    PSette.iter
      (begin fun cond ->
	ignore(env#idb_of_cond cond)
      end)
      setcond
    ;
    if not env#boolfirst then
      env#set_bddindex !bddindex
    ;
    env#compute_careset ~normalized:true;
    env
  end

let permutation12 (env1:('a,'b,'c,'d) #O.t) (env2:('a,'b,'c,'d) #O.t) : int array
  =
  assert(
    if is_leq env1 env2 then true
    else begin
      printf "env1=%a@.envé=%a@."
	print env1
	print env2
      ;
      false
    end
  );
  let perm = Array.init env2#bddindex (fun i -> i) in
  let offset = ref 0 in
  let bddvar () =
    PMappe.iter
      (begin fun var2 tid2 ->
	try
	  let tid1 = PMappe.find var2 env1#vartid in
	  Array.iter
	    (fun id -> perm.(id) <- id + !offset)
	    tid1;
	with Not_found ->
	  offset := !offset + ((Array.length tid2)*env2#bddincr)
      end)
      env2#vartid
  in
  let bddcond () =
    PMappe.iter
      (begin fun cons2 (id2,b2) ->
	if b2 then begin
	  try
	    let (id1,b1) = PDMappe.y_of_x cons2 env1#cond in
	    assert b1;
	    perm.(id1) <- id1 + !offset
	  with Not_found ->
	    offset := !offset + env2#bddincr
	end
      end)
      (PDMappe.mapx env2#cond)
  in
  if env2#boolfirst then
    (bddvar(); bddcond())
  else
    (bddcond(); bddvar())
  ;
  perm

let permutation21 (env2:('a,'b,'c,'d) #O.t) (env1:('a,'b,'c,'d) #O.t) : int array
  =
  assert(is_leq env1 env2);
  if false then
    printf "permutation21:@.env2=%a@.env1=%a@."
	print env2
	print env1
  ;
  let perm = Array.init env2#bddindex (fun i -> i) in
  let offset = ref 0 in
  let bddvar () =
    PMappe.iter
      (begin fun var2 tid2 ->
	try
	  let tid1 = PMappe.find var2 env1#vartid in
	  Array.iter
	    (fun id -> perm.(id + !offset) <- id)
	    tid1;
	with Not_found ->
	  offset := !offset + (Array.length tid2)*env2#bddincr;
      end)
      env2#vartid
  in
  let bddcond () =
    PMappe.iter
      (begin fun cons2 (id2,b2) ->
	if b2 then begin
	  try
	    let (id1,b1) = PDMappe.y_of_x cons2 env1#cond in
	    assert b1;
	    perm.(id1 + !offset) <- id1
	  with Not_found ->
	    offset := !offset + env2#bddincr;
	end
      end)
      (PDMappe.mapx env2#cond)
  in
  if env2#boolfirst then
    (bddvar(); bddcond())
  else
    (bddcond(); bddvar())
  ;
  perm

type ('a,'b) value = {
  env : 'a;
  value : 'b
}

let make_value env value =
  assert(
    let env = Obj.magic env in
    if (PMappe.cardinal env#idcondvar) + (PDMappe.cardinal env#cond)/2 =
      (env#bddindex/env#bddincr)
    then
      check_normalized env
    else begin
      printf "Pb in Bddenv.make_value@.";
      printf "env=%a@."
	print env
      ;
      false
    end
  );
  { env=env; value=value }

let extend_environment
  (permute:'a -> int array -> 'a)
  value
  nenv
  =
  if is_eq value.env nenv then
    value
  else begin
    if not (is_leq value.env nenv) then
      failwith "Bddenv.extend_environment: the given environment is not a superenvironment "
    ;
    let perm = permutation12 value.env nenv in
    make_value nenv (permute value.value perm)
  end
