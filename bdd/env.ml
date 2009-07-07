(** Normalized managers/environments *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Datatypes } *)
(*  ********************************************************************** *)

exception Bddindex

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
  class type ['a,'b,'c] t = object('d)
    constraint 'a = [> typ]
    constraint 'b = [> typdef]

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
    (** Remove the set of variables, as well as all constraints,
	and possibly normalize the environment and return the
	applied permutation. *)
    method rename_vars : (string * string) list -> int array option
    (** Rename the variables, and remove all constraints,possibly
	normalize the environment and return the applied
	permutation. *)
    method mem_var : string -> bool
    (** Is the label/var defined in the database ? *)
    method mem_label : string -> bool
    (** Is the label a label defined in the database ? *)
    method typ_of_var : string -> 'a
    (** Return the type of the label/variable *)
    method vars : string PSette.t
    (** Return the list of variables (not labels) *)
    method labels : string PSette.t
    (** Return the list of labels (not variables) *)

    method permutation : int array
    (** Compute the permutation for normalizing the environment *)
    method permute : int array -> unit
    (** Apply the given permutation to the environment *)
    method normalize : int array
    (** Combine the two previous functions, and return the permutation *)

(*
    method pair : unit
    (** Assuming [bddindex] is even and [bddincr=2]), if [v_paired=false],
	groups variable by pair *)
*)
    method add_var : string -> 'a -> unit
    (** Private function *)

    method print_idcondb : Format.formatter -> int*bool -> unit
    method print_order : Format.formatter -> unit
    (** Print the BDD variable ordering *)

    val v_cudd : 'c Cudd.Man.t
    (** CUDD manager *)
    val mutable v_paired : bool
    (** Are pair groups active (default false) ? *)
    val mutable v_typdef : (string, 'b) PMappe.t
    (** Named types definitions *)
    val mutable v_vartyp : (string, 'a) PMappe.t
    (** Associate to a var/label its type *)
    val mutable v_bddindex0 : int
    (** First index for finite-type variables *)
    val mutable v_bddsize : int
    (** Number of indices dedicated to finite-type variables *)
    val mutable v_bddindex : int
    (** Next free index in BDDs used by [self#add_var]. *)
    val v_bddincr : int
    (** Increment used by [self#add_var] for incrementing
	[self#_v_bddindex] *)
    val mutable v_idcondvar : (int, string) PMappe.t
    (** Associates to a BDD index the variable involved by it *)
    val mutable v_vartid : (string, int array) PMappe.t
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
    val mutable v_varset : (string, 'c Cudd.Bdd.t) PMappe.t
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
    val mutable v_print_external_idcondb : Format.formatter -> int*bool -> unit
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)

    method cudd : 'c Cudd.Man.t
    method typdef : (string, 'b) PMappe.t
    method vartyp : (string, 'a) PMappe.t
    method bddindex0 : int
    method bddsize : int
    method bddindex : int
    method bddincr : int
    method idcondvar : (int, string) PMappe.t
    method vartid : (string, int array) PMappe.t
    method varset : (string, 'c Cudd.Bdd.t) PMappe.t
    method print_external_idcondb : Format.formatter -> int*bool -> unit
    method set_bddindex0 : int -> unit
    method set_bddsize : int -> unit
    method set_bddindex : int -> unit
    method set_idcondvar : (int, string) PMappe.t -> unit
    method set_vartid : (string, int array) PMappe.t -> unit
    method set_varset : (string, 'c Cudd.Bdd.t) PMappe.t -> unit
    method set_typdef : (string, 'b) PMappe.t -> unit
    method set_vartyp : (string, 'a) PMappe.t -> unit
    method set_print_external_idcondb :
      (Format.formatter -> int*bool -> unit) -> unit
  end

  (*  ============================================================ *)
  (** {3 Creating the class [env]} *)
  (*  ============================================================ *)

  class ['a,'b,'c] make
    ?(bddindex0=0)
    ?(bddsize=100)
    ?(relational=false)
    (man:'c Cudd.Man.t)
    :
    ['a,'b,'c] t
    =
  object(self:'d)

    val v_cudd = man
    val mutable v_paired = false
    val mutable v_typdef = PMappe.empty String.compare
    val mutable v_vartyp = PMappe.empty String.compare
    val mutable v_bddindex0 = bddindex0
    val mutable v_bddsize = bddsize
    val mutable v_bddindex = bddindex0
    val v_bddincr = if relational then 2 else 1
    val mutable v_idcondvar = PMappe.empty (-)
    val mutable v_vartid = PMappe.empty String.compare
    val mutable v_varset = PMappe.empty String.compare
    val mutable v_print_external_idcondb =
      begin fun fmt (id,b) ->
	fprintf fmt "%s%i" (if b then "not " else "") id
      end

    method cudd = v_cudd
    method typdef = v_typdef
    method vartyp = v_vartyp
    method bddindex0 = v_bddindex0
    method bddsize = v_bddsize
    method bddindex = v_bddindex
    method bddincr = v_bddincr
    method idcondvar = v_idcondvar
    method vartid = v_vartid
    method varset = v_varset
    method print_external_idcondb = v_print_external_idcondb
    method set_typdef x = v_typdef <- x
    method set_vartyp x = v_vartyp <- x
    method set_bddindex0 x = v_bddindex0 <- x
    method set_bddsize x = v_bddsize <- x
    method set_bddindex x = v_bddindex <- x
    method set_idcondvar x = v_idcondvar <- x
    method set_vartid x = v_vartid <- x
    method set_varset x = v_varset <- x
    method set_print_external_idcondb x = v_print_external_idcondb <- x

    method permutation : int array
      =
      let perm = Array.init (Cudd.Man.get_bddvar_nb v_cudd) (fun i -> i) in
      let index = ref v_bddindex0 in
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

    method mem_label (label:string) : bool =
      let typ = PMappe.find label v_vartyp in
      match typ with
      | `Benum _ when not (PMappe.mem label v_vartid) -> true
      | _ -> false

    method typ_of_var (label:string) : 'a
      =
      try
	PMappe.find label v_vartyp
      with Not_found ->
	failwith ("Bdd.Env.t#typ_of_var: unknwon label/variable "^label)

    method vars =
      PMappe.maptoset v_vartid

    method labels =
      PMappe.fold
	(begin fun typ def res ->
	  match def with
	  | `Benum tlabel ->
	      Array.fold_right PSette.add tlabel res
	  | _ -> res
	end)
	v_typdef
	(PSette.empty String.compare)

    method add_var var typ : unit
      =
      if PMappe.mem var v_vartyp then
	failwith (sprintf "Bdd.Env.env#add_var: label/var %s already defined" var)
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
      end;
      if v_bddindex >= v_bddindex0+v_bddsize then raise Bddindex;
      ()

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
		    "Bdd.Env.remove: trying to remove the label %s of an enumerated type"
		    var)
	      end
	  | _ -> ()
	  end;
	  v_vartyp <- PMappe.remove var v_vartyp;
	end)
	lvar
      ;
      if !length = 0 then
	None
      else begin
	let perm = self#normalize in
	v_bddindex <- (v_bddindex - !length);
	Some perm
      end

    method rename_vars (lvarvar:(string*string) list)
      :
      int array option
      =
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
		"Bdd.Env.rename_vars: error, variable %s renamed in already existing %s"
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
		"Bdd.Env.rename_vars: error, variable %s renamed in already existing %s"
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
      let perm = self#normalize in
      Some perm
(*
  method pair : unit =
  assert(v_bddindex mod 2 = 0 && v_bddincr=2);
  if not v_paired then begin
  let nb = Cudd.Man.get_bddvar_nb v_cudd in
  for i=0 to max (nb/2) ((nb-1)/2) do
  Cudd.Man.group v_cudd (i*2) 2 Cudd.Man.MTR_FIXED
  done;
  v_paired <- true;
  end
*)
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
	v_print_external_idcondb fmt idb

    method print_order (fmt:Format.formatter) : unit
      =
      let cudd = v_cudd in
      let nb = Cudd.Man.get_bddvar_nb v_cudd in
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

  let print print_typ print_typdef fmt (env:('a,'b,'c) #t) =
    fprintf fmt
      "{@[<v>typdef = %a;@ vartyp = %a;@ bddindex0 = %i;@ bddindex = %i; bddincr = %i;@ idcondvar = %a;@ vartid = %a;@]}"
      (PMappe.print pp_print_string print_typdef) env#typdef
      (PMappe.print ~first:"[@[" pp_print_string print_typ) env#vartyp
      env#bddindex0 env#bddindex env#bddincr
      (PMappe.print pp_print_int pp_print_string) env#idcondvar
      (PMappe.print pp_print_string print_tid) env#vartid

end


(*  ********************************************************************** *)
(** {2 Closed signature} *)
(*  ********************************************************************** *)

class type ['a] t = [typ, typdef, 'a] O.t
class ['a] make ?bddindex0 ?bddsize ?relational man =
  [typ,typdef,'a] O.make
    man
    ?bddindex0 ?bddsize ?relational

let make = new make

(*  ====================================================================== *)
(** {3 Iterator on variables} *)
(*  ====================================================================== *)

let iter_ordered (env:('a,'b,'c) #O.t) (f:(string -> int array -> unit))
  :
  unit
  =
  let cudd = env#cudd in
  let processed = ref (PSette.empty String.compare) in
  let size = Cudd.Man.get_bddvar_nb env#cudd in
  for level=0 to size-1 do
    let (id:int) = Cudd.Man.var_of_level cudd level in
    try
      let (var:string) = PMappe.find id env#idcondvar in
      if not (PSette.mem var !processed) then begin
	let tid = PMappe.find var env#vartid in
	f var tid;
	processed := PSette.add var !processed;
      end
    with Not_found ->
      ()
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

let check_normalized (env:('a,'b,'c) #O.t) : bool
    =
  try
    let index = ref env#bddindex0 in
    PMappe.iter
      (begin fun var tid ->
	Array.iter
	  (begin fun id ->
	    if id <> !index then begin
	      printf
		"Bdd.Env.check_normalized: not normalized at index %i@.env=%a@."
		!index
		print (env:>('a,'b,'c) O.t)
	      ;
	      raise Exit
	    end;
	    index := !index + env#bddincr;
	  end)
	  tid
      end)
      env#vartid
    ;
    true
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

let permutation_of_offset (length:int) (offset:int) : int array =
  let perm = Array.create length 0 in
  for i=0 to pred length do
    perm.(i) <- i+offset
  done;
  perm

(* 2 cas pour lesquels not (e1 = e2):

   a) l'ens des variables n'est pas égal
   ou
   b) bddincr ou bddindex0 est différent

   Peut-être faut-il distinguer c'est deux cas ?

   Idem pour l'égalité
*)

let is_leq (env1:('a,'b,'c) #O.t) (env2:('a,'b,'c) #O.t) : bool =
  env1==(Obj.magic env2) ||
    env1#cudd = env2#cudd &&
      (env1#bddindex - env1#bddindex0 <= env2#bddindex - env2#bddindex0) &&
      (env1#typdef==Obj.magic env2#typdef || PMappe.subset (=) env1#typdef env2#typdef) &&
      (env1#vartyp==Obj.magic env2#vartyp || PMappe.subset (=) env1#vartyp env2#vartyp)

let is_eq (env1:('a,'b,'c) #O.t) (env2:('a,'b,'c) #O.t) : bool =
  env1==(Obj.magic env2) ||
    env1#cudd = env2#cudd &&
      env1#bddincr = env2#bddincr &&
      (env1#bddindex - env1#bddindex0 = env2#bddindex - env2#bddindex0) &&
      (env1#typdef==Obj.magic env2#typdef || PMappe.equal (=) env1#typdef env2#typdef) &&
      (env1#vartyp==Obj.magic env2#vartyp || (PMappe.equal (=) env1#vartyp env2#vartyp))

let shift (env:(('a,'b,'c) #O.t as 'd)) (offset:int) : 'd =
  let perm = permutation_of_offset env#bddindex offset in
  let nenv = Oo.copy env in
  nenv#set_bddindex0 (nenv#bddindex0 + offset);
  nenv#permute perm;
  nenv

let lce (env1:(('a,'b,'c) #O.t as 'd)) (env2:'d) : 'd =
  if is_leq env2 env1 then
    let offset = env1#bddindex0 - env2#bddindex0 in
    if offset>=0 then
      env1
    else
      shift env1 (-offset)
  else if is_leq env1 env2 then
    let offset = env2#bddindex0 - env1#bddindex0 in
    if offset>=0 then
      env2
    else
      shift env2 (-offset)
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
    let env = Oo.copy env1 in
    env#set_typdef typdef;
    env#set_vartyp labeltyp;
    env#set_bddindex0 (Pervasives.max env1#bddindex0 env2#bddindex0);
    env#set_bddsize (Pervasives.max env1#bddsize env2#bddsize);
    env#set_bddindex env#bddindex0;
    env#set_vartid (PMappe.empty String.compare);
    env#set_varset (PMappe.empty String.compare);
    env#set_idcondvar (PMappe.empty (-));
    PMappe.iter
      (begin fun var typ ->
	env#add_var var typ
      end)
      vartyp
    ;
    env
    end

let permutation12 (env1:('a,'b,'c) #O.t) (env2:('a,'b,'c) #O.t) : int array
    =
  assert(
    if is_leq env1 env2 then true
    else begin
      printf "env1=%a@.env2=%a@."
	print env1
	print env2
      ;
      false
    end
  );
  let perm = Array.init (Cudd.Man.get_bddvar_nb env1#cudd) (fun i -> i) in
  let offset = ref (env2#bddindex0 - env1#bddindex0) in
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
  ;
  perm

let permutation21 (env2:('a,'b,'c) #O.t) (env1:('a,'b,'c) #O.t) : int array
    =
  assert(
    if is_leq env1 env2 then true
    else begin
      printf "env1=%a@.env2=%a@."
	print env1
	print env2
      ;
      false
    end
  );
  let perm = Array.init (Cudd.Man.get_bddvar_nb env2#cudd) (fun i -> i) in
  let offset = ref (env2#bddindex0 - env1#bddindex0) in
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
  ;
  perm

(*  ********************************************************************** *)
(** {2 Precomputing change of environments} *)
(*  ********************************************************************** *)

type 'a change = {
  intro : int array option;
  remove : ('a Cudd.Bdd.t * int array) option;
}

let compute_change env1 env2 =
  let lce = lce env1 env2 in
  let intro =
    if is_eq env1 lce
    then None
    else Some (permutation12 env1 lce)
  in
  let remove =
    if is_eq env2 lce
    then None
    else
      let mapvarid =
	PMappe.diffset
	  lce#vartid
	  (PMappe.maptoset env2#vartid)
      in
      let cudd = env1#cudd in
      let supp = ref (Cudd.Bdd.dtrue cudd) in
      PMappe.iter
	(begin fun var tid ->
	  Array.iter
	    (fun id -> supp := Cudd.Bdd.dand !supp (Cudd.Bdd.ithvar cudd id))
	    tid
	end)
	mapvarid
      ;
      Some(!supp, permutation21 lce env2)
  in
  { intro = intro; remove = remove }

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

type ('a,'b) value = {
  env : 'a;
  val0 : 'b
}

let make_value env val0 =
  assert(
    let env = Obj.magic env in
    if (PMappe.cardinal env#idcondvar) =
      ((env#bddindex - env#bddindex0)/env#bddincr)
    then
      check_normalized env
    else begin
      printf "Pb in Bdd.Env.make_value@.";
      printf "env=%a@."
	print env
      ;
      false
    end
  );
  { env=env; val0=val0 }

let extend_environment
  (permute:'a -> int array -> 'a)
  value
  nenv
  =
  if is_eq value.env nenv then
    let offset = nenv#bddindex0 - value.env#bddindex0 in
    if offset=0 then
      value
    else
      let perm = permutation_of_offset value.env#bddindex offset in
      make_value nenv (permute value.val0 perm)
  else if is_leq value.env nenv then
    let perm = permutation12 value.env nenv in
    make_value nenv (permute value.val0 perm)
  else
    failwith "Bdd.Env.extend_environment: the given environment is not a superenvironment "
