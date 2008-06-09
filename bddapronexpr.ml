(** Finite-type and arithmetical expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type typ = [
  | Bddexpr.typ
  | Apronexpr.typ
]
type typdef = Bddexpr.typdef
type cond = ApronexprDD.cond
type expr = [
  | Bddexpr.expr
  | `Apron of ApronexprDD.expr
]

let print_typ fmt (typ:[< typ]) =
  match typ with
  | #Bddexpr.typ as x -> Bddexpr.print_typ fmt x
  | #Apronexpr.typ as x -> Apronexpr.print_typ fmt x
let print_typdef fmt (x:[< typdef]) = Bddexpr.print_typdef fmt (x:>typdef)

let cofactor (e:expr) (bdd:Bdd.t) : expr
  =
  let (res:expr) =
    match e with
    | #Bddexpr.expr as x -> ((Bddexpr.cofactor x bdd):>expr)
    | `Apron x -> `Apron (ApronexprDD.cofactor x bdd)
  in
  res

let restrict (e:expr) (bdd:Bdd.t) : expr
  =
  match e with
  | #Bddexpr.expr as x -> ((Bddexpr.restrict x bdd):>expr)
  | `Apron x -> `Apron (ApronexprDD.restrict x bdd)

let tdrestrict (e:expr) (bdd:Bdd.t) : expr
  =
  match e with
  | #Bddexpr.expr as x -> ((Bddexpr.tdrestrict x bdd):>expr)
  | `Apron x -> `Apron (ApronexprDD.tdrestrict x bdd)

let permute (e:expr) (tab:int array) : expr
  =
  match e with
  | #Bddexpr.expr as x -> ((Bddexpr.O.permute x tab):>expr)
  | `Apron x -> `Apron (ApronexprDD.permute x tab)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

let print_cond env fmt (cond:[< cond]) =
  match cond with
  | `Apron x -> Apronexpr.Condition.print fmt x

let compare_cond c1 c2 = match (c1,c2) with
  (`Apron c1, `Apron c2) -> Apronexpr.Condition.compare c1 c2

let negate_cond env (c:cond) : cond = match c with
  | `Apron x -> `Apron (Apronexpr.Condition.negate env x)

let cond_support cond = match cond with
  | `Apron x -> Apronexpr.Condition.support x

let typ_of_expr (expr:[<expr]) =
  match expr with
  | `Bool _ -> `Bool
  | `Bint(x) -> `Bint(x.Bddint.signed, Array.length x.Bddint.reg)
  | `Benum(x) -> `Benum(x.Bddenum.typ)
  | `Apron _ -> `Real

class type ['a,'b,'c] env = object
  inherit [[> typ] as 'a,[> typdef] as 'b,[> ] as 'c] Bddenv.t
  val v_apronexprdd : Apronexpr.expr Mtbdd2.manager
  method apronexprdd : Apronexpr.expr Mtbdd2.manager
  val mutable v_apron_env : Apron.Environment.t
  method apron_env : Apron.Environment.t
  method set_apron_env : Apron.Environment.t -> unit
end

let print_env fmt (env:('a,'b,'c) env) =
  Format.fprintf fmt "{@[<v>%a;@ apron_env = %a@]}"
    (Bddenv.print print_typ print_typdef) (env:>('a,'b,'c) Bddenv.t)
    (fun fmt x -> Apron.Environment.print fmt x) env#apron_env

class ['a,'b] make_env ?(boolfirst=true) man : ['a,'b,cond] env = object(self)
  inherit ['a,'b,cond] Bddenv.make ~boolfirst man ~compare_cond ~negate_cond ~print_cond
  val v_apronexprdd = ApronexprDD.make_manager()
  method apronexprdd = v_apronexprdd
  val mutable v_apron_env = Apron.Environment.make [||] [||]
  method apron_env = v_apron_env
  method set_apron_env apron_env = v_apron_env <- apron_env
end

let make_env ?(boolfirst=true) man = new make_env ~boolfirst man

module Internal = struct
  let add_vars (env:('a,'b,'c) #env) lvartyp =
    let (bool,integer,real) =
      List.fold_left
	(begin fun ((bool,integer,real) as acc) (var,typ) ->
	  match typ with
	  | `Int -> (bool,(Apron.Var.of_string var)::integer,real)
	  | `Real -> (bool,integer,(Apron.Var.of_string var)::real)
	  | #Bddexpr.typ ->
	      (true,integer,real)
	  | _ ->
	      acc
	end)
	(false,[],[]) lvartyp
    in
    let (env,perm) = Bddenv.add_vars env lvartyp in
    if integer<>[] || real<>[] then begin
      env#set_apron_env
	(Apron.Environment.add env#apron_env
	  (Array.of_list integer) (Array.of_list real))
    end;
    (env,perm)

  let remove_vars (env:('a,'b,'c) #env) lvar =
    let (nenv,perm) = Bddenv.remove_vars env lvar in
    let arith =
      List.fold_left
	(begin fun acc var ->
	  match env#typ_of_var var with
	  | `Int
	  | `Real -> (Apron.Var.of_string var)::acc
	  | _ -> acc
	end)
	[] lvar
    in
    if arith<>[] then begin
      nenv#set_apron_env
	(Apron.Environment.remove nenv#apron_env
	  (Array.of_list arith))
    end;
    (nenv,perm)

end

let add_typ = Bddenv.add_typ
let add_vars env x = fst (Internal.add_vars env x)
let remove_vars env x = fst (Internal.remove_vars env x)

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

let check_typ2 env e2 e3 =
  let t2 = typ_of_expr e2 in
  let t3 = typ_of_expr e3 in
  if t2 != t3 then
    failwith
      (Print.sprintf "Expr.ite: 2 branches have different types %a and %a"
	print_typ t2 print_typ t3)
  else
    t2

let compose_of_substitution (env:('a,'b,cond) #env) (substitution:(string*expr) list)
  :
  Bdd.t array option * expr MappeS.t
  =
  let change = ref false in
  (* we first look at Boolean variables/conditions *)
  let (bsub,osub) =
    List.fold_left
      (begin fun (bsub,osub) (var,expr) ->
	begin match expr with
	| #Bddexpr.expr as x ->
	    ((var,x)::bsub,osub)
	| _ ->
	    (bsub, MappeS.add var expr osub)
	end
      end)
      ([],MappeS.empty)
      substitution
  in
  if bsub<>[] then change := true;
  (* we initialize the substitution table *)
  let tab = Bddexpr.O.composition_of_substitution env bsub in
  if osub<>MappeS.empty then begin
    (* we now take care of other conditions *)
    for id=0 to pred(Array.length tab) do
      begin try
	let cond = env#cond_of_idb (id,true) in
	let supp = cond_support cond in
	let substitution = MappeS.interset osub supp in
	if substitution<>MappeS.empty then begin
	  change := true;
	  let bdd = match cond with
	    | `Apron x ->
		ApronexprDD.Condition.substitute env#manager env#apronexprdd env x substitution
	  in
	  tab.(id) <- bdd
	end
      with Not_found ->
	()
      end
    done;
  end;
  ((if !change then Some tab else None), osub)

let substitute (env:('a,'b,cond) #env) (e:expr) (substitution:(string*expr) list) : expr
  =
  if substitution = [] then
    e
  else begin
    let (tab,sub) = compose_of_substitution env substitution in
    if sub=MappeS.empty then begin
      let tab = match tab with
	| None -> assert false
	| Some(tab) -> tab
      in
      begin match e with
      | #Bddexpr.expr as x ->
	  ((Bddexpr.O.compose x tab):>expr)
      | `Apron mtbdd ->
	  `Apron (ApronexprDD.vectorcompose tab mtbdd)
      end
    end
    else begin
      begin match e with
      | #Bddexpr.expr as x ->
	  begin match tab with
	  | None -> e
	  | Some(tab) -> ((Bddexpr.O.compose x tab):>expr)
	  end
      | `Apron expr ->
	  let cudd = env#manager in
	  let apronexprdd = env#apronexprdd in
	  let nexpr =
	    Array.fold_left
	      (begin fun res (guard,leaf) ->
		let nguard = match tab with
		  | None -> guard
		  | Some(tab) -> Bdd.vectorcompose tab guard
		in
		let nexpr = ApronexprDD.substitute cudd apronexprdd leaf sub in
		ApronexprDD.ite nguard nexpr res
	      end)
	      (ApronexprDD.background cudd apronexprdd)
	      (ApronexprDD.guardleafs expr)
	  in
	  `Apron nexpr
      end
    end
  end

let var env (var:string) : expr =
  let typ = env#typ_of_var var in
  match typ with
  | #Bddexpr.typ ->
      ((Bddexpr.O.var env var):>expr)
  | `Int | `Real ->
      `Apron (ApronexprDD.var env#manager env#apronexprdd env var)
  | _ -> raise Not_found

let substitute_by_var (env:('a,'b,cond) #env) e (substitution:(string*string) list) =
  substitute env e (List.map (fun (v1,v2) -> (v1,var env v2)) substitution)

let ddsubstitute = substitute
let ddsubstitute_by_var = substitute_by_var

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool = struct
  include Bddexpr.O.Bool
  let substitute_by_var (env:('a,'b,cond) #env) (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute (env:('a,'b,cond) #env) (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint = struct
  include Bddexpr.O.Bint
  let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 Enumerated type expressions} *)
(*  ====================================================================== *)

module Benum = struct
  include Bddexpr.O.Benum
  let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 Apronexprmetic expressions} *)
(*  ====================================================================== *)

module Apron = struct
  type t = ApronexprDD.expr

  let of_expr = ApronexprDD.of_expr
  let to_expr = ApronexprDD.to_expr

  let print env fmt x = ApronexprDD.print (Bool.print env) fmt x

  let cst env c = ApronexprDD.cst env#manager env#apronexprdd c
  let var env name = ApronexprDD.var env#manager env#apronexprdd env name
  let add env = ApronexprDD.add
  let sub env = ApronexprDD.sub
  let mul env = ApronexprDD.mul
  let div env = ApronexprDD.div
  let gmod env = ApronexprDD.gmod
  let negate env = ApronexprDD.negate
  let cast env = ApronexprDD.cast
  let sqrt env = ApronexprDD.sqrt

  let supeq (env:('a,'b,'c) #env) = ApronexprDD.Condition.supeq env
  let sup (env:('a,'b,'c) #env) = ApronexprDD.Condition.sup env
  let eq (env:('a,'b,'c) #env) = ApronexprDD.Condition.eq env

  let ite (env:('a,'b,'c) #env) b (t1:t) (t2:t) : t = ApronexprDD.ite b t1 t2

  let cofactor = ApronexprDD.cofactor
  let restrict = ApronexprDD.restrict
  let tdrestrict = ApronexprDD.tdrestrict
  let permute = ApronexprDD.permute
  let support = ApronexprDD.support
  let support_leaf = ApronexprDD.support_leaf

  let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

let eq (env:('a,'b,'c) #env) (e1:expr) (e2:expr) : Bool.t
  =
  let t = check_typ2 env e1 e2 in
  match t with
  | `Bool ->
      Bool.eq env (Bool.of_expr e1) (Bool.of_expr e2)
  | `Bint _ ->
      Bint.eq env (Bint.of_expr e1) (Bint.of_expr e2)
  | `Benum _ ->
      Benum.eq env (Benum.of_expr e1) (Benum.of_expr e2)
  | `Real ->
      let diff = Apron.sub env
	(Apron.of_expr e1)
	(Apron.of_expr e2)
      in
      Apron.eq env diff
  | _ -> failwith ""

let ite env (cond:Bool.t) (e1:expr) (e2:expr) : expr
  =
  let t = check_typ2 env e1 e2 in
  match t with
  | `Bool ->
      `Bool (Bool.ite env cond (Bool.of_expr e1) (Bool.of_expr e2))
  | `Bint _ ->
      `Bint (Bint.ite env cond (Bint.of_expr e1) (Bint.of_expr e2))
  | `Benum _ ->
      `Benum (Benum.ite env cond (Benum.of_expr e1) (Benum.of_expr e2))
  | `Real ->
      Apron.to_expr (Apron.ite env cond (Apron.of_expr e1) (Apron.of_expr e2))
  | _ -> failwith ""

let support_cond env (expr:expr) : Bdd.t =
  match expr with
  | #Bddexpr.expr as x ->
      Bddexpr.O.support_cond env (x:> Bddexpr.expr)
  | `Apron x ->
      Apron.support x

(** Concatenates in pair of BDD and IDD arrays the BDDs/IDDs involved in the
  expressions *)
let tbddidd_of_texpr (texpr:expr array) : Bdd.t array * Idd.t array
  =
  let (nbdd,nidd) =
    Array.fold_left
      (begin fun (nb,ni) expr ->
	match expr with
	| `Bool(x) ->  (nb+1,ni)
	| `Bint(x) -> (nb + (Array.length x.Bddint.reg), ni)
	| `Benum(x) -> (nb + (Array.length x.Bddenum.reg), ni)
	| `Apron(x) -> (nb,ni+1)
      end)
      (0,0)
      texpr
  in
  let tb = Array.make nbdd Bdd.dummy in
  let ti = Array.make nidd Idd.dummy in
  let (b,i) =
    Array.fold_left
      (begin fun (b,i) expr ->
	begin match expr with
	| `Bool(x) ->
	    tb.(b) <- x;
	    (b+1,i)
	| `Bint(x) ->
	    let reg = x.Bddint.reg in
	    let length = Array.length reg in
	    Array.blit reg 0 tb b length;
	    (b+length,i)
	| `Benum(x) ->
	    let reg = x.Bddenum.reg in
	    let length = Array.length reg in
	    Array.blit reg 0 tb b length;
	    (b+length,i)
	| `Apron x ->
	    ti.(i) <- x.Mtbdd2.idd;
	    (b,i+1)
	end
      end)
      (0,0) texpr
  in
  assert(b=nbdd && i=nidd);
  (tb,ti)

(** Inverse operation: rebuild an array of expressions from the old array of
  expressions (for the types) and the arrays of BDDs/IDDs.
*)
let texpr_of_tbddidd
  (oldtexpr:expr array)
  (tbdd:Bdd.t array) (tidd:Idd.t array)
  :
  expr array
  =
  let indexb = ref 0 in
  let indexi = ref 0 in
  Array.map
    (begin fun oldexpr ->
      match oldexpr with
      | `Bool x ->
	  let res = tbdd.(!indexb) in
	  incr indexb;
	  `Bool res
      | `Bint x ->
	  let res =
	    { x with Bddint.reg =
	      Array.mapi (fun i bdd -> tbdd.(!indexb + i)) x.Bddint.reg
	    }
	  in
	  indexb := !indexb + (Array.length x.Bddint.reg);
	  `Bint res
      | `Benum x ->
	  let res =
	    { x with Bddenum.reg =
	      Array.mapi (fun i bdd -> tbdd.(!indexb + i)) x.Bddenum.reg
	    }
	  in
	  indexb := !indexb + (Array.length x.Bddenum.reg);
	  `Benum res
      | `Apron x ->
	  let idd = tidd.(!indexi) in
	  let res = ApronexprDD.of_idd x.Mtbdd2.man idd in
	  incr indexi;
	  `Apron res
    end)
    oldtexpr

let vectorsupport_cond (env:('a,'b,'c) #env) (texpr:expr array) : Bdd.t =
  let (tb,ti) = tbddidd_of_texpr texpr in
  if tb=[||] && ti=[||] then
    Bdd.dtrue env#manager
  else
    Idd.vectorsupport2 tb ti

let support env (expr:expr) : SetteS.t
  =
  let supp = support_cond env expr in
  let list = Bdd.list_of_support supp in
  let set = ref SetteS.empty in
  List.iter
    (begin fun id ->
      try
	let cond = env#cond_of_idb (id,true) in
	let supp = cond_support cond in
	set := SetteS.union supp !set
      with Not_found ->
	let var = MappeI.find id env#idcondvar in
	set := SetteS.add var !set
    end)
    list
  ;
  begin match expr with
  | #Bddexpr.expr -> ()
  | `Apron expr ->
      set := SetteS.union (Apron.support_leaf expr) !set
  end;
  !set

let print_expr (env:('a,'b,'c) #env) (fmt:Format.formatter) (expr:[<expr])
  =
  match expr with
  | #Bddexpr.expr as x -> Bddexpr.O.print_expr env fmt x
  | `Apron x -> Apron.print env fmt x

let print_bdd = Bool.print

(*
let cleanup (env:('a,'b,'c) #env) (setid:SetteI.t) (lexpr:expr list) : unit
  =
  let supp = vectorsupport_cond env lexpr in
  let supp = Bdd.list_of_support supp in
  let suppid =
    List.fold_left
      (begin fun res id -> SetteI.add id res end)
      setid
      supp
  in
  env#set_cond
    (DMappe.Custom.fold
      (begin fun cond (id,b) res ->
	if not (SetteI.mem id suppid) then
	  DMappe.Custom.remove cond res
	else
	  res
      end)
      env#cond env#cond)
  ;
  Bddenv.compute_careset env;
  let setleafarith = ref SetteI.empty in
  List.iter
    (begin fun expr -> match expr with
    | #Bddexpr.expr -> ()
    | `Apron (Bddcond.Leaf _) -> ()
    | `Apron (Bddcond.Mtbdd x) ->
	let idd = ApronexprDD.Add.to_idd x in
	let leaves = Idd.leaves idd in
	setleafarith :=
	Array.fold_left
	  (fun res id -> SetteI.add id res)
	  !setleafarith leaves
	;
    end)
    lexpr
  ;
  ApronexprDD.Add.iter_leaf
    (begin fun id leaf ->
      if not (SetteI.mem id !setleafarith) then
	ApronexprDD.Add.remove_leaf leaf
    end)
  ;
  ()
*)
end

(*  ********************************************************************** *)
(** Closed signature *)
(*  ********************************************************************** *)

class type env = [typ,typdef,cond] O.env
class make_env = [typ,typdef] O.make_env
let make_env = O.make_env

let add_typ = O.add_typ
let add_vars = O.add_vars
let remove_vars = O.remove_vars
let typ_of_expr= O.typ_of_expr
let var = O.var
let ite = O.ite
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let support = O.support
let eq = O.eq
let support_cond = O.support_cond
let vectorsupport_cond = O.vectorsupport_cond
let print_cond = O.print_cond
let print_expr = O.print_expr
let print_env = O.print_env

module Bool = struct
  include Bddexpr.O.Bool
  let substitute_by_var = O.Bool.substitute_by_var
  let substitute = O.Bool.substitute
end
module Bint = struct
  include Bddexpr.O.Bint
  let substitute_by_var = O.Bint.substitute_by_var
  let substitute = O.Bint.substitute
end
module Benum = struct
  include Bddexpr.O.Benum
  let substitute_by_var = O.Benum.substitute_by_var
  let substitute = O.Benum.substitute
end
module Apron = struct
  type t = ApronexprDD.expr
  let of_expr = O.Apron.of_expr
  let to_expr = O.Apron.to_expr
  let cst = O.Apron.cst
  let var = O.Apron.var
  let add = O.Apron.add
  let sub = O.Apron.sub
  let mul = O.Apron.mul
  let div = O.Apron.div
  let gmod = O.Apron.gmod
  let negate = O.Apron.negate
  let cast = O.Apron.cast
  let sqrt = O.Apron.sqrt
  let supeq = O.Apron.supeq
  let sup = O.Apron.sup
  let eq = O.Apron.eq
  let ite = O.Apron.ite
  let cofactor = O.Apron.cofactor
  let restrict = O.Apron.restrict
  let tdrestrict = O.Apron.tdrestrict
  let permute = O.Apron.permute
  let substitute_by_var = O.Apron.substitute_by_var
  let substitute = O.Apron.substitute
  let print = O.Apron.print
end
