(** Finite-type and arithmetical expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type t = [
  | Cudd.Man.v Bdd.Expr0.t
  | `Apron of ApronexprDD.t
]
type expr = t

let make_env = Env.make

let cofactor (e:t) (bdd:Cudd.Man.v Cudd.Bdd.t) : t
  =
  let (res:t) =
    match e with
    | #Bdd.Expr0.t as x -> ((Bdd.Expr0.cofactor x bdd):>expr)
    | `Apron x -> `Apron (Cudd.Mtbdd.cofactor x bdd)
  in
  res

let restrict (e:t) (bdd:Cudd.Man.v Cudd.Bdd.t) : t
  =
  match e with
  | #Bdd.Expr0.t as x -> ((Bdd.Expr0.restrict x bdd):>expr)
  | `Apron x -> `Apron (Cudd.Mtbdd.restrict x bdd)

let tdrestrict (e:t) (bdd:Cudd.Man.v Cudd.Bdd.t) : t
  =
  match e with
  | #Bdd.Expr0.t as x -> ((Bdd.Expr0.tdrestrict x bdd):>expr)
  | `Apron x -> `Apron (Cudd.Mtbdd.tdrestrict x bdd)

let permute (e:t) (tab:int array) : t
  =
  match e with
  | #Bdd.Expr0.t as x -> ((Bdd.Expr0.O.permute x tab):>expr)
  | `Apron x -> `Apron (Cudd.Mtbdd.permute x tab)

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

let make_env = Env.O.make

let typ_of_expr (expr:[<expr]) =
  match expr with
  | `Bool _ -> `Bool
  | `Bint(x) -> `Bint(x.Bdd.Int.signed, Array.length x.Bdd.Int.reg)
  | `Benum(x) -> `Benum(x.Bdd.Enum.typ)
  | `Apron _ -> `Real


(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

let check_typ2 e2 e3 =
  let t2 = typ_of_expr e2 in
  let t3 = typ_of_expr e3 in
  if t2 <> t3 then
    failwith
      (Print.sprintf "Expr.ite: 2 branches have different types %a and %a"
	Env.print_typ t2 Env.print_typ t3)
  else
    t2

let compose_of_substitution (env:('a,'b,Env.cond) #Env.O.t) (substitution:(string*expr) list)
  :
  Cudd.Man.v Cudd.Bdd.t array option * expr MappeS.t
  =
  let change = ref false in
  (* we first look at Boolean variables/conditions *)
  let (bsub,osub) =
    List.fold_left
      (begin fun (bsub,osub) (var,expr) ->
	begin match expr with
	| #Bdd.Expr0.t as x ->
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
  let tab = Bdd.Expr0.O.composition_of_substitution env bsub in
  if osub<>MappeS.empty then begin
    (* we now take care of other conditions *)
    for id=0 to pred(Array.length tab) do
      begin try
	let cond = env#cond_of_idb (id,true) in
	let supp = env#support_cond cond in
	let substitution = MappeS.interset osub supp in
	if substitution<>MappeS.empty then begin
	  change := true;
	  let bdd = match cond with
	    | `Apron x ->
		ApronexprDD.Condition.substitute env#cudd env x substitution
	  in
	  tab.(id) <- bdd
	end
      with Not_found ->
	()
      end
    done;
  end;
  ((if !change then Some tab else None), osub)

let substitute (env:('a,'b,Env.cond) #Env.O.t) (e:t) (substitution:(string*expr) list) : t
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
      | #Bdd.Expr0.t as x ->
	  ((Bdd.Expr0.O.compose x tab):>expr)
      | `Apron mtbdd ->
	  `Apron (Cudd.Mtbdd.vectorcompose tab mtbdd)
      end
    end
    else begin
      begin match e with
      | #Bdd.Expr0.t as x ->
	  begin match tab with
	  | None -> e
	  | Some(tab) -> ((Bdd.Expr0.O.compose x tab):>expr)
	  end
      | `Apron expr ->
	  let cudd = env#cudd in
	  let default = Cudd.Mtbdd.cst_u cudd (Cudd.Mtbdd.pick_leaf_u expr) in
	  let leaves_u = Cudd.Mtbdd.leaves_u expr in
	  let res = ref default in
	  Array.iter
	    (begin fun leaf_u ->
	      let guard = Cudd.Mtbdd.guard_of_leaf_u expr leaf_u in
	      let apronexpr = Cudd.Mtbdd.get leaf_u in
	      let nguard = match tab with
		| None -> guard
		| Some(tab) -> Cudd.Bdd.vectorcompose tab guard
	      in
	      let nexpr = ApronexprDD.substitute cudd apronexpr sub in
	      res := Cudd.Mtbdd.ite nguard nexpr !res
	    end)
	    leaves_u;
	  `Apron !res
      end
    end
  end

let var env (var:string) : t =
  let typ = env#typ_of_var var in
  match typ with
  | #Bdd.Env.typ ->
      ((Bdd.Expr0.O.var env var):>expr)
  | `Int | `Real ->
      `Apron (ApronexprDD.var env#cudd env var)
  | _ -> raise Not_found

let substitute_by_var (env:('a,'b,Env.cond) #Env.O.t) e (substitution:(string*string) list) =
  substitute env e (List.map (fun (v1,v2) -> (v1,var env v2)) substitution)

let ddsubstitute = substitute
let ddsubstitute_by_var = substitute_by_var

(*  ====================================================================== *)
(** {3 Boolean expressions} *)
(*  ====================================================================== *)

module Bool = struct
  type t = Cudd.Man.v Bdd.Expr0.O.Bool.t
  let of_expr = Bdd.Expr0.O.Bool.of_expr
  let to_expr = Bdd.Expr0.O.Bool.to_expr

  let dtrue = Bdd.Expr0.O.Bool.dtrue
  let dfalse = Bdd.Expr0.O.Bool.dfalse
  let of_bool = Bdd.Expr0.O.Bool.of_bool
  let var = Bdd.Expr0.O.Bool.var

  let dnot = Bdd.Expr0.O.Bool.dnot
  let dand = Bdd.Expr0.O.Bool.dand
  let dor = Bdd.Expr0.O.Bool.dor

  let xor = Bdd.Expr0.O.Bool.xor
  let nand = Bdd.Expr0.O.Bool.nand
  let nor = Bdd.Expr0.O.Bool.nor
  let nxor = Bdd.Expr0.O.Bool.nxor
    (** Exclusive or, not and, nor or and not xor *)

  let leq = Bdd.Expr0.O.Bool.leq
  let eq = Bdd.Expr0.O.Bool.eq
  let ite = Bdd.Expr0.O.Bool.ite

  let is_true = Bdd.Expr0.O.Bool.is_true
  let is_false = Bdd.Expr0.O.Bool.is_false
  let is_cst = Bdd.Expr0.O.Bool.is_cst
  let is_eq = Bdd.Expr0.O.Bool.is_eq
  let is_leq = Bdd.Expr0.O.Bool.is_leq
  let is_and_false = Bdd.Expr0.O.Bool.is_and_false

  let exist = Bdd.Expr0.O.Bool.exist
  let forall = Bdd.Expr0.O.Bool.forall

  let cofactor = Bdd.Expr0.O.Bool.cofactor
  let restrict = Bdd.Expr0.O.Bool.restrict
  let tdrestrict = Bdd.Expr0.O.Bool.tdrestrict
  let permute = Bdd.Expr0.O.Bool.permute

  let substitute_by_var (env:('a,'b,Env.cond) #Env.O.t) (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute (env:('a,'b,Env.cond) #Env.O.t) (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)

  let print = Bdd.Expr0.O.Bool.print
end

(*  ====================================================================== *)
(** {3 Bounded integer expressions} *)
(*  ====================================================================== *)

module Bint = struct
  type t = Cudd.Man.v Bdd.Expr0.O.Bint.t
  let of_expr = Bdd.Expr0.O.Bint.of_expr
  let to_expr = Bdd.Expr0.O.Bint.to_expr

  let of_int = Bdd.Expr0.O.Bint.of_int
  let var = Bdd.Expr0.O.Bint.var
  let ite = Bdd.Expr0.O.Bint.ite

  let neg = Bdd.Expr0.O.Bint.neg
  let succ = Bdd.Expr0.O.Bint.succ
  let pred = Bdd.Expr0.O.Bint.pred
  let add = Bdd.Expr0.O.Bint.add
  let sub = Bdd.Expr0.O.Bint.sub
  let mul = Bdd.Expr0.O.Bint.mul
  let shift_left = Bdd.Expr0.O.Bint.shift_left
  let shift_right = Bdd.Expr0.O.Bint.shift_right
  let scale = Bdd.Expr0.O.Bint.scale
  let zero = Bdd.Expr0.O.Bint.zero
  let eq = Bdd.Expr0.O.Bint.eq
  let eq_int = Bdd.Expr0.O.Bint.eq_int
  let supeq = Bdd.Expr0.O.Bint.supeq
  let supeq_int = Bdd.Expr0.O.Bint.supeq_int
  let sup = Bdd.Expr0.O.Bint.sup

  let cofactor = Bdd.Expr0.O.Bint.cofactor
  let restrict = Bdd.Expr0.O.Bint.restrict
  let tdrestrict = Bdd.Expr0.O.Bint.tdrestrict
  let permute = Bdd.Expr0.O.Bint.permute

  let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
  let guard_of_int= Bdd.Expr0.O.Bint.guard_of_int
  let guardints= Bdd.Expr0.O.Bint.guardints

  let print = Bdd.Expr0.O.Bint.print
end

(*  ====================================================================== *)
(** {3 Enumerated type expressions} *)
(*  ====================================================================== *)

module Benum = struct
  type t = Cudd.Man.v Bdd.Expr0.O.Benum.t
  let of_expr = Bdd.Expr0.O.Benum.of_expr
  let to_expr = Bdd.Expr0.O.Benum.to_expr
  let var = Bdd.Expr0.O.Benum.var
  let ite = Bdd.Expr0.O.Benum.ite
  let eq = Bdd.Expr0.O.Benum.eq
  let eq_label = Bdd.Expr0.O.Benum.eq_label
  let cofactor = Bdd.Expr0.O.Benum.cofactor
  let restrict = Bdd.Expr0.O.Benum.restrict
  let tdrestrict = Bdd.Expr0.O.Benum.tdrestrict
  let permute = Bdd.Expr0.O.Benum.permute
 let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
  let guard_of_label = Bdd.Expr0.O.Benum.guard_of_label
  let guardlabels = Bdd.Expr0.O.Benum.guardlabels
  let print = Bdd.Expr0.O.Benum.print
end

(*  ====================================================================== *)
(** {3 Apronexprmetic expressions} *)
(*  ====================================================================== *)

module Apron = struct
  type t = ApronexprDD.t

  let of_expr = ApronexprDD.of_expr
  let to_expr = ApronexprDD.to_expr

  let print env fmt x = ApronexprDD.print (Bool.print env) fmt x

  let cst env c = ApronexprDD.cst env#cudd c
  let var env name = ApronexprDD.var env#cudd env name
  let add env = ApronexprDD.add
  let sub env = ApronexprDD.sub
  let mul env = ApronexprDD.mul
  let div env = ApronexprDD.div
  let gmod env = ApronexprDD.gmod
  let negate env = ApronexprDD.negate
  let cast env = ApronexprDD.cast
  let sqrt env = ApronexprDD.sqrt

  let supeq (env:('a,'b,'c) #Env.O.t) = ApronexprDD.Condition.supeq env
  let sup (env:('a,'b,'c) #Env.O.t) = ApronexprDD.Condition.sup env
  let eq (env:('a,'b,'c) #Env.O.t) = ApronexprDD.Condition.eq env

  let ite (env:('a,'b,'c) #Env.O.t) b (t1:t) (t2:t) : t = Cudd.Mtbdd.ite b t1 t2

  let cofactor = Cudd.Mtbdd.cofactor
  let restrict = Cudd.Mtbdd.restrict
  let tdrestrict = Cudd.Mtbdd.tdrestrict
  let permute = Cudd.Mtbdd.permute
  let support = Cudd.Mtbdd.support
  let support_leaf = ApronexprDD.support_leaf

  let substitute_by_var env (e:t) sub = of_expr (ddsubstitute_by_var env (to_expr e) sub)
  let substitute env (e:t) sub = of_expr (ddsubstitute env (to_expr e) sub)
end

(*  ====================================================================== *)
(** {3 General expressions} *)
(*  ====================================================================== *)

let eq (env:('a,'b,'c) #Env.O.t) (e1:t) (e2:t) : Bool.t
  =
  let t = check_typ2 e1 e2 in
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

let ite env (cond:Bool.t) (e1:t) (e2:t) : t
  =
  let t = check_typ2 e1 e2 in
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

let support_cond env (expr:t) : Cudd.Man.v Cudd.Bdd.t =
  match expr with
  | #Bdd.Expr0.t as x ->
      Bdd.Expr0.O.support_cond env (x:> Cudd.Man.v Bdd.Expr0.t)
  | `Apron x ->
      Apron.support x
 
let support env (expr:t) : SetteS.t
  =
  let supp = support_cond env expr in
  let list = Cudd.Bdd.list_of_support supp in
  let set = ref SetteS.empty in
  List.iter
    (begin fun id ->
      try
	let cond = env#cond_of_idb (id,true) in
	let supp = env#support_cond cond in
	set := SetteS.union supp !set
      with Not_found ->
	let var = MappeI.find id env#idcondvar in
	set := SetteS.add var !set
    end)
    list
  ;
  begin match expr with
  | #Bdd.Expr0.t -> ()
  | `Apron expr ->
      set := SetteS.union (Apron.support_leaf expr) !set
  end;
  !set

let print (env:('a,'b,'c) #Env.O.t) (fmt:Format.formatter) (expr:[<expr])
  =
  match expr with
  | #Bdd.Expr0.t as x -> Bdd.Expr0.O.print env fmt x
  | `Apron x -> Apron.print env fmt x

let print_bdd = Bool.print

end

(*  ********************************************************************** *)
(** Closed signature *)
(*  ********************************************************************** *)

let typ_of_expr= O.typ_of_expr
let var = O.var
let ite = O.ite
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute
let support = O.support
let eq = O.eq
let support_cond = O.support_cond
let print = O.print

module Bool = struct
  include O.Bool
end
module Bint = struct
  include O.Bint
end
module Benum = struct
  include O.Benum
end
module Apron = struct
  type t = ApronexprDD.t
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
