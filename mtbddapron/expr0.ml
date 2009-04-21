(** Finite-type and arithmetical expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type t = [
  | Cudd.Man.v Bdd.Expr0.t
  | `Apron of ApronexprDD.t
]
type expr = t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

module O = struct

  let print_bdd
      (env:('a,'b) #Env.O.t as 'c)
      (cond:(Cond.cond,'c) #Cond.O.t)
      fmt bdd
      =
    Bdd.Expr0.O.print_bdd
      ~print_external_idcondb:begin fun fmt idb ->
	let condition = cond#cond_of_idb idb in
	cond#print_cond env fmt condition
      end
      env fmt bdd

  let typ_of_expr (expr:[<expr]) =
    match expr with
    | `Bool _ -> `Bool
    | `Bint(x) -> `Bint(x.Bdd.Int.signed, Array.length x.Bdd.Int.reg)
    | `Benum(x) -> `Benum(x.Bdd.Enum.typ)
    | `Apron _ -> `Real


  (*  ==================================================================== *)
  (** {3 General expressions} *)
  (*  ==================================================================== *)

  let check_typ2 e2 e3 =
    let t2 = typ_of_expr e2 in
    let t3 = typ_of_expr e3 in
    if t2 <> t3 then
      failwith
	(Print.sprintf "Expr.ite: 2 branches have different types %a and %a"
	  Env.print_typ t2 Env.print_typ t3)
    else
      t2

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

  let permute_list le tab
      =
    List.map (fun e -> permute e tab) le

  let compose_of_lvarexpr
      (env:('a,'b) #Env.O.t as 'c) (cond:(Cond.cond,'c) #Cond.O.t)
      (substitution:(string*expr) list)
      :
      Cudd.Man.v Cudd.Bdd.t array option * (string, expr) PMappe.t
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
	      (bsub, PMappe.add var expr osub)
	  end
	end)
	([],(PMappe.empty String.compare))
	substitution
    in
    if bsub<>[] then change := true;
    (* we initialize the substitution table *)
    let tab = Bdd.Expr0.O.composition_of_lvarexpr env bsub in
    if not (PMappe.is_empty osub) then begin
      (* we now take care of other conditions *)
      for id=0 to pred(Array.length tab) do
	begin try
	  let condition = cond#cond_of_idb (id,true) in
	  let supp = cond#support_cond env condition in
	  let substitution = PMappe.interset osub supp in
	  if not (PMappe.is_empty substitution) then begin
	    change := true;
	    let bdd = match condition with
	      | `Apron x ->
		  ApronexprDD.Condition.substitute env#cudd env cond x substitution
	    in
	    tab.(id) <- bdd
	  end
	with Not_found ->
	  ()
	end
      done;
    end;
    ((if !change then Some tab else None), osub)

  let substitute
      (env:('a,'b) #Env.O.t as 'c) (cond:(Cond.cond,'c) #Cond.O.t)
      (e:t) (substitution:(string*expr) list) : t
      =
    if substitution = [] then
      e
    else begin
      let (tab,sub) = compose_of_lvarexpr env cond substitution in
      if PMappe.is_empty sub then begin
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

  let var
      (env:('a,'b) #Env.O.t as 'c) (cond:(Cond.cond,'c) #Cond.O.t)
      (var:string) : t
      =
    let typ = env#typ_of_var var in
    match typ with
    | #Bdd.Env.typ ->
	((Bdd.Expr0.O.var env var):>expr)
    | `Int | `Real ->
	`Apron (ApronexprDD.var env#cudd env var)
    | _ -> raise Not_found

  let substitute_by_var
      (env:('a,'b) #Env.O.t as 'c) (cond:(Cond.cond,'c) #Cond.O.t)
      e (substitution:(string*string) list)
      =
    substitute env cond e
      (List.map (fun (v1,v2) -> (v1,var env cond v2)) substitution)

  let ddsubstitute = substitute
  let ddsubstitute_by_var = substitute_by_var

  (*  ==================================================================== *)
  (** {3 Boolean expressions} *)
  (*  ==================================================================== *)

  module Bool = struct
    type t = Cudd.Man.v Bdd.Expr0.O.Bool.t
    let of_expr = Bdd.Expr0.O.Bool.of_expr
    let to_expr = Bdd.Expr0.O.Bool.to_expr

    let dtrue env cond = Bdd.Expr0.O.Bool.dtrue env
    let dfalse env cond = Bdd.Expr0.O.Bool.dfalse env
    let of_bool env cond = Bdd.Expr0.O.Bool.of_bool env
    let var env cond = Bdd.Expr0.O.Bool.var env

    let dnot env cond = Bdd.Expr0.O.Bool.dnot env
    let dand env cond = Bdd.Expr0.O.Bool.dand env
    let dor env cond = Bdd.Expr0.O.Bool.dor env

    let xor env cond = Bdd.Expr0.O.Bool.xor env
    let nand env cond = Bdd.Expr0.O.Bool.nand env
    let nor env cond = Bdd.Expr0.O.Bool.nor env
    let nxor env cond = Bdd.Expr0.O.Bool.nxor env
    (** Exclusive or, not and, nor or and not xor *)

    let leq env cond = Bdd.Expr0.O.Bool.leq env
    let eq env cond = Bdd.Expr0.O.Bool.eq env
    let ite env cond = Bdd.Expr0.O.Bool.ite env

    let is_true env cond = Bdd.Expr0.O.Bool.is_true env
    let is_false env cond = Bdd.Expr0.O.Bool.is_false env
    let is_cst env cond = Bdd.Expr0.O.Bool.is_cst env
    let is_eq env cond = Bdd.Expr0.O.Bool.is_eq env
    let is_leq env cond = Bdd.Expr0.O.Bool.is_leq env
    let is_and_false env cond = Bdd.Expr0.O.Bool.is_and_false env

    let exist env cond = Bdd.Expr0.O.Bool.exist env
    let forall env cond = Bdd.Expr0.O.Bool.forall env

    let cofactor = Bdd.Expr0.O.Bool.cofactor
    let restrict = Bdd.Expr0.O.Bool.restrict
    let tdrestrict = Bdd.Expr0.O.Bool.tdrestrict
    let permute = Bdd.Expr0.O.Bool.permute

    let substitute_by_var env cond (e:t) sub = of_expr (ddsubstitute_by_var env cond (to_expr e) sub)
    let substitute env cond (e:t) sub = of_expr (ddsubstitute env cond (to_expr e) sub)

    let print = print_bdd
  end

  (*  ==================================================================== *)
  (** {3 Bounded integer expressions} *)
  (*  ==================================================================== *)

  module Bint = struct
    type t = Cudd.Man.v Bdd.Expr0.O.Bint.t
    let of_expr = Bdd.Expr0.O.Bint.of_expr
    let to_expr = Bdd.Expr0.O.Bint.to_expr

    let of_int env cond = Bdd.Expr0.O.Bint.of_int env
    let var env cond = Bdd.Expr0.O.Bint.var env
    let ite env cond = Bdd.Expr0.O.Bint.ite env

    let neg env cond = Bdd.Expr0.O.Bint.neg env
    let succ env cond = Bdd.Expr0.O.Bint.succ env
    let pred env cond = Bdd.Expr0.O.Bint.pred env
    let add env cond = Bdd.Expr0.O.Bint.add env
    let sub env cond = Bdd.Expr0.O.Bint.sub env
    let mul env cond = Bdd.Expr0.O.Bint.mul env
    let shift_left env cond = Bdd.Expr0.O.Bint.shift_left env
    let shift_right env cond = Bdd.Expr0.O.Bint.shift_right env
    let scale env cond = Bdd.Expr0.O.Bint.scale env
    let zero env cond = Bdd.Expr0.O.Bint.zero env
    let eq env cond = Bdd.Expr0.O.Bint.eq env
    let eq_int env cond = Bdd.Expr0.O.Bint.eq_int env
    let supeq env cond = Bdd.Expr0.O.Bint.supeq env
    let supeq_int env cond = Bdd.Expr0.O.Bint.supeq_int env
    let sup env cond = Bdd.Expr0.O.Bint.sup env

    let cofactor = Bdd.Expr0.O.Bint.cofactor
    let restrict = Bdd.Expr0.O.Bint.restrict
    let tdrestrict = Bdd.Expr0.O.Bint.tdrestrict
    let permute = Bdd.Expr0.O.Bint.permute

    let substitute_by_var env cond (e:t) sub = of_expr (ddsubstitute_by_var env cond (to_expr e) sub)
    let substitute env cond (e:t) sub = of_expr (ddsubstitute env cond (to_expr e) sub)
    let guard_of_int env cond = Bdd.Expr0.O.Bint.guard_of_int env
    let guardints env cond = Bdd.Expr0.O.Bint.guardints env

    let print env cond fmt x = Bdd.Int.print_minterm (print_bdd env cond) fmt x
  end

  (*  ==================================================================== *)
  (** {3 Enumerated type expressions} *)
  (*  ==================================================================== *)

  module Benum = struct
    type t = Cudd.Man.v Bdd.Expr0.O.Benum.t
    let of_expr = Bdd.Expr0.O.Benum.of_expr
    let to_expr = Bdd.Expr0.O.Benum.to_expr
    let var env cond = Bdd.Expr0.O.Benum.var env
    let ite env cond = Bdd.Expr0.O.Benum.ite env
    let eq env cond = Bdd.Expr0.O.Benum.eq env
    let eq_label env cond = Bdd.Expr0.O.Benum.eq_label env
    let cofactor = Bdd.Expr0.O.Benum.cofactor
    let restrict = Bdd.Expr0.O.Benum.restrict
    let tdrestrict = Bdd.Expr0.O.Benum.tdrestrict
    let permute = Bdd.Expr0.O.Benum.permute
    let substitute_by_var env cond (e:t) sub = of_expr (ddsubstitute_by_var env cond (to_expr e) sub)
    let substitute env cond (e:t) sub = of_expr (ddsubstitute env cond (to_expr e) sub)
    let guard_of_label env cond = Bdd.Expr0.O.Benum.guard_of_label env
    let guardlabels env cond = Bdd.Expr0.O.Benum.guardlabels env
    let print env cond fmt x = Bdd.Enum.print_minterm (print_bdd env cond) env fmt x
  end

  (*  ==================================================================== *)
  (** {3 Apronexprmetic expressions} *)
  (*  ==================================================================== *)

  module Apron = struct
    type t = ApronexprDD.t

    let of_expr = ApronexprDD.of_expr
    let to_expr = ApronexprDD.to_expr

    let print env cond fmt x = ApronexprDD.print (print_bdd env cond) fmt x

    let cst env cond c = ApronexprDD.cst env#cudd c
    let var env cond name = ApronexprDD.var env#cudd env name
    let add env cond = ApronexprDD.add
    let sub env cond = ApronexprDD.sub
    let mul env cond = ApronexprDD.mul
    let div env cond = ApronexprDD.div
    let gmod env cond = ApronexprDD.gmod
    let negate env cond = ApronexprDD.negate
    let cast env cond = ApronexprDD.cast
    let sqrt env cond = ApronexprDD.sqrt

    let supeq (env:('a,'b) #Env.O.t) cond = ApronexprDD.Condition.supeq env cond
    let sup (env:('a,'b) #Env.O.t) cond = ApronexprDD.Condition.sup env cond
    let eq (env:('a,'b) #Env.O.t) cond = ApronexprDD.Condition.eq env cond

    let ite (env:('a,'b) #Env.O.t) cond b (t1:t) (t2:t) : t =
      Cudd.Mtbdd.ite b t1 t2

    let cofactor = Cudd.Mtbdd.cofactor
    let restrict = Cudd.Mtbdd.restrict
    let tdrestrict = Cudd.Mtbdd.tdrestrict
    let permute = Cudd.Mtbdd.permute
    let support = Cudd.Mtbdd.support
    let support_leaf = ApronexprDD.support_leaf

    let substitute_by_var env cond (e:t) sub = of_expr (ddsubstitute_by_var env cond (to_expr e) sub)
    let substitute env cond (e:t) sub = of_expr (ddsubstitute env cond (to_expr e) sub)
  end

  (*  ==================================================================== *)
  (** {3 General expressions} *)
  (*  ==================================================================== *)

  let eq env cond (e1:t) (e2:t) : Bool.t
      =
    let t = check_typ2 e1 e2 in
    match t with
    | `Bool ->
	Bool.eq env cond (Bool.of_expr e1) (Bool.of_expr e2)
    | `Bint _ ->
	Bint.eq env cond (Bint.of_expr e1) (Bint.of_expr e2)
    | `Benum _ ->
	Benum.eq env cond (Benum.of_expr e1) (Benum.of_expr e2)
    | `Real ->
	let diff = Apron.sub env cond
	  (Apron.of_expr e1)
	  (Apron.of_expr e2)
	in
	Apron.eq env cond diff
    | _ -> failwith ""

  let ite env cond (condition:Bool.t) (e1:t) (e2:t) : t
      =
    let t = check_typ2 e1 e2 in
    match t with
    | `Bool ->
	`Bool (Bool.ite env cond condition (Bool.of_expr e1) (Bool.of_expr e2))
    | `Bint _ ->
	`Bint (Bint.ite env cond condition (Bint.of_expr e1) (Bint.of_expr e2))
    | `Benum _ ->
	`Benum (Benum.ite env cond condition (Benum.of_expr e1) (Benum.of_expr e2))
    | `Real ->
	Apron.to_expr (Apron.ite env cond condition (Apron.of_expr e1) (Apron.of_expr e2))
    | _ -> failwith ""

  let support_cond
      (env: < cudd : Cudd.Man.v Cudd.Man.t; .. >)
      (expr:t) : Cudd.Man.v Cudd.Bdd.t
      =
    match expr with
    | #Bdd.Expr0.t as x ->
	Bdd.Expr0.O.support_cond env (x:> Cudd.Man.v Bdd.Expr0.t)
    | `Apron x ->
	Apron.support x

  let support
      (env:('a,'b) #Env.O.t as 'c) (cond:(Cond.cond,'c) #Cond.O.t)
      (expr:t) : string PSette.t
      =
    let supp = support_cond env expr in
    let list = Cudd.Bdd.list_of_support supp in
    let set = ref (PSette.empty String.compare) in
    List.iter
      (begin fun id ->
	try
	  let condition = cond#cond_of_idb (id,true) in
	  let supp = cond#support_cond env condition in
	  set := PSette.union supp !set
	with Not_found ->
	  let var = PMappe.find id env#idcondvar in
	  set := PSette.add var !set
      end)
      list
    ;
    begin match expr with
    | #Bdd.Expr0.t -> ()
    | `Apron expr ->
	set := PSette.union (Apron.support_leaf expr) !set
    end;
    !set

  let normalize 
      ?(reduce=false) ?(careset=false) 
      ((cond,lexpr):(((Cond.cond,'a) #Cond.O.t as 'b) * t list))
      :
      'b * t list
      =
    let ncond = Oo.copy cond in
    let support lexpr =
      let cond_supp = ncond#cond_supp in
      List.fold_left
	(begin fun supp expr ->
	  Cudd.Bdd.support_union supp 
	    (Cudd.Bdd.support_inter 
	      cond_supp 
	      (support_cond ncond expr))
	end)
	(Cudd.Bdd.dtrue cond#cudd)
	lexpr
    in
    if reduce then begin
      let supp = support lexpr in
      ncond#reduce supp
    end;
    let perm = ncond#normalize in
    let lexpr = List.map (fun e -> permute e perm) lexpr in
    let lexpr = 
      if careset then begin
	ncond#compute_careset ~normalized:true;
	let careset = ncond#careset in
	let lexpr = List.map (fun e -> tdrestrict e careset) lexpr in
	if reduce then begin
	  let supp = support lexpr in
	  if not (Cudd.Bdd.is_equal supp ncond#cond_supp) then begin
	    ncond#reduce supp;
	    let perm = ncond#normalize in
	    let lexpr = List.map (fun e -> permute e perm) lexpr in
	    lexpr
	  end
	  else lexpr
	end
	else lexpr
      end
      else lexpr
    in
    (ncond,lexpr)

  let print
      env cond
      (fmt:Format.formatter) (expr:[<expr])
      =
    match expr with
    | `Bool x -> Bool.print env cond fmt x
    | `Bint x -> Bint.print env cond fmt x
    | `Benum x -> Benum.print env cond fmt x
    | `Apron x -> Apron.print env cond fmt x

end

(*  ********************************************************************** *)
(** Closed signature *)
(*  ********************************************************************** *)

let typ_of_expr= O.typ_of_expr
let var = O.var
let ite = O.ite
let cofactor = O.cofactor
let restrict = O.restrict
let tdrestrict = O.tdrestrict
let permute = O.permute
let substitute_by_var = O.substitute_by_var
let substitute = O.substitute  
let support = O.support
let eq = O.eq
let support_cond env = O.support_cond
let print = O.print
let normalize = O.normalize

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
