(** Decision Diagrams on top of arithmetic expressions *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Decision diagram} *)
(*  ********************************************************************** *)


let table = Cudd.PWeakke.create Apronexpr.hash Apronexpr.equal 23

type t = Apronexpr.t Cudd.Mtbdd.t

let of_expr = function
| `Apron x -> x
| _ -> failwith "ApronexprDD.of_expr: Arithmetical expression expected"

let to_expr (x:t) = `Apron x

let print print_bdd fmt (expr:t) =
  Cudd.Mtbdd.print
    print_bdd
    Apronexpr.print
    fmt expr

let is_zero expr = (Apronexpr.compare expr Apronexpr.zero) = 0
let is_one expr = (Apronexpr.compare expr Apronexpr.one) = 0
let absorbant_zero expr =
  let expr0 = Cudd.Mtbdd.get expr in
  if (Apronexpr.equal expr0 Apronexpr.zero)
  then Some expr
  else None
let absorbant_one expr =
  let expr0 = Cudd.Mtbdd.get expr in
  if (Apronexpr.equal expr0 Apronexpr.one)
  then Some expr
  else None

let cst cudd coeff = 
  Cudd.Mtbdd.cst cudd table (Apronexpr.cst coeff)
let var cudd env v =
  Cudd.Mtbdd.cst cudd table (Apronexpr.var env v)
let add ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.Mtbdd.map_op2
    ~commutative:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then
	if is_zero (Cudd.Mtbdd.dval dd1) then Some dd2 else None
      else 
	if is_zero (Cudd.Mtbdd.dval dd2) then Some dd1 else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique table
	(Apronexpr.add ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let sub ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.Mtbdd.map_op2
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd2 && is_zero (Cudd.Mtbdd.dval dd2) then Some dd1
      else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique table
	(Apronexpr.sub ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let mul ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.Mtbdd.map_op2
    ~commutative:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then
	let v = Cudd.Mtbdd.dval dd1 in
	if is_zero v then Some dd1
	else if is_one v then Some dd2
	else None
      else 
	let v = Cudd.Mtbdd.dval dd2 in
	if is_zero v then Some dd2
	else if is_one v then Some dd1
	else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique table
	(Apronexpr.mul ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let div ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.Mtbdd.map_op2
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 then 
	if is_zero (Cudd.Mtbdd.dval dd1) then Some dd1 else None
      else 
	if is_one (Cudd.Mtbdd.dval dd2) then Some dd1 else None
    )
    (fun e1 e2 ->
      Cudd.Mtbdd.unique table
	(Apronexpr.div ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2

let gmod ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  Cudd.Mtbdd.map_op2
    (fun e1 e2 ->
      Cudd.Mtbdd.unique table
	(Apronexpr.gmod ~typ ~round (Cudd.Mtbdd.get e1) (Cudd.Mtbdd.get e2)))
    e1 e2
let negate e =
  Cudd.Mtbdd.map_op1
    (fun e -> Cudd.Mtbdd.unique table (Apronexpr.negate (Cudd.Mtbdd.get e)))
    e

let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Cudd.Mtbdd.map_op1
    (fun e -> Cudd.Mtbdd.unique table (Apronexpr.cast ~typ ~round (Cudd.Mtbdd.get e)))
    e

let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Cudd.Mtbdd.map_op1
    (fun e -> Cudd.Mtbdd.unique table (Apronexpr.sqrt ~typ ~round (Cudd.Mtbdd.get e)))
    e

let support_leaf e =
  Array.fold_left
    (fun res e -> PSette.union res (Apronexpr.support e))
    (PSette.empty String.compare)
    (Cudd.Mtbdd.leaves e)

let support_cond = Cudd.Mtbdd.support

let substitute_linexpr
  cudd
  (linexpr:Apronexpr.Lin.t)
  (substitution:(string, [>`Apron of t]) PMappe.t)
  :
  t
  =
  let substitute_linterm (term:Apronexpr.Lin.term) : t
    =
    let (mpqf,var) = term in
    let expr = of_expr (PMappe.find var substitution) in
    let coeffexpr = 
      Cudd.Mtbdd.cst cudd table
	(Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    mul coeffexpr expr
  in

  let (lterm1,lterm2) =
    List.partition
      (fun (coeff,var) -> PMappe.mem var substitution)
      linexpr.Apronexpr.Lin.lterm
  in
  if lterm1=[] then
    Cudd.Mtbdd.cst cudd table (Apronexpr.Lin linexpr)
  else begin
    let linexpr2 = {
      Apronexpr.Lin.cst = linexpr.Apronexpr.Lin.cst;
      Apronexpr.Lin.lterm = lterm2
    }
    in
    let expr2 = 
      Cudd.Mtbdd.cst cudd table (Apronexpr.Lin linexpr2)
    in
    let expr =
      List.fold_left
	(begin fun res term ->
	  add res (substitute_linterm term)
	end)
	expr2 lterm1
    in
    expr
  end

let substitute_polyexpr
  cudd
  (polyexpr:Apronexpr.Poly.t)
  (substitution:(string, [>`Apron of t]) PMappe.t)
  :
  t
  =
  let substitute_varexp (res:t) (varexp:Apronexpr.Poly.varexp) : t
    =
    let (var,exp) = varexp in
    try
      let expr = of_expr (PMappe.find var substitution) in
      let res = ref res in
      for i=1 to exp do
	res := mul !res expr
      done;
      !res
    with Not_found ->
      let monomial = [varexp] in
      let term = ((Mpqf.of_int 1), monomial) in
      let polyexpr = [term] in
      Cudd.Mtbdd.cst cudd table (Apronexpr.Poly polyexpr)
  in
  let substitute_monomial (res:t) (monomial:Apronexpr.Poly.monomial) : t
    =
    List.fold_left substitute_varexp res monomial
  in
  let substitute_term (term:Apronexpr.Poly.term) : t
    =
    let (mpqf,monomial) = term in
    let res = Cudd.Mtbdd.cst cudd table
	(Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    substitute_monomial res monomial
  in
  let res = Cudd.Mtbdd.cst cudd table
    (Apronexpr.cst (Apron.Coeff.s_of_int 0))
  in
  List.fold_left
    (begin fun res term ->
      add res (substitute_term term)
    end)
    res polyexpr

let substitute_treeexpr
  cudd
  (treeexpr:Apronexpr.Tree.t)
  (substitution:(string, [>`Apron of t]) PMappe.t)
  :
  t
  =
  let rec parcours = function
    | Apronexpr.Tree.Cst coeff ->
	Cudd.Mtbdd.cst cudd table (Apronexpr.cst coeff)
    | Apronexpr.Tree.Var var ->
	let name = Apron.Var.to_string var in
	begin
	  try of_expr (PMappe.find name substitution)
	  with Not_found ->
	    Cudd.Mtbdd.cst cudd table (Apronexpr.Lin(Apronexpr.Lin.var name))
	end
    | Apronexpr.Tree.Unop(unop,e,t,r) ->
	let res = parcours e in
	(begin match unop with
	| Apronexpr.Tree.Neg -> negate res
	| Apronexpr.Tree.Cast -> cast ~typ:t ~round:r res
	| Apronexpr.Tree.Sqrt -> sqrt ~typ:t ~round:r res
	end)
    | Apronexpr.Tree.Binop(binop,e1,e2,t,r) ->
	let res1 = parcours e1 in
	let res2 = parcours e2 in
	begin match binop with
	| Apronexpr.Tree.Add -> add
	| Apronexpr.Tree.Sub -> sub
	| Apronexpr.Tree.Mul -> mul
	| Apronexpr.Tree.Div -> div
	| Apronexpr.Tree.Mod -> gmod
	end
	  ~typ:t ~round:r res1 res2
  in
  parcours treeexpr

let substitute
  cudd 
  (expr:Apronexpr.t)
  (substitution:(string, [>`Apron of t]) PMappe.t)
  :
  t
  =
  match expr with
  | Apronexpr.Lin x -> substitute_linexpr cudd x substitution
  | Apronexpr.Poly x -> substitute_polyexpr cudd x substitution
  | Apronexpr.Tree x -> substitute_treeexpr cudd x substitution

module Condition = struct
  let make env cond typ e =
    let manager = Cudd.Mtbdd.manager e in
    let guardleafs = Cudd.Mtbdd.guardleafs e in
    let bdd =
      Array.fold_left
	(begin fun res (guard,e) ->
	  let atom =
	    let condition = Apronexpr.Condition.make env typ e in
	    match condition with
	    | `Bool b ->
		if b then Cudd.Bdd.dtrue manager else Cudd.Bdd.dfalse manager
	    | `Cond x ->
		let (id,b) = cond#idb_of_cond env (`Apron x) in
		let bdd = Cudd.Bdd.ithvar manager id in
		if b then bdd else Cudd.Bdd.dnot bdd
	  in
	  Cudd.Bdd.dor res (Cudd.Bdd.dand guard atom)
	end)
	(Cudd.Bdd.dfalse manager)
	guardleafs
    in
    bdd

  let supeq env cond expr = make env cond Apronexpr.Condition.SUPEQ expr
  let sup env cond expr = make env cond Apronexpr.Condition.SUP expr
  let eq env cond expr = make env cond Apronexpr.Condition.EQ expr

  let substitute
   cudd
   env
   cond
   (condition:Apronexpr.Condition.t)
   (substitution:(string,[>`Apron of t]) PMappe.t)
   :
   Cudd.Man.v Cudd.Bdd.t
    =
    let (typ,expr) = condition in
    let nexpr = substitute cudd expr substitution in
    make env cond typ nexpr

end
