(** Arithmetical Decision Diagrams *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Decision diagram} *)
(*  ********************************************************************** *)

include Mtbdd2

let make_manager () :  Apronexpr.expr manager =
  let background = Apronexpr.sqrt (Apronexpr.cst (Apron.Coeff.s_of_int (-1))) in
  Mtbdd2.make_manager
    ~background:background
    ~hash:Apronexpr.hash
    ~equal:Apronexpr.equal

type expr = Apronexpr.expr t
type cond = [`Apron of Apronexpr.Condition.t]

let print print_bdd fmt (expr:expr) =
  Mtbdd2.print
    print_bdd
    Apronexpr.print
    fmt expr

let print_manager fmt x = Mtbdd2.print_manager Apronexpr.print fmt x

let var cudd man env v = Mtbdd2.cst cudd man (Apronexpr.var env v)
let add ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  assert(e1.man==e2.man);
  Mtbdd2.mapbinop ~commutative:true
    e1.man (Apronexpr.add ~typ ~round) e1 e2
let sub ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  assert(e1.man==e2.man);
  Mtbdd2.mapbinop ~commutative:false
    e1.man (Apronexpr.sub ~typ ~round) e1 e2
let mul ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  assert(e1.man==e2.man);
  Mtbdd2.mapbinop ~commutative:true
    e1.man (Apronexpr.mul ~typ ~round) e1 e2
let div ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  assert(e1.man==e2.man);
  Mtbdd2.mapbinop ~commutative:false
    e1.man (Apronexpr.div ~typ ~round) e1 e2
let gmod ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  assert(e1.man==e2.man);
  Mtbdd2.mapbinop ~commutative:false
    e1.man (Apronexpr.gmod ~typ ~round) e1 e2
let negate e =
  Mtbdd2.mapunop e.man Apronexpr.negate e
let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Mtbdd2.mapunop e.man (Apronexpr.cast ~typ ~round) e
let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Mtbdd2.mapunop e.man (Apronexpr.sqrt ~typ ~round) e
let support_leaf e =
  Array.fold_left
    (fun res e -> SetteS.union res (Apronexpr.support e))
    SetteS.empty
    (leaves e)
let support_cond = support

let of_expr = function 
| `Apron x -> x 
| _ -> failwith "ApronexprDD.of_expr: Arithmetical expression expected"
let to_expr (x:expr) = `Apron x
let substitute_linexpr 
  cudd
  manager
  (linexpr:Apronexpr.Lin.expr)
  (substitution:[>`Apron of expr] MappeS.t)
  :
  expr
  =
  let substitute_linterm (term:Apronexpr.Lin.term) : expr
    =
    let (mpqf,ref) = term in
    let expr = of_expr (MappeS.find ref substitution) in
    let coeffexpr = Mtbdd2.cst cudd manager 
      (Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    mul coeffexpr expr
  in

  let (lterm1,lterm2) =
    List.partition
      (fun (coeff,var) -> MappeS.mem var substitution)
      linexpr.Apronexpr.Lin.lterm
  in
  if lterm1=[] then
    Mtbdd2.cst cudd manager (Apronexpr.Lin linexpr)
  else begin
    let linexpr2 = {
      Apronexpr.Lin.cst = linexpr.Apronexpr.Lin.cst;
      Apronexpr.Lin.lterm = lterm2
    }
    in
    let expr2 = Mtbdd2.cst cudd manager (Apronexpr.Lin linexpr2) in
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
  cudd manager
  (polyexpr:Apronexpr.Poly.expr)
  (substitution:[>`Apron of expr] MappeS.t)
  :
  expr
  =
  let substitute_varexp (res:expr) (varexp:Apronexpr.Poly.varexp) : expr
    =
    let (var,exp) = varexp in
    try
      let expr = of_expr (MappeS.find var substitution) in
      let res = ref res in
      for i=1 to exp do
	res := mul !res expr
      done;
      !res
    with Not_found ->
      let monomial = [varexp] in
      let term = ((Mpqf.of_int 1), monomial) in
      let polyexpr = [term] in
      Mtbdd2.cst cudd manager (Apronexpr.Poly polyexpr)
  in
  let substitute_monomial (res:expr) (monomial:Apronexpr.Poly.monomial) : expr
    =
    List.fold_left substitute_varexp res monomial
  in
  let substitute_term (term:Apronexpr.Poly.term) : expr
    =
    let (mpqf,monomial) = term in
    let res = Mtbdd2.cst cudd manager 
      (Apronexpr.cst (Apron.Coeff.s_of_mpqf mpqf))
    in
    substitute_monomial res monomial
  in
  let res = Mtbdd2.cst cudd manager (Apronexpr.cst (Apron.Coeff.s_of_int 0)) in
  List.fold_left
    (begin fun res term ->
      add res (substitute_term term)
    end)
    res polyexpr

let substitute_treeexpr 
  cudd manager
  (treeexpr:Apronexpr.Tree.expr)
  (substitution:[>`Apron of expr] MappeS.t)
  :
  expr
  =
  let rec parcours = function
    | Apronexpr.Tree.Cst coeff ->
	Mtbdd2.cst cudd manager (Apronexpr.cst coeff)
    | Apronexpr.Tree.Var var ->
	let name = Apron.Var.to_string var in
	begin
	  try of_expr (MappeS.find name substitution)
	  with Not_found -> 
	    Mtbdd2.cst cudd manager (Apronexpr.Lin(Apronexpr.Lin.var name))
	end
    | Apronexpr.Tree.Unop(unop,e,t,r) ->
	let res = parcours e in
	mapunop
	  res.man
	  (begin fun e ->
	    (begin match unop with
	    | Apronexpr.Tree.Neg -> Apronexpr.negate
	    | Apronexpr.Tree.Cast -> Apronexpr.cast ~typ:t ~round:r
	    | Apronexpr.Tree.Sqrt -> Apronexpr.sqrt ~typ:t ~round:r
	    end)
	    e
	  end)
	  res
    | Apronexpr.Tree.Binop(binop,e1,e2,t,r) ->
	let res1 = parcours e1 in
	let res2 = parcours e2 in
	mapbinop ~commutative:(binop=Apronexpr.Tree.Add || binop=Apronexpr.Tree.Mul)
	  res1.man
	  (begin fun e1 e2 ->
	    (begin match binop with
	    | Apronexpr.Tree.Add -> Apronexpr.add
	    | Apronexpr.Tree.Sub -> Apronexpr.sub
	    | Apronexpr.Tree.Mul -> Apronexpr.mul
	    | Apronexpr.Tree.Div -> Apronexpr.div
	    | Apronexpr.Tree.Mod -> Apronexpr.gmod
	    end)
	    ~typ:t ~round:r e1 e2
	  end)
	  res1 res2
  in
  parcours treeexpr

let substitute
  cudd manager
  (expr:Apronexpr.expr)
  (substitution:[>`Apron of expr] MappeS.t)
  :
  expr
  =
  match expr with
  | Apronexpr.Lin x -> substitute_linexpr cudd manager x substitution
  | Apronexpr.Poly x -> substitute_polyexpr cudd manager x substitution
  | Apronexpr.Tree x -> substitute_treeexpr cudd manager x substitution

module Condition = struct
  let make env typ e =
    let manager = manager e in
    let guardleafs = guardleafs e in
    let bdd =
      Array.fold_left
	(begin fun res (guard,e) ->
	  let atom =
	    let cond = Apronexpr.Condition.make env typ e in
	    match cond with
	    | `Bool b ->
		if b then Bdd.dtrue manager else Bdd.dfalse manager
	    | `Cond x ->
		let (id,b) = env#idb_of_cond (`Apron x) in
		let bdd = Bdd.ithvar manager id in
		if b then bdd else Bdd.dnot bdd
	  in
	  Bdd.dor res (Bdd.dand guard atom)
	end)
	(Bdd.dfalse manager)
	guardleafs
    in
    bdd

  let supeq env expr = make env Apronexpr.Condition.SUPEQ expr 
  let sup env expr = make env Apronexpr.Condition.SUP expr
  let eq env expr = make env Apronexpr.Condition.EQ expr

  let substitute
   cudd manager 
   env
   (cond:Apronexpr.Condition.t)
   (substitution:[>`Apron of expr] MappeS.t)
   :
   Bdd.t
    =
    let (typ,expr) = cond in
    let nexpr = substitute cudd manager expr substitution in
    make env typ nexpr

end

let cst cudd man coeff = Mtbdd2.cst cudd man (Apronexpr.cst coeff)
