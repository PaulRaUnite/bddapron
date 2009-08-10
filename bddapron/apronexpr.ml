(** Purely arithmetic expressions (internal) *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type typ = [
  | `Int
  | `Real
]

type 'a typ_of_var = string -> 'a constraint 'a = [>typ]

exception Constant of int

(*  ********************************************************************** *)
(** {2 Expressions} *)
(*  ********************************************************************** *)

(* Scale the list such that the minimum rational it contains is with gcd equal
   to 1.  The factor applied is returned together with the new list.

   Assume non-empty list
*)
let reduce_list (list:(Mpqf.t * 'a) list) : (Mpqf.t * (Mpqf.t * 'a) list)
  =
  if list=[] then raise (Invalid_argument "Arith.reduce_list");
  (* compute lcm of denominators *)
  let tmp = Mpz.init () in
  let lcm = Mpz.init_set_si 1 in
  List.iter
    (begin fun (c,_) ->
      Mpq.get_den tmp (Mpqf.to_mpq c);
      Mpz.lcm lcm lcm tmp
    end)
    list
  ;
  (* multiplying by lcm -> getting a list of integers *)
  let (ncoeffs : Mpz.t list) =
    List.map
      (begin fun (c,_) ->
	let mpz = Mpz.init () in
	Mpq.get_den tmp (Mpqf.to_mpq c);
	Mpz.divexact mpz lcm tmp;
	Mpq.get_num tmp (Mpqf.to_mpq c);
	Mpz.mul mpz mpz tmp;
	mpz
      end)
      list
  in
  (* compute gcd of ncoeff *)
  let gcd = Mpz.init () in
  Mpz.set gcd (List.hd ncoeffs);
  Mpz.abs gcd gcd;
  List.iter
    (begin fun coeff ->
      Mpz.gcd gcd gcd coeff
    end)
    (List.tl ncoeffs)
  ;
  let factor = Mpqf.of_mpz2 lcm gcd in
  let nlist =
    List.map2
      (begin fun coeff (_,something) ->
	Mpz.divexact coeff coeff gcd;
	(Mpqf.of_mpz coeff,something)
      end)
      ncoeffs list
  in
  assert(Mpqf.sgn factor > 0);
  (factor,nlist)

(*  ==================================================================== *)
(** {3 Linear expressions} *)
(*  ==================================================================== *)

module Lin = struct
  type term = Mpqf.t * string

  type t = {
    cst: Mpqf.t;
    lterm: term list;
  }

  let is_dependent_on_integer_only typ_of_var (e:t) =
    List.for_all
      (begin fun (coeff,var) ->
	(typ_of_var var) = `Int
      end)
      e.lterm

  let normalize e = {
    cst = e.cst;
    lterm = List.stable_sort (fun (c1,v1) (c2,v2) -> String.compare v1 v2) e.lterm
  }

  let support (e:t) : string PSette.t =
    List.fold_left
      (begin fun res (_,var) -> PSette.add var res end)
      (PSette.empty String.compare)
      e.lterm

  let substitute_by_var e (substitution:(string,string) PMappe.t)
    =
    let ne = {
      cst = e.cst;
      lterm = begin
	List.map
	  (begin fun ((c,v) as x) ->
	    try
	      (c, PMappe.find v substitution)
	    with Not_found ->
	      x
	  end)
	  e.lterm
      end
    }
    in
    normalize ne

   let print fmt expr =
    let first = ref true in
    fprintf fmt "@[";
    if expr.lterm<>[] then begin
      List.iter
	(begin fun (coeff,var) ->
	  let sgn = Mpqf.sgn coeff in
	  if sgn <> 0 then begin
	    if sgn>0 then begin
	      if not !first then Format.pp_print_string fmt "+";
	    end;
	    if not !first then Format.fprintf fmt "@,";
	    if Mpqf.cmp_int coeff (-1) = 0 then
	      Format.pp_print_string fmt "-"
	    else if Mpqf.cmp_int coeff 1 <> 0 then
	      Mpqf.print fmt coeff
	    ;
	    pp_print_string fmt var;
	    first := false;
	  end
	end)
	expr.lterm;
    end;
    let sgn = Mpqf.sgn expr.cst in
    if !first || sgn < 0 then
      Mpqf.print fmt expr.cst
    else if sgn > 0 then
      fprintf fmt "+%a" Mpqf.print expr.cst
    ;
    fprintf fmt "@]"

  let rec compare_lterm l1 l2 =
    match (l1,l2) with
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	if (v1<v2) then
	  Mpqf.sgn c1
	else if (v1>v2) then
	  -(Mpqf.sgn c2)
	else
	  let s = Mpqf.cmp c1 c2 in
	  if s=0 then compare_lterm r1 r2 else s
    | ((c1,v1)::r1,[]) ->
	let s = Mpqf.sgn c1 in
	if s=0 then failwith "";
	s
    | ([],(c1,v1)::r1) ->
	let s = Mpqf.sgn c1 in
	if s=0 then failwith "";
	-s
    | ([],[]) -> 0

  let compare e1 e2 =
    let res = compare_lterm e1.lterm e2.lterm in
    if res=0 then
      let res = Mpqf.cmp e1.cst e2.cst in
      if res>0 then 1 else if res < 0 then -1 else 0
    else
      if res>0 then 2 else if res < 0 then -2 else 0

  let var (x:string) = {
    cst = Mpqf.of_int 0;
    lterm = [ (Mpqf.of_int 1, x) ]
  }

  let cst (x:Mpqf.t) = {
    cst = x;
    lterm = [];
  }

  let zero = cst (Mpqf.of_int 0)
  let one = cst (Mpqf.of_int 1)

  let rec add_lterm l1 l2 = match (l1,l2) with
    | (l,[])
    | ([],l)
      -> l
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	if (v1<v2) then
	  (c1,v1)::(add_lterm r1 l2)
	else if (v1>v2) then
	  (c2,v2)::(add_lterm l1 r2)
	else
	  let c = Mpqf.add c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,v1)::(add_lterm r1 r2)
	  else add_lterm r1 r2

  let scale f e =
    if (Mpqf.sgn f) = 0 then {
      cst = Mpqf.of_int 0;
      lterm = [];
    }
    else {
      cst = Mpqf.mul f e.cst;
      lterm = List.map (fun (c,v) -> (Mpqf.mul f c,v)) e.lterm
    }

  let negate_lterm l =
    List.map (fun (c,v) -> (Mpqf.neg c, v)) l

  let negate e = {
    cst = Mpqf.neg e.cst;
    lterm = negate_lterm e.lterm;
  }

  let add e1 e2 = {
    cst = Mpqf.add e1.cst e2.cst;
    lterm = add_lterm e1.lterm e2.lterm
  }

  let rec sub_lterm l1 l2 = match (l1,l2) with
    | (l,[]) -> l
    | ([],l) -> negate_lterm l
    | ((c1,v1)::r1,(c2,v2)::r2) ->
	if (v1<v2) then
	  (c1,v1)::(sub_lterm r1 l2)
	else if (v1>v2) then
	  (Mpqf.neg c2,v2)::(sub_lterm l1 r2)
	else
	  let c = Mpqf.sub c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,v1)::(sub_lterm r1 r2)
	  else sub_lterm r1 r2

  let sub e1 e2 = {
    cst = Mpqf.sub e1.cst e2.cst;
    lterm = sub_lterm e1.lterm e2.lterm
  }

  let normalize_as_constraint e =
    if e.lterm=[] then
      raise (Constant (Mpqf.sgn e.cst))
    else
      let (factor,lterm) = reduce_list e.lterm in
	{
	  cst = Mpqf.mul factor e.cst;
	  lterm = lterm;
	}

  let to_linexpr1 (env:Apron.Environment.t) e : Apron.Linexpr1.t =
    let res = Apron.Linexpr1.make ~sparse:true env in
    let list = List.map
      (fun (mpqf,name) -> 
	(Apron.Coeff.s_of_mpqf mpqf,Apron.Var.of_string name))
      e.lterm
    in
    Apron.Linexpr1.set_list res list
      (if (Mpqf.sgn e.cst)=0 
      then None else 
	Some(Apron.Coeff.s_of_mpqf e.cst))
    ;
    res

end

(*  ==================================================================== *)
(** {3 Polynomial expressions} *)
(*  ==================================================================== *)

module Poly = struct

  type varexp = string * int
  type monomial = varexp list

  type term = Mpqf.t * monomial

  type t = term list

  let is_dependent_on_integer_only typ_of_var (e:t) =
    List.for_all
      (begin fun (c,mon) ->
	List.for_all
	(begin fun (var,exp) ->
	  (typ_of_var var) = `Int
	end)
	mon
      end)
      e

  let print_varexp fmt (var,exp) =
    pp_print_string fmt var;
    if exp<>1 then
      fprintf fmt "^%i" exp;
    ()

  let print_monomial fmt lvarexp =
    Print.list ~first:"" ~sep:"." ~last:""
      print_varexp
      fmt
      lvarexp

  let print fmt expr =
    let first = ref true in
    fprintf fmt "@[";
    if expr<>[] then begin
      List.iter
	(begin fun (coeff,mon) ->
	  let sgn = Mpqf.sgn coeff in
	  if sgn <> 0 then begin
	    if sgn>0 then begin
	      if not !first then Format.pp_print_string fmt "+";
	    end;
	    if not !first then Format.fprintf fmt "@,";
	    if Mpqf.cmp_int coeff (-1) = 0 then
	      Format.pp_print_string fmt "-"
	    else if Mpqf.cmp_int coeff 1  <> 0 then
	      Mpqf.print fmt coeff
	    ;
	    print_monomial fmt mon;
	    first := false;
	  end
	end)
	expr
    end
    else
      pp_print_string fmt "0"
    ;
    fprintf fmt "@]";
    ()

  let compare_varexp (v1,n1) (v2,n2) =
    let res = String.compare v1 v2 in
    if res<>0
    then res
    else n1-n2

  let rec compare_monomial m1 m2 = match (m1,m2) with
    | (v1::r1,v2::r2) ->
	let res = compare_varexp v1 v2 in
	if res<>0
	then res
	else compare_monomial r1 r2
    | (v1::r1,[]) -> 1
    | ([],v1::r1) -> -1
    | ([],[]) -> 0

  let normalize_monomial m =
    let nm = List.filter
      (begin fun (v,n) ->
	if n<0
	then raise Exit
	else n>0
      end)
      m
    in
    List.stable_sort compare_varexp nm

  let normalize e =
    let ne = List.filter (fun (c,m) -> Mpqf.sgn c <> 0) e in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial m1 m2) ne

  let normalize_full e =
    let ne = List.filter (fun (c,m) -> Mpqf.sgn c <> 0) e in
    let ne2 = List.map (fun (c,m)  -> (c,normalize_monomial m)) ne in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial m1 m2) ne2

  let substitute_by_var (e:t) (substitution:(string,string) PMappe.t) : t
    =
    let rename_varexp ((var,exp) as varexp) : varexp =
      try (PMappe.find var substitution, exp)
      with Not_found -> varexp
    in
    let rename_monomial (monomial:monomial) : monomial =
      let res = List.map rename_varexp monomial in
      normalize_monomial res
    in
    let rename_term (c,mon) : term = (c,rename_monomial mon)
    in
    let res = List.map rename_term e in
    List.stable_sort (fun (c1,m1) (c2,m2) -> compare_monomial m1 m2) res

  let support (e:t) : string PSette.t =
    List.fold_left
      (begin fun res (_,monomial) ->
	List.fold_left
	(begin fun res (var,_) ->
	  PSette.add var res
	end)
	res
	monomial
      end)
      (PSette.empty String.compare)
      e

  let compare l1 l2 =
    let rec compare l1 l2 = match (l1,l2) with
      | ((c1,m1)::r1,(c2,m2)::r2) ->
	  let s = compare_monomial m1 m2 in
	  if s<0 then
	    Mpqf.sgn c1
	  else if s>0 then
	    -(Mpqf.sgn c2)
	  else
	    let s = Mpqf.cmp c1 c2 in
	    if s=0 then compare r1 r2 else s
      | ((c1,m1)::r1,[]) ->
	  let s = Mpqf.sgn c1 in
	  if s=0 then failwith "";
	  s
      | ([],(c1,m1)::r1) ->
	  let s = Mpqf.sgn c1 in
	  if s=0 then failwith "";
	  -s
      | ([],[]) -> 0
    in
    let (cst1,l1) = match l1 with
      | (c,[])::r -> (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let (cst2,l2) = match l2 with
      | (c,[])::r -> (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let res = compare l1 l2 in
    if res=0 then
      let res = Mpqf.cmp cst1 cst2 in
      if res>0 then 1 else if res < 0 then -1 else 0
    else
      if res>0 then 2 else if res < 0 then -2 else 0

  let cst x = [(x,[])]
  let var x = [(Mpqf.of_int 1),[(x,1)]]

  let negate e =
    List.map (fun (c,m) -> (Mpqf.neg c, m)) e

  let rec add l1 l2 = match (l1,l2) with
    | (l,[])
    | ([],l)
      -> l
    | ((c1,m1)::r1,(c2,m2)::r2) ->
	let cmp = compare_monomial m1 m2 in
	if cmp<0 then
	  (c1,m1)::(add r1 l2)
	else if cmp>0 then
	  (c2,m2)::(add l1 r2)
	else
	  let c = Mpqf.add c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,m1)::(add r1 r2)
	  else add r1 r2

  let rec sub l1 l2 = match (l1,l2) with
    | (l,[]) -> l
    | ([],l) -> negate l
    | ((c1,m1)::r1,(c2,m2)::r2) ->
	let cmp = compare_monomial m1 m2 in
	if cmp<0 then
	  (c1,m1)::(sub r1 l2)
	else if cmp>0 then
	  (c2,m2)::(sub l1 r2)
	else
	  let c = Mpqf.sub c1 c2 in
	  if (Mpqf.sgn c)<> 0
	  then (c,m1)::(sub r1 r2)
	  else sub r1 r2

  let rec mul_monomial m1 m2 = match (m1,m2) with
    | ([],l)
    | (l,[]) ->
	l
    | ((v1,n1)::r1,(v2,n2)::r2) ->
	if v1<v2 then (v1,n1)::(mul_monomial r1 m2)
	else if v1>v2 then (v2,n2)::(mul_monomial m1 r2)
	else (v1,n1+n2)::(mul_monomial r1 r2)

  let scale ((coeff,mon):Mpqf.t*monomial) (l:t) =
    if (Mpqf.sgn coeff) = 0 then []
    else begin
      let res =
	List.map
	  (fun (c,m) -> (Mpqf.mul coeff c, mul_monomial mon m))
	  l
      in
      normalize res
    end

  let mul (l1:t) (l2:t) =
    List.fold_left
      (begin fun res cm ->
	add res (scale cm l2)
      end)
      [] l1

  let div (l1:t) (l2:t) =
    match l2 with
    | [] -> failwith "Num.Poly.div: division by zero"
    | [(c,m)] ->
	let res = scale (c,List.map (fun (v,n) -> (v,-n)) m) l1 in
	normalize_full res
    | _ -> raise Exit

  let normalize_as_constraint l =
    let (cst,l) = match l with
      | [] -> raise (Constant 0)
      | (c,[])::r ->
	  if r=[] then
	    raise (Constant (Mpqf.sgn c))
	  else
	    (c,r)
      | _ as l -> (Mpqf.of_int 0, l)
    in
    let (factor,nl) = reduce_list l in
    if Mpqf.sgn cst=0 then
      nl
    else
      (Mpqf.mul factor cst,[])::nl

end

(*  ==================================================================== *)
(** {3 Tree expressions} *)
(*  ==================================================================== *)

module Tree = struct
  type unop = Apron.Texpr1.unop =
	      | Neg
	      | Cast
	      | Sqrt
  (** Binary operators *)
  type binop = Apron.Texpr1.binop =
	       | Add
	       | Sub
	       | Mul
	       | Div
	       | Mod

  (** Destination type for rounding *)
  type typ = Apron.Texpr1.typ =
	     | Real
	     | Int
	     | Single
	     | Double
	     | Extended
	     | Quad
  (** Rounding direction *)
  type round = Apron.Texpr1.round =
	       | Near
	       | Zero
	       | Up
	       | Down
	       | Rnd

  type t = Apron.Texpr1.expr =
    | Cst of Apron.Coeff.t
    | Var of Apron.Var.t
    | Unop of unop * t * typ * round
    | Binop of binop * t * t * typ * round

  let is_zero = function
    | Cst(coeff) when Apron.Coeff.is_zero coeff -> true
    | _ -> false

  let equal_int t b = match t with
    | Cst(coeff) when Apron.Coeff.equal_int coeff b -> true
    | _ -> false
	
  let is_exact = function
    | Cst _ | Var _ -> true
    | Unop(Neg,_,_,_) -> true
    | Unop(_,_,Real,_) -> true
    | Binop(_,_,_,Real,_) -> true
    | Binop(Add,_,_,Int,_) -> true
    | Binop(Sub,_,_,Int,_) -> true
    | Binop(Mul,_,_,Int,_) -> true
    | _ -> false

  let negate = function
    | Unop(Neg,e,_,_) -> e 
    | _ as e -> Unop(Neg,e,Real,Rnd)

  let add ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then e2 
    else if is_zero e2 then e1 
    else Binop(Add,e1,e2,typ,round)

  let sub ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then negate e2 
    else if is_zero e2 then e1 
    else Binop(Sub,e1,e2,typ,round)

  let mul ?(typ=Real) ?(round=Rnd) e1 e2 =
    if is_zero e1 then e1
    else if is_zero e2 then e2 
    else if equal_int e1 1 then e2
    else if equal_int e2 1 then e1
    else if equal_int e1 (-1) then negate e2
    else if equal_int e2 (-1) then negate e1
    else Binop(Mul,e1,e2,typ,round)

  let div ?(typ=Real) ?(round=Rnd) e1 e2 =
    if equal_int e2 1 then e1
    else if equal_int e2 (-1) then negate e1
    else Binop(Div,e1,e2,typ,round)

  let rec support = function
    | Cst _ -> (PSette.empty String.compare)
    | Var(var) -> PSette.singleton String.compare (Apron.Var.to_string var)
    | Unop(op,e,_,_) -> support e
    | Binop(op,e1,e2,_,_) -> PSette.union (support e1) (support e2)

  let substitute_by_var e (substitution:(string,string) PMappe.t) =
    let rec parcours = function
    | Cst _ as x -> x
    | Var(var) as x ->
	begin
	  try
	    let name = Apron.Var.to_string var in
	    let name2 = PMappe.find name substitution in
	    Var(Apron.Var.of_string name2)
	  with Not_found -> x
	end
    | Unop(op,e,t,r) -> Unop(op,parcours e, t,r)
    | Binop(op,e1,e2,t,r) -> Binop(op,parcours e1, parcours e2, t, r)
    in
    parcours e

  let print = Apron.Texpr1.print_expr

  let compare x y = 2 * (Pervasives.compare x y)
end

(*  ==================================================================== *)
(** {3 Conversions} *)
(*  ==================================================================== *)

let rec lin_of_poly (p:Poly.t) : Lin.t = match p with
  | (c,m)::r ->
      let lexpr = begin match m with
	| [] -> Lin.cst c
	| [(v,1)] -> Lin.var v
	| _ -> raise Exit
      end
      in
      Lin.add lexpr (lin_of_poly r)
  | [] -> Lin.cst (Mpqf.of_int 0)

let rec lin_of_tree (x:Tree.t) : Lin.t =
  if not (Tree.is_exact x) then raise Exit;
  match x with
  | Tree.Cst x -> 
      begin match x with
      | Apron.Coeff.Scalar (Apron.Scalar.Mpqf x) ->
	  Lin.cst x
      | _ -> raise Exit
      end
  | Tree.Var x -> Lin.var (Apron.Var.to_string x)
  | Tree.Binop(op,e1,e2,t,r) ->
      let l1 = lin_of_tree e1 in
      let l2 = lin_of_tree e2 in
      begin match op with
      | Tree.Add -> Lin.add l1 l2
      | Tree.Sub -> Lin.sub l1 l2
      | Tree.Mul ->
	  if l1.Lin.lterm=[] then
	    Lin.scale l1.Lin.cst l2
	  else if l2.Lin.lterm=[] then
	    Lin.scale l2.Lin.cst l1
	  else
	    raise Exit
      | Tree.Div ->
	  if l2.Lin.lterm=[] then
	    Lin.scale (Mpqf.inv l2.Lin.cst) l1
	  else
	    raise Exit
      | Tree.Mod ->
	  if l1.Lin.lterm=[] && l2.Lin.lterm=[] &&
	    (Mpzf.cmp_int (Mpqf.get_den l1.Lin.cst) 1) = 0 &&
	    (Mpzf.cmp_int (Mpqf.get_den l2.Lin.cst) 1) = 0 then
	      Lin.cst
		(Mpqf.of_mpzf
		  (Mpzf.gmod
		    (Mpqf.get_num l1.Lin.cst)
		    (Mpqf.get_num l1.Lin.cst)))
	  else
	    raise Exit
      end
  | _ -> raise Exit

let rec poly_of_tree (x:Tree.t) : Poly.t =
  if not (Tree.is_exact x) then raise Exit;
  match x with
  | Tree.Cst x -> 
      begin match x with
      | Apron.Coeff.Scalar (Apron.Scalar.Mpqf x) ->
	  Poly.cst x
      | _ -> raise Exit
      end
  | Tree.Var x -> Poly.var (Apron.Var.to_string x)
  | Tree.Binop(op,e1,e2,t,r) ->
      let l1 = poly_of_tree e1 in
      let l2 = poly_of_tree e2 in
      begin match op with
      | Tree.Add -> Poly.add l1 l2
      | Tree.Sub -> Poly.sub l1 l2
      | Tree.Mul -> Poly.mul l1 l2
      | Tree.Div -> Poly.div l1 l2
      | _ -> raise Exit
      end
  | _ -> raise Exit

let tree_of_lin (lin:Lin.t) : Tree.t =
  List.fold_left
    (begin fun res (c,v) ->
      Tree.add 
	res
	(Tree.mul 
	  (Tree.Cst (Apron.Coeff.s_of_mpqf c))
	  (Tree.Var(Apron.Var.of_string v)))
    end)
    (Tree.Cst (Apron.Coeff.s_of_mpqf lin.Lin.cst))
    lin.Lin.lterm

let tree_of_poly (poly:Poly.t) : Tree.t =
  let tree_of_monomial (mon:Poly.monomial) : Tree.t =
    List.fold_left
      (begin fun res (var,exp) ->
	let t = Tree.Var (Apron.Var.of_string var) in
	let res1 = ref res in
	for i=1 to exp do
	  res1 := Tree.mul t !res1;
	done;
	!res1
      end)
      (Tree.Cst (Apron.Coeff.s_of_int 1))
      mon
  in
  List.fold_left
    (begin fun res (c,mon) ->
      Tree.add res
	(Tree.mul 
	  (Tree.Cst(Apron.Coeff.s_of_mpqf c))
	  (tree_of_monomial mon))
    end)
    (Tree.Cst (Apron.Coeff.s_of_int 0))
    poly

(*  ********************************************************************** *)
(** {2 General expressions and operations} *)
(*  ********************************************************************** *)

type t =
  | Lin of Lin.t
  | Poly of Poly.t
  | Tree of Tree.t
type expr = t

let is_dependent_on_integer_only typ_of_var expr = match expr with
  | Lin e -> Lin.is_dependent_on_integer_only typ_of_var e
  | Poly e -> Poly.is_dependent_on_integer_only typ_of_var e
  | _ -> false

let print fmt expr = match expr with
  | Lin x -> Lin.print fmt x
  | Poly x -> Poly.print fmt x
  | Tree x -> Tree.print fmt x

let print_typ fmt (typ:[>typ]) =
  pp_print_string fmt
    begin match typ with
    | `Int -> "int"
    | `Real -> "real"
    | _ -> "unknown type"
    end

let normalize expr =
  match expr with
  | Lin _ -> expr
  | Poly p ->
      begin try
	Lin(lin_of_poly p)
      with Exit ->
	expr
      end
  | Tree t ->
      begin try
	let p = poly_of_tree t in
	begin try
	  let l = lin_of_poly p in
	  Lin l
	with Exit ->
	  Poly p
	end
      with Exit ->
	expr
      end

let var typ_of_var x =
  match typ_of_var x with
  | #typ -> Lin(Lin.var x)
  | _ ->
      failwith
      (Print.sprintf "Arith.var: reference %s undeclared or of wrong type in environment"
	x)

let zero = Lin(Lin.zero)
let one = Lin(Lin.one)

let cst (x:Apron.Coeff.t) = 
  match x with
  | Apron.Coeff.Scalar (Apron.Scalar.Mpqf x) ->
      Lin(Lin.cst x)
  | _ ->
      Tree(Tree.Cst x)


let to_tree = function
  | Lin l -> tree_of_lin l
  | Poly p -> tree_of_poly p
  | Tree t -> t

let add ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      match (e1,e2) with
      | (Lin e1, Lin e2) -> Lin(Lin.add e1 e2)
      | (Poly e1, Poly e2) -> Poly(Poly.add e1 e2)
      | _ -> raise Exit
    end
  with Exit ->
    normalize (Tree(Tree.Binop(Tree.Add, to_tree e1, to_tree e2,typ,round)))

let sub ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      match (e1,e2) with
      | (Lin e1, Lin e2) -> Lin(Lin.sub e1 e2)
      | (Poly e1, Poly e2) -> Poly(Poly.sub e1 e2)
      | _ -> raise Exit
    end
  with Exit ->
    normalize (Tree(Tree.Binop(Tree.Sub, to_tree e1, to_tree e2,typ,round)))

let mul ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try
    begin
      if typ<>Apron.Texpr1.Real then raise Exit;
      begin match typ with
      | Apron.Texpr1.Real -> ()
      | _ -> raise Exit
      end;
      match (e1,e2) with
      | (Lin e1, Lin e2) when e1.Lin.lterm=[] -> Lin(Lin.scale e1.Lin.cst e2)
      | (Lin e1, Lin e2) when e2.Lin.lterm=[] -> Lin(Lin.scale e2.Lin.cst e1)
      | (Poly e1, Poly e2) -> Poly(Poly.mul e1 e2)
      | _ -> raise Exit
    end
  with Exit ->
    normalize (Tree(Tree.Binop(Tree.Mul, to_tree e1, to_tree e2,typ,round)))

let div ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  try begin
    if typ<>Apron.Texpr1.Real then raise Exit;
    match (e1,e2) with
    | (Lin e1, Lin e2) when e2.Lin.lterm=[] -> Lin(Lin.scale (Mpqf.inv e2.Lin.cst) e1)
    | (Poly e1, Poly e2) -> Poly(Poly.div e1 e2)
    | _ -> raise Exit
  end
  with Exit ->
    normalize (Tree(Tree.Binop(Tree.Div, to_tree e1, to_tree e2,typ,round)))

let gmod ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e1 e2 =
  normalize (Tree(Tree.Binop(Tree.Mod, to_tree e1, to_tree e2,typ,round)))

let negate e = match e with
  | Lin l -> Lin(Lin.negate l)
  | Poly p -> Poly(Poly.negate p)
  | Tree t -> Tree(Tree.Binop(Tree.Sub,(Tree.Cst (Apron.Coeff.s_of_int 0)),t,Apron.Texpr1.Real, Apron.Texpr1.Rnd))

let cast ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  if typ=Apron.Texpr1.Real then
    e
  else
    Tree(Tree.Unop(Tree.Cast, to_tree e,typ,round))

let sqrt ?(typ=Apron.Texpr1.Real) ?(round=Apron.Texpr1.Rnd) e =
  Tree(Tree.Unop(Tree.Sqrt, to_tree e,typ,round))

let substitute_by_var e (substitution:(string,string) PMappe.t) =
  match e with
  | Lin l -> Lin(Lin.substitute_by_var l substitution)
  | Poly p -> Poly(Poly.substitute_by_var p substitution)
  | Tree t -> Tree(Tree.substitute_by_var t substitution)

let support = function
  | Lin l -> Lin.support l
  | Poly p -> Poly.support p
  | Tree t -> Tree.support t

(* Assume normalized expressions *)
let equal e1 e2 = (e1=e2)

(* Better with normalized expressions *)
let hash = Hashtbl.hash

(* Assume normalized expressions *)
let compare e1 e2 =  match (e1,e2) with
  | (Lin e1, Lin e2) -> Lin.compare e1 e2
  | (Lin _,_) -> -2
  | (Poly _, Lin _) -> 2
  | (Poly e1, Poly e2) -> Poly.compare e1 e2
  | (Poly _, Tree _) -> -2
  | (Tree e1, Tree e2) ->
      let s = Tree.compare e1 e2 in
      if s<0 then -2 else if s>0 then 2 else 0
  | (Tree _, _) -> 2

(* Assume normalized expressions *)
let normalize_as_constraint expr = match expr with
  | Lin e -> Lin (Lin.normalize_as_constraint e)
  | Poly e -> Poly (Poly.normalize_as_constraint e)
  | _ -> expr

let typ_of_expr typ_of_var expr =
  if is_dependent_on_integer_only typ_of_var expr then `Int else `Real

let to_texpr1 (env:Apron.Environment.t) expr =
  Apron.Texpr1.of_expr env (to_tree expr)

let to_texpr0 (env:Apron.Environment.t) expr =
  let texpr1 = to_texpr1 env expr in
  texpr1.Apron.Texpr1.texpr0

let to_apron (env:Apron.Environment.t) expr = match expr with
   | Lin e -> `Linexpr1 (Lin.to_linexpr1 env e)
   | _ -> `Texpr1 (to_texpr1 env expr)

let extract_cst expr =
  match expr with
  | Lin e -> e.Lin.cst
  | Poly e ->
      begin match e with
      | (c,[])::_ -> c
      | _ -> Mpqf.of_int 0
      end
  | _ -> raise (Invalid_argument "")

let extract_fstcoeff expr = 
  match expr with
  | Lin e ->  
      if e.Lin.lterm <> [] then
	fst (List.hd e.Lin.lterm)
      else if Mpqf.sgn e.Lin.cst<>0 then
	e.Lin.cst
      else 
	Mpqf.of_int 0
  | Poly lterm ->
      begin match lterm with
      | (_,[])::term::_ 
      | term::_ ->
	  fst term
      | [] ->
	  Mpqf.of_int 0
      end
  | Tree expr ->
      let rec parcours = function
	| Tree.Cst coeff ->
	    if Apron.Coeff.is_zero coeff then None
	    else begin
	      let sgn =
		match coeff with
		| Apron.Coeff.Scalar scalar -> Apron.Scalar.sgn scalar
		| Apron.Coeff.Interval itv -> 
		    let sgn = Apron.Scalar.sgn itv.Apron.Interval.inf in
		    if sgn<>0 
		    then sgn
		    else Apron.Scalar.sgn itv.Apron.Interval.sup
	      in
	      assert(sgn<>0);
	      Some(Mpqf.of_int sgn)
	    end
	| Tree.Var _ -> None
	| Tree.Unop(_,e,_,_) -> parcours e
	| Tree.Binop(_,e1,e2,_,_) ->
	    match parcours e1 with
	    | None -> parcours e2
	    | _ as res -> res 
      in
      match parcours expr with
      | Some coeff -> coeff
      | None -> Mpqf.of_int 0

let modify_cst expr cst = match expr with
  | Lin e ->
      Lin { Lin.cst = cst; Lin.lterm = e.Lin.lterm }
  | Poly e ->
      let s = Mpqf.sgn cst in
      Poly
	begin match e with
	| (_,[])::r ->
	    if s<>0 then (cst,[])::r else r
	| _ as r ->
	    if s<>0 then (cst,[])::r else r
	end
  | _ -> raise (Invalid_argument "")

module Condition = struct
  type typ = Apron.Tcons1.typ = 
    EQ | SUPEQ | SUP | DISEQ | EQMOD of Apron.Scalar.t

  type t = typ * expr

  let print_typ fmt typ = 
    pp_print_string fmt (Apron.Lincons0.string_of_typ typ)

  let print fmt (cons:t) =
    let (typ,expr) = cons in
    fprintf fmt "%a%a0" print expr print_typ typ

  (** Assume a normalized-as-constraint expression *)
  let normalize typ_of_var (cons:t) : [ `Cond of t | `Bool of bool ]
    =
    let (typ,expr) = cons in
    let cons = match typ with
      | EQ | DISEQ ->
	  let coeff = extract_fstcoeff expr in
	  let sgn = Mpqf.sgn coeff in
	  if sgn<0 then
	    (typ,negate expr)
	  else
	    cons
      | _ -> cons
    in
    match expr with
    | Tree _ -> `Cond(cons)
    | _ ->
	if is_dependent_on_integer_only typ_of_var expr then begin
	  let cst = extract_cst expr in
	  let is_integer = (Mpzf.cmp_int (Mpqf.get_den cst) 1)=0 in
	  begin match (typ,is_integer) with
	  | (EQ,false) -> `Bool false
	  | (DISEQ,false) -> `Bool true
	  | (SUPEQ,false)
	  | (SUP,false) ->
	      let
		  ncst = Mpzf.fdiv_q (Mpqf.get_num cst) (Mpqf.get_den cst)
	      in
	      let ncst = Mpqf.of_mpzf ncst in
	      `Cond(SUPEQ,modify_cst expr ncst)
	  | (SUP,true) ->
	      let ncst = Mpqf.sub cst (Mpqf.of_int 1) in
	      `Cond(SUPEQ,modify_cst expr ncst)
	  | _ ->
	      `Cond(cons)
	  end
	end
	else
	  `Cond(cons)
	    
  let make typ_of_var typ expr =
    begin try
      let nexpr = normalize_as_constraint expr in
      normalize typ_of_var (typ,nexpr)
    with Constant sgn ->
      begin match typ with
      | EQ -> `Bool(sgn=0)
      | DISEQ -> `Bool(sgn<>0)
      | SUP -> `Bool(sgn>0)
      | SUPEQ -> `Bool(sgn>=0)
      | EQMOD m -> failwith ""
      end
    end

  let negate typ_of_var (cons:t)
    =
    let ncons =
      match cons with
      | (EQ,expr) -> `Cond (DISEQ,expr)
      | (DISEQ,expr) -> `Cond (EQ,expr)
      | (SUPEQ,expr) ->
	  normalize typ_of_var (SUP,negate expr)
      | (SUP,expr) ->
	  normalize typ_of_var (SUPEQ,negate expr)
      | (EQMOD _, _) ->
	  failwith ""
    in
    match ncons with
    | `Bool _ -> failwith ""
    | `Cond x -> x

  let support (t,e) = support e

  let compare (t1,e1) (t2,e2) : int
    =
    let sgn = compare e1 e2 in
    let asgn = abs sgn in
    match (t1,t2) with
    | (EQMOD m1, EQMOD m2) -> 
	if asgn=0 then
	  let cmp = Apron.Scalar.cmp m1 m2 in
	  if cmp > 0 then 2 else (-2)
	else
	  (if sgn>0 then 3 else -3)
    | (EQMOD _, _) ->
	3
    | (_, EQMOD _) ->
	-3
    | (_,_) -> 
	if asgn >= 2 then
	  (if sgn>0 then 3 else -3)
	else if asgn = 1 then begin 
	  match (t1,t2) with
	  | (EQMOD _, _) 
	  | (_, EQMOD _) -> failwith ""
	  | (DISEQ,_) -> 2*sgn
	  | (EQ,DISEQ) -> sgn
	  | (_,DISEQ) -> 2*sgn
	  | (_,EQ) -> 2*sgn
	  | _ -> sgn
	end
	else begin
	  if t1=t2 then 0
	  else
	    (* order SUP,EQ,DISEQ,SUPEQ *)
	    begin match (t1,t2) with
	    | (EQMOD _, _) 
	    | (_, EQMOD _) -> failwith ""

	    | (SUP,SUPEQ) -> -1
	    | (SUPEQ,SUP) -> 1
	    | (SUP,DISEQ) -> -1
	    | (DISEQ,SUP) -> 1
	    | (SUP,_) -> -2
	    | (_,SUP) -> 2

	    | (EQ,SUPEQ) -> -1
	    | (SUPEQ,EQ) -> 1
	    | (EQ,_) -> -2
	    | (_,EQ) -> 2

	    | (DISEQ,_) -> -2
	    | (_,DISEQ) -> 2
	    | _ -> assert(t1=t2); 0
	    end
	end
	  
  let to_tcons1 env (typ,expr) =
    Apron.Tcons1.make (to_texpr1 env expr) typ

  let to_tcons0 env (typ,expr) =
    Apron.Tcons0.make (to_texpr0 env expr) typ

  let to_apron env (typ,expr) =
    match expr with
    | Lin e -> `Lincons1 (Apron.Lincons1.make (Lin.to_linexpr1 env e) typ)
    | _ -> `Tcons1 (to_tcons1 env (typ,expr))

end
