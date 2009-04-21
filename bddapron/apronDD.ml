(** Decision diagrams on top of Apron abstract values *)

open Format

type 'a t = 'a Apron.Abstract1.t Cudd.Mtbdd.t
type 'a table = 'a Apron.Abstract1.t Cudd.Mtbdd.table

type 'a global = {
  op_is_leq : (('a Apron.Abstract1.t, 'a Apron.Abstract1.t) Cudd.Mtbdd.test2,  Cudd.Mtbdd.global) Cudd.Mtbdd.op;
  op_join : (('a Apron.Abstract1.t, 'a Apron.Abstract1.t, 'a Apron.Abstract1.t) Cudd.Mtbdd.op2, Cudd.Mtbdd.global) Cudd.Mtbdd.op;
  op_meet : (('a Apron.Abstract1.t, 'a Apron.Abstract1.t, 'a Apron.Abstract1.t) Cudd.Mtbdd.op2, Cudd.Mtbdd.global) Cudd.Mtbdd.op;
  op_exist : (('a Apron.Abstract1.t, Cudd.Mtbdd.global) Cudd.Mtbdd.exist, Cudd.Mtbdd.global) Cudd.Mtbdd.op;
}
type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}

let make_table apron : 'a table =
  Cudd.PWeakke.create
    Hashtbl.hash
    (fun (x:'a Apron.Abstract1.t) (y:'a Apron.Abstract1.t) ->
      x==y ||
	(Apron.Environment.equal
	  x.Apron.Abstract1.env
	  y.Apron.Abstract1.env
	&&
	  Apron.Abstract1.is_eq apron x y
	)
    )
    23

let neutral_join x =
  let apron =  Apron.Abstract1.manager x in
  Apron.Abstract1.is_bottom apron x

(*
let absorbant_meet xu =
  let x = Cudd.Mtbdd.get xu in
  let apron = Apron.Abstract1.manager x in
  if Apron.Abstract1.is_bottom apron x then Some xu else None
*)

let special_is_leq apron dd1 dd2 =
  if Cudd.Mtbdd.is_cst dd1 && Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd1) then
    Some true 
  else if Cudd.Mtbdd.is_cst dd2 && Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd2) then 
    Some false 
  else None

let special_join apron dd1 dd2 =
  if Cudd.Mtbdd.is_cst dd1 && Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd1) then
    Some dd2
  else if Cudd.Mtbdd.is_cst dd2 &&  Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd2) then 
    Some dd1 
  else
    None

let special_meet apron dd1 dd2 =
  if Cudd.Mtbdd.is_cst dd1 && Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd1) then
    Some dd1
  else if Cudd.Mtbdd.is_cst dd2 &&  Apron.Abstract1.is_bottom apron (Cudd.Mtbdd.dval dd2) then 
    Some dd2 
  else
    None

let make_global (apron:'a Apron.Manager.t) (table:'a table) : 'a global =
  let op_is_leq =
    Cudd.Mtbdd.register_test2
      ~cachetyp:Cudd.Mtbdd.global
      ~commutative:false ~reflexive:true
      ~special:(special_is_leq apron)
      (fun x y -> (Apron.Abstract1.is_leq apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
  in
  let op_join =
    Cudd.Mtbdd.register_op2
      ~cachetyp:Cudd.Mtbdd.global
      ~commutative:true ~idempotent:true
      ~special:(special_join apron)
      (fun x y -> Cudd.Mtbdd.unique table (Apron.Abstract1.join apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
  in
  let op_meet =
    Cudd.Mtbdd.register_op2
      ~cachetyp:Cudd.Mtbdd.global
      ~commutative:true ~idempotent:true
      ~special:(special_meet apron)
      (fun x y -> Cudd.Mtbdd.unique table (Apron.Abstract1.meet apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
  in
  let op_exist =
    Cudd.Mtbdd.register_exist
      ~cachetyp:Cudd.Mtbdd.global
      op_join
  in {
    op_is_leq = op_is_leq;
    op_join = op_join;
    op_meet = op_meet;
    op_exist = op_exist
  }

let make_man ?(global=false) apron =
  let table = make_table apron in
  let oglobal =
    if global then
      Some(make_global apron table)
    else
      None
  in {
    apron = apron;
    table = table;
    oglobal = oglobal
  }

let bottom_of_abs man (mtbdd:'a t) : 'a Apron.Abstract1.t =
  let abs = Cudd.Mtbdd.pick_leaf mtbdd in
  Apron.Abstract1.bottom man.apron (abs.Apron.Abstract1.env)

let print print_bdd fmt (t:'a t) =
  if Cudd.Mtbdd.is_cst t then
    Apron.Abstract1.print fmt (Cudd.Mtbdd.dval t)
  else
    let nb = Cudd.Mtbdd.nbpaths t in
    if nb > (float_of_int !Cudd.Man.print_limit) then
      fprintf fmt "idd with %i nodes, %i leaves and %g paths"
	(Cudd.Mtbdd.size t) (Cudd.Mtbdd.nbleaves t) nb
    else begin
      let leaves_u = Cudd.Mtbdd.leaves_u t in
      assert((Array.length leaves_u) >= 2);
      let first = ref true in
      fprintf fmt "{ @[<v>";
      for i=Array.length leaves_u - 1 downto 0 do
	let leaf_u = leaves_u.(i) in
	let leaf = Cudd.Mtbdd.get leaf_u in
	if not (Apron.Abstract1.is_bottom (Apron.Abstract1.manager leaf) leaf) then begin
	  if !first then first := false else fprintf fmt " or@,";
	  let bdd = Cudd.Mtbdd.guard_of_leaf_u t leaf_u in
	  fprintf fmt "@[<hv>(%a) and@ %a@]"
	    print_bdd bdd Apron.Abstract1.print leaf;
	end
      done;
      fprintf fmt "@] }"
    end


let cst ~cudd man abs : 'a t =
  Cudd.Mtbdd.cst cudd man.table abs

let bottom ~cudd man env : 'a t =
  cst cudd man (Apron.Abstract1.bottom man.apron env)
let top ~cudd man env : 'a t =
  cst cudd man (Apron.Abstract1.top man.apron env)
let is_bottom man (x:'a t) =
  Cudd.Mtbdd.is_cst x && Apron.Abstract1.is_bottom man.apron (Cudd.Mtbdd.dval x)
let is_top man (x:'a t)  =
  Cudd.Mtbdd.is_cst x && Apron.Abstract1.is_top man.apron (Cudd.Mtbdd.dval x)

let is_eq (man:'a man) =
  Cudd.Mtbdd.is_equal

let is_leq man (x:'a t) (y:'a t) =
  match man.oglobal with
  | None ->
      Cudd.Mtbdd.map_test2
	~commutative:false ~reflexive:true
	~special:(special_is_leq man.apron)
	(fun x y -> Apron.Abstract1.is_leq man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y))
	x y
  | Some global ->
      Cudd.Mtbdd.apply_test2 global.op_is_leq x y

let join man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.Mtbdd.map_op2
	~commutative:true ~idempotent:true
	~special:(special_join man.apron)
	(fun x y -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.join man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
	x y
  | Some global ->
      Cudd.Mtbdd.apply_op2 global.op_join x y

let meet man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.Mtbdd.map_op2
	~commutative:true ~idempotent:true
	~special:(special_meet man.apron)
	(fun x y -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.meet man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
	x y
  | Some global ->
      Cudd.Mtbdd.apply_op2 global.op_meet x y

let widening man (x:'a t) (y:'a t) : 'a t  =
  Cudd.Mtbdd.map_op2
    ~commutative:false ~idempotent:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbdd.is_cst dd1 && Apron.Abstract1.is_bottom man.apron (Cudd.Mtbdd.dval dd1) 
      then Some dd2 else None
    )
    (fun x y -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.widening man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y)))
    x y

let meet_tcons_array man (x:'a t) tcons :'a t =
  Cudd.Mtbdd.map_op1
    (fun x -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.meet_tcons_array man.apron (Cudd.Mtbdd.get x) tcons))
    x

let forget_array man (x:'a t) (tvar:Apron.Var.t array) : 'a t =
  Cudd.Mtbdd.map_op1
    (fun x -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.forget_array man.apron (Cudd.Mtbdd.get x) tvar false))
    x

let change_environment man (x:'a t) nenv : 'a t =  
  Cudd.Mtbdd.map_op1
    (fun x -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.change_environment man.apron (Cudd.Mtbdd.get x) nenv false))
    x

let rename_array man (x:'a t) tvar1 tvar2 : 'a t =
  let env =
    let abs = Cudd.Mtbdd.pick_leaf x in
    abs.Apron.Abstract1.env
  in
  let (nenv,perm) = Apron.Environment.rename_perm env tvar1 tvar2 in
  Cudd.Mtbdd.map_op1
    (fun xu -> 
      let x = Cudd.Mtbdd.get xu in
      let abs0 = x.Apron.Abstract1.abstract0 in
      let nabs0 = Apron.Abstract0.permute_dimensions man.apron abs0 perm in
      let y = { Apron.Abstract1.env = nenv; Apron.Abstract1.abstract0 = nabs0 } in
      let yu = Cudd.Mtbdd.unique man.table y in
      yu
    )
    x

let asssub_texpr
    (asssub : 'a Apron.Manager.t -> 'a Apron.Abstract1.t -> Apron.Var.t array -> Apron.Texpr1.t array -> 'a Apron.Abstract1.t option -> 'a Apron.Abstract1.t)
    man
    (x:'a t) (var:Apron.Var.t) (expr:ApronexprDD.t)
    (odest:'a t option)
    : 'a t
    =
  let env =
    let abs = Cudd.Mtbdd.pick_leaf x in
    abs.Apron.Abstract1.env
  in
  let varexpr = Apronexpr.Lin(Apronexpr.Lin.var (Apron.Var.to_string var)) in
  match odest with
  | None ->
      Cudd.Mtbdd.map_op2
	~special:(fun ddx ddexpr ->
	  if Cudd.Mtbdd.is_cst ddx then
	    if Apron.Abstract1.is_bottom man.apron (Cudd.Mtbdd.dval ddx) then Some ddx else None
	  else
	    if Apronexpr.equal varexpr (Cudd.Mtbdd.dval ddexpr) then Some ddx else None
	)
	(fun x expr ->
	  let x = Cudd.Mtbdd.get x in
	  let expr = Cudd.Mtbdd.get expr in
	  let texpr = Apronexpr.to_texpr1 env expr in
	  Cudd.Mtbdd.unique man.table (
	    asssub man.apron x [|var|] [|texpr|] None
	  )
	)
	x expr
  | Some y ->
      Cudd.Mtbdd.map_op3
	~special:(fun ddx ddy ddexpr ->
	  if Cudd.Mtbdd.is_cst ddx && Apron.Abstract1.is_bottom man.apron (Cudd.Mtbdd.dval ddx) then Some ddx
	  else if Cudd.Mtbdd.is_cst ddy && Apron.Abstract1.is_bottom man.apron (Cudd.Mtbdd.dval ddy) then Some ddy
	  else 
	    None
	)
	(fun x y expr ->
	  let x = Cudd.Mtbdd.get x in
	  let y = Cudd.Mtbdd.get y in
	  let expr = Cudd.Mtbdd.get expr in
	  let texpr = Apronexpr.to_texpr1 env expr in
	  Cudd.Mtbdd.unique man.table (
	    asssub man.apron x [|var|] [|texpr|] (Some y)
	  )
	)
	x y expr

let asssub_texpr_array
    ?(asssub_bdd : (Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t) option)
    (asssub : 'a Apron.Manager.t -> 'a Apron.Abstract1.t -> Apron.Var.t array -> Apron.Texpr1.t array -> 'a Apron.Abstract1.t option -> 'a Apron.Abstract1.t)
    (man:'a man)
    (x:'a t) (tvar:Apron.Var.t array) (texpr:ApronexprDD.t array)
    (odest:'a t option)
    : 'a t
    =
  if (Array.length tvar) = 1 && asssub_bdd = None then
    asssub_texpr asssub man x tvar.(0) texpr.(0) odest
  else begin
    let cudd = Cudd.Mtbdd.manager x in
    let bottom = bottom_of_abs man x in
    let bottom_u = Cudd.Mtbdd.unique man.table bottom in
    let default = Cudd.Mtbdd.cst_u cudd bottom_u in
    let env = bottom.Apron.Abstract1.env in
    let combine = fun (bdd,res1) res2 ->
      join man (Cudd.Mtbdd.ite bdd res1 default) res2
    in
    let absorbant = neutral_join in
    let tabsorbant = Array.create (Array.length texpr) None in
    match odest with
    | Some(y) when asssub_bdd=None ->
	Cudd.Mtbdd.combineleaf2_array
	  ~default ~combine
	  ~absorbant1:absorbant ~absorbant2:absorbant ~tabsorbant
	  (fun guard x y texpr ->
	    let nleaf =
	      if texpr=[||] then x
	      else
		let x = Cudd.Mtbdd.get x in
		let y = Cudd.Mtbdd.get y in
		let texpr =
		  Array.map
		    (fun expr -> Apronexpr.to_texpr1 env (Cudd.Mtbdd.get expr))
		    texpr
		in
		Cudd.Mtbdd.unique man.table (asssub man.apron x tvar texpr (Some y))
	    in
	    (guard,Cudd.Mtbdd.cst_u cudd nleaf)
	  )
	  x y texpr
    | _ ->
	Cudd.Mtbdd.combineleaf1_array
	  ~default ~combine ~absorbant:absorbant ~tabsorbant
	  (begin fun guard x texpr ->
	    let nguard = match asssub_bdd with
	      | None -> guard
	      | Some f -> f guard
	    in
	    if Cudd.Bdd.is_false nguard then (nguard,default)
	    else if texpr=[||] then
	      let ncst = Cudd.Mtbdd.cst_u cudd x in
	      match odest with
	      | None ->
		  (nguard, ncst)
	      | Some y ->
		  let res =
		    meet man ncst (Cudd.Mtbdd.ite nguard y default)
		  in
		  (Cudd.Bdd.dtrue cudd,res)
	    else
	      let x = Cudd.Mtbdd.get x in
	      let texpr =
		Array.map
		  (fun expr -> Apronexpr.to_texpr1 env (Cudd.Mtbdd.get expr))
		  texpr
	      in
	      match odest with
	      | None ->
		  let nleaf = asssub man.apron x tvar texpr None in
		  (nguard, Cudd.Mtbdd.cst cudd man.table nleaf)
	      | Some y ->
		  let y = Cudd.Mtbdd.ite nguard y default in
		  let res =
		    Cudd.Mtbdd.mapleaf1
		      (fun y ->
			if y==bottom_u then y
			else
			  let y = Cudd.Mtbdd.get y in
			  Cudd.Mtbdd.unique man.table (
			    asssub man.apron x tvar texpr (Some y)
			  )
		      )
		      y
		  in
		  (Cudd.Bdd.dtrue cudd,res)
	  end)
	  x texpr
  end

let assign_texpr_array man =
  asssub_texpr_array ?asssub_bdd:None Apron.Abstract1.assign_texpr_array man

(* mieux: mapvectorcomposeapply, si dest vide *)
let substitute_texpr_array man =
  asssub_texpr_array ?asssub_bdd:None Apron.Abstract1.substitute_texpr_array man

let make_fun (man:'a man)
    =
  let special = Some(special_join man.apron) in
  let op = (fun x y -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.join man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y))) in
  `Fun (special,op)

let exist man ~(supp:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.Mtbdd.map_exist (make_fun man) ~supp t
  | Some global ->
      Cudd.Mtbdd.apply_exist global.op_exist ~supp t

let make_funop man bottomdd : ('a,'b) Cudd.Mtbdd.mexist
    =
  let special = 
    Some (fun dd1 dd2 -> 
      if Cudd.Mtbdd.is_equal dd1 bottomdd then Some dd2
      else if Cudd.Mtbdd.is_equal dd2 bottomdd then Some dd1
      else None
    )
  in
  let op = (fun x y -> Cudd.Mtbdd.unique man.table (Apron.Abstract1.join man.apron (Cudd.Mtbdd.get x) (Cudd.Mtbdd.get y))) in
  `Fun (special,op)

let existand (man:'a man) 
    ~(bottom:'a Apron.Abstract1.t Cudd.Mtbdd.unique) 
    ~(supp:Cudd.Man.v Cudd.Bdd.t) (guard:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t  
    =
  match man.oglobal with
  | None ->
      let bottomdd = Cudd.Mtbdd.cst_u (Cudd.Bdd.manager supp) bottom in
      Cudd.Mtbdd.map_existand ~bottom (make_funop man bottomdd) ~supp guard t
  | Some global ->
      let (mexist:('a Apron.Abstract1.t, Cudd.Mtbdd.global) Cudd.Mtbdd.mexist) =
	(`Op global.op_join)
      in
      Cudd.Mtbdd.map_existand ~bottom mexist ~supp guard t
