(** Decision diagrams on top of Apron abstract values *)

open Format
type 'a leaf = 'a Apron.Abstract0.t
type 'a t = 'a leaf Cudd.Mtbddc.t
type 'a table = 'a leaf Cudd.Mtbddc.table

type 'a leaf_u = 'a leaf Cudd.Mtbddc.unique

type 'a global = {
  op_is_leq : (('a leaf_u, 'a leaf_u) Cudd.User.test2,  Cudd.User.global) Cudd.User.op;
  op_join : (('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2, Cudd.User.global) Cudd.User.op;
  op_meet : (('a leaf_u, 'a leaf_u, 'a leaf_u) Cudd.User.op2, Cudd.User.global) Cudd.User.op;
  op_exist : (('a leaf_u, Cudd.User.global) Cudd.User.exist, Cudd.User.global) Cudd.User.op;
}
type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}

let make_table (apron:'a Apron.Manager.t) : 'a table =
  Cudd.Mtbddc.make_table
    ~hash:Hashtbl.hash
    ~equal:(fun (x:'a Apron.Abstract0.t) (y:'a Apron.Abstract0.t) ->
      x==y || Apron.Abstract0.is_eq apron x y
    )
    
let neutral_join x =
  let apron =  Apron.Abstract0.manager x in
  Apron.Abstract0.is_bottom apron x

let special_is_leq apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd1) then
    Some true 
  else if Cudd.Mtbddc.is_cst dd2 && Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd2) then 
    Some false 
  else None

let special_join apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd1) then
    Some dd2
  else if Cudd.Mtbddc.is_cst dd2 &&  Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd2) then 
    Some dd1 
  else
    None

let special_meet apron dd1 dd2 =
  if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd1) then
    Some dd1
  else if Cudd.Mtbddc.is_cst dd2 &&  Apron.Abstract0.is_bottom apron (Cudd.Mtbddc.dval dd2) then 
    Some dd2 
  else
    None

let make_global (apron:'a Apron.Manager.t) (table:'a table) : 'a global =
  let op_is_leq =
    Cudd.User.register_test2
      ~cachetyp:Cudd.User.global
      ~commutative:false ~reflexive:true
      ~special:(special_is_leq apron)
      (fun x y -> (Apron.Abstract0.is_leq apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
  in
  let op_join =
    Cudd.User.register_op2
      ~cachetyp:Cudd.User.global
      ~commutative:true ~idempotent:true
      ~special:(special_join apron)
      (fun x y -> Cudd.Mtbddc.unique table (Apron.Abstract0.join apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
  in
  let op_meet =
    Cudd.User.register_op2
      ~cachetyp:Cudd.User.global
      ~commutative:true ~idempotent:true
      ~special:(special_meet apron)
      (fun x y -> Cudd.Mtbddc.unique table (Apron.Abstract0.meet apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
  in
  let op_exist =
    Cudd.User.register_exist
      ~cachetyp:Cudd.User.global
      op_join
  in {
    op_is_leq = op_is_leq;
    op_join = op_join;
    op_meet = op_meet;
    op_exist = op_exist
  }

let make_man ?(global=false) (apron:'a Apron.Manager.t) =
  let table = make_table apron in
  let oglobal =
    if global then
      Some(make_global apron table)
    else
      None
  in 
  let man = {
    apron = apron;
    table = table;
    oglobal = oglobal
  }
  in
  man

let bottom_of_abs man (mtbdd:'a t) : 'a Apron.Abstract0.t =
  let abs = Cudd.Mtbddc.pick_leaf mtbdd in
  let dim = Apron.Abstract0.dimension man.apron abs in
  Apron.Abstract0.bottom man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald

let print print_bdd string_of_dim fmt (t:'a t) =
  if Cudd.Mtbddc.is_cst t then
    Apron.Abstract0.print string_of_dim fmt (Cudd.Mtbddc.dval t)
  else
    let nb = Cudd.Mtbddc.nbpaths t in
    if nb > (float_of_int !Cudd.Man.print_limit) then
      fprintf fmt "idd with %i nodes, %i leaves and %g paths"
	(Cudd.Mtbddc.size t) (Cudd.Mtbddc.nbleaves t) nb
    else begin
      let leaves_u = Cudd.Mtbddc.leaves_u t in
      assert((Array.length leaves_u) >= 2);
      let first = ref true in
      fprintf fmt "{ @[<v>";
      for i=Array.length leaves_u - 1 downto 0 do
	let leaf_u = leaves_u.(i) in
	let leaf = Cudd.Mtbddc.get leaf_u in
	if not (Apron.Abstract0.is_bottom (Apron.Abstract0.manager leaf) leaf) then begin
	  if !first then first := false else fprintf fmt " or@,";
	  let bdd = Cudd.Mtbddc.guard_of_leaf_u t leaf_u in
	  fprintf fmt "@[<hv>(%a) and@ %a@]"
	    print_bdd bdd (Apron.Abstract0.print string_of_dim) leaf;
	end
      done;
      fprintf fmt "@] }"
    end


let cst ~cudd man abs : 'a t =
  Cudd.Mtbddc.cst cudd man.table abs

let bottom ~cudd man dim : 'a t =
  cst cudd man 
    (Apron.Abstract0.bottom man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald)
let top ~cudd man dim : 'a t =
  cst cudd man 
    (Apron.Abstract0.top man.apron dim.Apron.Dim.intd dim.Apron.Dim.reald)
let is_bottom man (x:'a t) =
  Cudd.Mtbddc.is_cst x && Apron.Abstract0.is_bottom man.apron (Cudd.Mtbddc.dval x)
let is_top man (x:'a t)  =
  Cudd.Mtbddc.is_cst x && Apron.Abstract0.is_top man.apron (Cudd.Mtbddc.dval x)

let is_eq (man:'a man) =
  Cudd.Mtbddc.is_equal

let is_leq man (x:'a t) (y:'a t) =
  match man.oglobal with
  | None ->
      Cudd.User.map_test2
	~commutative:false ~reflexive:true
	~special:(special_is_leq man.apron)
	(fun x y -> Apron.Abstract0.is_leq man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y))
	x y
  | Some global ->
      Cudd.User.apply_test2 global.op_is_leq x y

let join man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.User.map_op2
	~commutative:true ~idempotent:true
	~special:(special_join man.apron)
	(fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.join man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
	x y
  | Some global ->
      Cudd.User.apply_op2 global.op_join x y

let meet man (x:'a t) (y:'a t) : 'a t  =
  match man.oglobal with
  | None ->
      Cudd.User.map_op2
	~commutative:true ~idempotent:true
	~special:(special_meet man.apron)
	(fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.meet man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
	x y
  | Some global ->
      Cudd.User.apply_op2 global.op_meet x y

let widening man (x:'a t) (y:'a t) : 'a t  =
  Cudd.User.map_op2
    ~commutative:false ~idempotent:true
    ~special:(fun dd1 dd2 ->
      if Cudd.Mtbddc.is_cst dd1 && Apron.Abstract0.is_bottom man.apron (Cudd.Mtbddc.dval dd1) 
      then Some dd2 else None
    )
    (fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.widening man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y)))
    x y

let meet_tcons_array man (x:'a t) tcons :'a t =
  Cudd.User.map_op1
    (fun x -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.meet_tcons_array man.apron (Cudd.Mtbddc.get x) tcons))
    x

let forget_array man (x:'a t) tdim : 'a t =
  Cudd.User.map_op1
    (fun x -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.forget_array man.apron (Cudd.Mtbddc.get x) tdim false))
    x

let permute_dimensions man (x:'a t) perm : 'a t =
  Cudd.User.map_op1
    (fun x -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.permute_dimensions man.apron (Cudd.Mtbddc.get x) perm))
    x

let add_dimensions  man (x:'a t) change project : 'a t =
  Cudd.User.map_op1
    (fun x -> 
      Cudd.Mtbddc.unique man.table (Apron.Abstract0.add_dimensions man.apron (Cudd.Mtbddc.get x) change project))
    x
let remove_dimensions man (x:'a t) change : 'a t =
  Cudd.User.map_op1
    (fun x -> 
      Cudd.Mtbddc.unique man.table (Apron.Abstract0.remove_dimensions man.apron (Cudd.Mtbddc.get x) change))
    x

let apply_dimchange2 man (x:'a t) change2 project =
  Cudd.User.map_op1
    (fun x -> 
      Cudd.Mtbddc.unique man.table (Apron.Abstract0.apply_dimchange2 man.apron (Cudd.Mtbddc.get x) change2 project))
    x

let asssub_texpr
    (asssub : 'a Apron.Manager.t -> 'a Apron.Abstract0.t -> Apron.Dim.t array -> Apron.Texpr0.t array -> 'a Apron.Abstract0.t option -> 'a Apron.Abstract0.t)
    man env
    (x:'a t) (dim:Apron.Dim.t) (expr:ApronexprDD.t)
    (odest:'a t option)
    : 'a t
    =
  let var = Apron.Environment.var_of_dim env dim in
  let varexpr = Apronexpr.Lin(Apronexpr.Lin.var (Apron.Var.to_string var)) in
  match odest with
  | None ->
      Cudd.User.map_op2
	~special:(fun ddx ddexpr ->
	  if Cudd.Mtbddc.is_cst ddx then
	    if Apron.Abstract0.is_bottom man.apron (Cudd.Mtbddc.dval ddx) then Some ddx else None
	  else
	    if Apronexpr.equal varexpr (Cudd.Mtbdd.dval ddexpr) then Some ddx else None
	)
	(fun x expr ->
	  let x = Cudd.Mtbddc.get x in
	  let expr = Cudd.Mtbdd.get expr in
	  let texpr = Apronexpr.to_texpr0 env expr in
	  Cudd.Mtbddc.unique man.table (
	    asssub man.apron x [|dim|] [|texpr|] None
	  )
	)
	x expr
  | Some y ->
      Cudd.User.map_op3
	~special:(fun ddx ddy ddexpr ->
	  if Cudd.Mtbddc.is_cst ddx && Apron.Abstract0.is_bottom man.apron (Cudd.Mtbddc.dval ddx) then Some ddx
	  else if Cudd.Mtbddc.is_cst ddy && Apron.Abstract0.is_bottom man.apron (Cudd.Mtbddc.dval ddy) then Some ddy
	  else 
	    None
	)
	(fun x y expr ->
	  let x = Cudd.Mtbddc.get x in
	  let y = Cudd.Mtbddc.get y in
	  let expr = Cudd.Mtbdd.get expr in
	  let texpr = Apronexpr.to_texpr0 env expr in
	  Cudd.Mtbddc.unique man.table (
	    asssub man.apron x [|dim|] [|texpr|] (Some y)
	  )
	)
	x y expr

let asssub_texpr_array
    ?(asssub_bdd : (Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t) option)
    (asssub : 'a Apron.Manager.t -> 'a Apron.Abstract0.t -> Apron.Dim.t array -> Apron.Texpr0.t array -> 'a Apron.Abstract0.t option -> 'a Apron.Abstract0.t)
    (man:'a man) env
    (x:'a t) (tdim:Apron.Dim.t array) (texpr:ApronexprDD.t array)
    (odest:'a t option)
    : 'a t
    =
  if (Array.length tdim) = 1 && asssub_bdd = None then
    asssub_texpr asssub man env x tdim.(0) texpr.(0) odest
  else begin
    let cudd = Cudd.Mtbddc.manager x in
    let bottom = bottom_of_abs man x in
    let bottom_u = Cudd.Mtbddc.unique man.table bottom in
    let default = Cudd.Mtbddc.cst_u cudd bottom_u in
    let combine = fun (bdd,res1) res2 ->
      join man (Cudd.Mtbddc.ite bdd res1 default) res2
    in
    let absorbant x = neutral_join (Cudd.Mtbddc.get x) in
    let tabsorbant = Array.create (Array.length texpr) None in
    match odest with
    | Some(y) when asssub_bdd=None ->
	Cudd.Mapleaf.combineleaf2_array
	  ~default ~combine
	  ~absorbant1:absorbant ~absorbant2:absorbant ~tabsorbant
	  (fun guard x y texpr ->
	    let x = Cudd.Mtbddc.get x in
	    let y = Cudd.Mtbddc.get y in
	    let nleaf =
	      if texpr=[||] then 
		Cudd.Mtbddc.unique man.table 
		  (Apron.Abstract0.meet man.apron x y)
	      else
		let texpr =
		  Array.map
		    (fun expr -> Apronexpr.to_texpr0 env (Cudd.Mtbdd.get expr))
		    texpr
		in
		Cudd.Mtbddc.unique man.table (asssub man.apron x tdim texpr (Some y))
	    in
	    (guard,Cudd.Mtbddc.cst_u cudd nleaf)
	  )
	  x y texpr
    | _ ->
	Cudd.Mapleaf.combineleaf1_array
	  ~default ~combine ~absorbant:absorbant ~tabsorbant
	  (begin fun guard x texpr ->
	    let nguard = match asssub_bdd with
	      | None -> guard
	      | Some f -> f guard
	    in
	    if Cudd.Bdd.is_false nguard then (nguard,default)
	    else if texpr=[||] then
	      let ncst = Cudd.Mtbddc.cst_u cudd x in
	      match odest with
	      | None ->
		  (nguard, ncst)
	      | Some y ->
		  let res =
		    meet man ncst (Cudd.Mtbddc.ite nguard y default)
		  in
		  (Cudd.Bdd.dtrue cudd,res)
	    else
	      let x = Cudd.Mtbddc.get x in
	      let texpr =
		Array.map
		  (fun expr -> Apronexpr.to_texpr0 env (Cudd.Mtbdd.get expr))
		  texpr
	      in
	      match odest with
	      | None ->
		  let nleaf = asssub man.apron x tdim texpr None in
		  (nguard, Cudd.Mtbddc.cst cudd man.table nleaf)
	      | Some y ->
		  let y = Cudd.Mtbddc.ite nguard y default in
		  let res =
		    Cudd.Mapleaf.mapleaf1
		      (fun y ->
			if y==bottom_u then y
			else
			  let y = Cudd.Mtbddc.get y in
			  Cudd.Mtbddc.unique man.table (
			    asssub man.apron x tdim texpr (Some y)
			  )
		      )
		      y
		  in
		  (Cudd.Bdd.dtrue cudd,res)
	  end)
	  x texpr
  end

let assign_texpr_array man =
  asssub_texpr_array ?asssub_bdd:None Apron.Abstract0.assign_texpr_array man

(* mieux: mapvectorcomposeapply, si dest vide *)
let substitute_texpr_array man =
  asssub_texpr_array ?asssub_bdd:None Apron.Abstract0.substitute_texpr_array man

let make_fun (man:'a man)
    =
  let special = Some(special_join man.apron) in
  let op = (fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.join man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y))) in
  `Fun (special,op)

let make_fun (man:'a man)
    =
  let special = Some(special_join man.apron) in
  let op = (fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.join man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y))) in
  `Fun (special,op)

let exist man ~(supp:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t =
  match man.oglobal with
  | None ->
      Cudd.User.map_exist (make_fun man) ~supp t
  | Some global ->
      Cudd.User.apply_exist global.op_exist ~supp t

let make_funop man bottomdd : ('a,'b) Cudd.User.mexist
    =
  let special = 
    Some (fun dd1 dd2 -> 
      if Cudd.Mtbddc.is_equal dd1 bottomdd then Some dd2
      else if Cudd.Mtbddc.is_equal dd2 bottomdd then Some dd1
      else None
    )
  in
  let op = (fun x y -> Cudd.Mtbddc.unique man.table (Apron.Abstract0.join man.apron (Cudd.Mtbddc.get x) (Cudd.Mtbddc.get y))) in
  `Fun (special,op)

let existand (man:'a man) 
    ~(bottom:'a Apron.Abstract0.t Cudd.Mtbddc.unique) 
    ~(supp:Cudd.Man.v Cudd.Bdd.t) (guard:Cudd.Man.v Cudd.Bdd.t) (t:'a t) : 'a t  
    =
  match man.oglobal with
  | None ->
      let bottomdd = Cudd.Mtbddc.cst_u (Cudd.Bdd.manager supp) bottom in
      Cudd.User.map_existand ~bottom (make_funop man bottomdd) ~supp guard t
  | Some global ->
      let (mexist:('a leaf_u, Cudd.User.global) Cudd.User.mexist) =
	(`Op global.op_join)
      in
      Cudd.User.map_existand ~bottom mexist ~supp guard t
