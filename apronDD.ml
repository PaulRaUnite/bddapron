open Format

(*  ********************************************************************** *)
(** {2 Decision diagram} *)
(*  ********************************************************************** *)

include Mtbdd2

type 'a leaf = 'a Apron.Abstract1.t

(*
let make_manager (apron:'a Apron.Manager.t) =
  make_manager
    ~background:(Apron.Abstract1.bottom apron (Apron.Environment.make [||] [||]))
    ~hash:(fun x ->
      let x0 = x.Apron.Abstract1.abstract0 in
      11*(Apron.Environment.hash x.Apron.Abstract1.env) +
      13*(Apron.Abstract0.size (Apron.Abstract0.manager x0) x0)
    )
    ~equal:(fun x y ->
      x==y ||
     (Apron.Abstract1.env x)=(Apron.Abstract1.env y) &&
      (Apron.Abstract1.is_eq (Apron.Abstract1.manager x) x y)
    )
*)
let make_manager (apron:'a Apron.Manager.t) =
  Mtbdd2.make_manager
    ~background:(Apron.Abstract1.bottom apron (Apron.Environment.make [||] [|Apron.Var.of_string "%d%u%m%m%y%"|]))
    ~hash:Hashtbl.hash
    ~equal:(fun x y -> Pervasives.compare x y = 0)

let print_manager fmt x = Mtbdd2.print_manager Apron.Abstract1.print fmt x

let print print_bdd fmt t =
  if false then
    printf "ApronDD.t=%a@.man=%a@.t.leaves=%a@.iddleaves=%a@."
      (Idd.print_minterm string_of_int string_of_int) t.idd
      print_manager t.man
      (Print.array Apron.Abstract1.print) (leaves t)
      (Print.array pp_print_int) (Idd.leaves t.idd)
  ;
  if Mtbdd2.is_cst t then
    Apron.Abstract1.print fmt (dval t)
  else
    let nb = Mtbdd2.nbpaths t in
    if nb > (float_of_int !Manager.print_limit) then
      fprintf fmt "idd with %i nodes, %i leaves and %g paths"
	(Idd.size t.idd) (Idd.nbleaves t.idd) nb
    else begin
      let leaves = Mtbdd2.leaves t in
      assert((Array.length leaves) >= 2);
      let index_bottom = ref (-1) and first = ref true in
      fprintf fmt "{ @[<v>";
      for i=Array.length leaves - 1 downto 0 do
	let leaf = leaves.(i) in
	if Apron.Abstract1.is_bottom (Apron.Abstract1.manager leaf) leaf then
	  index_bottom := i
	else begin
	  if !first then first := false else fprintf fmt ",@ ";
	  let bdd = Mtbdd2.guard_of_leaf t leaf in
	  fprintf fmt "%a IF %a"
	    Apron.Abstract1.print leaf print_bdd bdd;
	end
      done;
      if !index_bottom >= 0 then begin
	assert (not !first);
	fprintf fmt ",@ %a OTHERWISE"
	  Apron.Abstract1.print leaves.(!index_bottom)
      end;
      fprintf fmt "@] }"
    end

let join (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) (y:'a Apron.Abstract1.t t) :'a Apron.Abstract1.t t =
  assert(x.man==y.man);
  Mtbdd2.mapbinopid ~commutative:true
    x.man
    (fun idx x idy y ->
      if idx=idy then x
      else if Apron.Abstract1.is_bottom apron x then y
      else if Apron.Abstract1.is_bottom apron y then x
      else Apron.Abstract1.join apron x y)
    x y

let meet (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) (y:'a Apron.Abstract1.t t) :'a Apron.Abstract1.t t =
  assert(x.man==y.man);
  Mtbdd2.mapbinopid ~commutative:true
    x.man
    (fun idx x idy y ->
      if idx=idy then x
      else Apron.Abstract1.meet apron x y)
    x y

let widening (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) (y:'a Apron.Abstract1.t t) :'a Apron.Abstract1.t t =
  assert(x.man==y.man);
  Mtbdd2.mapbinop ~commutative:false
    x.man
    (Apron.Abstract1.widening apron)
    x y


let meet_tcons_array apron x tcons =
(*
  printf "ApronDD.meet_tcons_array %a %a@."
    (print (Bdd.print_minterm string_of_int)) x
    (fun x -> Apron.Tcons1.array_print x) tcons;
*)
  mapunop x.man
    (fun x -> Apron.Abstract1.meet_tcons_array apron x tcons)
    x

let forget_array (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) (tvar:Apron.Var.t array)
  =
  mapunop x.man
    (fun x -> Apron.Abstract1.forget_array apron x tvar false)
    x

let change_environment (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) nenv : 'a Apron.Abstract1.t t =
  mapunop x.man
    (fun x -> Apron.Abstract1.change_environment apron x nenv false)
    x

let rename_array (apron:'a Apron.Manager.t) (x:'a Apron.Abstract1.t t) tvar1 tvar2 : 'a Apron.Abstract1.t t =
  mapunop x.man
    (fun x -> Apron.Abstract1.rename_array apron x tvar1 tvar2)
    x

let is_leq (apron:'a Apron.Manager.t) t1 t2 =
  assert(t1.man==t2.man);
  let res =
    Idd.mapbinop ~commutative:false
      (begin fun id1 id2 ->
	if
	  id1=id2 ||
	  Apron.Abstract1.is_leq apron
	  (leaf_of_id t1.man id1) (leaf_of_id t2.man id2)
	then
	  1
	else
	  0
      end)
      t1.idd t2.idd
  in
  Idd.is_cst res && Idd.dval res = 1

let is_eq (apron:'a Apron.Manager.t) =
  Mtbdd2.is_equal

let cst cudd manager abs =
  Mtbdd2.cst cudd manager abs

let bottom cudd manager apron env =
  cst cudd manager (Apron.Abstract1.bottom apron env)
let top cudd manager apron env =
  cst cudd manager (Apron.Abstract1.top apron env)
let is_bottom apron x =
  Mtbdd2.is_cst x && Apron.Abstract1.is_bottom apron (Mtbdd2.dval x)
let is_top apron x =
  Mtbdd2.is_cst x && Apron.Abstract1.is_top apron (Mtbdd2.dval x)

let mapguardleaf apron f t background =
  let guardleafs = Mtbdd2.guardleafs t in
  let manager = Mtbdd2.manager t in
  let background = cst manager t.man background in
  let res = ref background in
  Array.iter
    (begin fun ((guard,leaf) as guardleaf) ->
      let (nguard,nleaf) = f guardleaf in
      res := join apron !res
	(Mtbdd2.ite nguard
	  (cst manager t.man nleaf)
	  background)
    end)
    guardleafs
  ;
  !res

let mapguardleaf2 apron f t1 t2 background
  =
  assert(t1.man==t2.man);
  let guardleafs1 = Mtbdd2.guardleafs t1 in
  let manager = Mtbdd2.manager t1 in
  let background = cst manager t1.man background in
  let res = ref background in
  Array.iter
    (begin fun (guard1,leaf1) ->
      let t2 = Mtbdd2.ite guard1 t2 background in
      if not (Mtbdd2.is_background t2) then begin
	let guardleafs2 = Mtbdd2.guardleafs t2 in
	Array.iter
	  (begin fun (guard2,leaf2) ->
	    let (nguard,nleaf) = f guard2 leaf1 leaf2 in
	    res := join apron !res
	      (Mtbdd2.ite nguard (cst manager t1.man nleaf) background)
	  end)
	  guardleafs2
      end
    end)
    guardleafs1
  ;
  !res

let assign_texpr_array
  (apron:'a Apron.Manager.t)
  (x:'a Apron.Abstract1.t t) (tvar:Apron.Var.t array) (texpr:Apron.Texpr1.t array)
  (odest:'a Apron.Abstract1.t t option)
  :
  'a Apron.Abstract1.t t
  =
  if is_bottom apron x then x else begin
    match odest with
    | None ->
	mapunop x.man
	(fun x -> Apron.Abstract1.assign_texpr_array apron x tvar texpr None)
	x
    | Some y ->
	if is_bottom apron y then x else begin
	  assert(x.man==y.man);
	  mapbinop ~commutative:false x.man
	    (fun x y ->
	      if Apron.Abstract1.is_bottom apron x then x
	      else if Apron.Abstract1.is_bottom apron y then y
	      else
		Apron.Abstract1.assign_texpr_array apron x tvar texpr (Some y))
	    x y
	end
  end

let substitute_texpr_array
  (apron:'a Apron.Manager.t)
  (x:'a Apron.Abstract1.t t) (tvar:Apron.Var.t array) (texpr:Apron.Texpr1.t array)
  (odest:'a Apron.Abstract1.t t option)
  :
  'a Apron.Abstract1.t t
  =
  if is_bottom apron x then x else begin
    match odest with
    | None ->
	mapunop x.man
	(fun x -> Apron.Abstract1.substitute_texpr_array apron x tvar texpr None)
	x
    | Some y ->
	if is_bottom apron y then x else begin
	  assert(x.man==y.man);
	  mapbinop ~commutative:false x.man
	    (fun x y ->
	      if Apron.Abstract1.is_bottom apron x then x
	      else if Apron.Abstract1.is_bottom apron y then y
	      else
		Apron.Abstract1.substitute_texpr_array apron x tvar texpr (Some y))
	    x y
	end
  end
