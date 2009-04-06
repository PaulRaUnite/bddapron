(* essai BddApronDomain *)

open Format;;
open Bddapron;;

(*
bddaprontop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib -I $MLGMPIDL_INSTALL/lib -I $APRON_INSTALL/lib

Gc.set { (Gc.get()) with Gc.verbose = 0x11 };;

open Format;;
open Bddapron;;
#install_printer Apron.Abstract1.print;;
#install_printer Cudd.Bdd.print__minterm;;
let print fmt x = Apron.Environment.print fmt x;;
#install_printer print;;
#install_printer Apronexpr.print;;
let print fmt x = Cudd.Weakke.print Apronexpr.print fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print Apron.Abstract1.print fmt x ;;
#install_printer print;;
#install_printer Expr1.print;;
#install_printer Expr1.Bool.print;;
#install_printer Expr1.Bint.print;;
#install_printer Expr1.Benum.print;;
#install_printer Expr1.Apron.print;;
#install_printer Domain1.print;;
#install_printer Env.print;;
*)

let cudd = Cudd.Man.make_v ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let apron = Polka.manager_alloc_loose ();;
let bddapron = Domain1.make_man apron;;
let env = Domain1.make_env cudd;;
env#add_typ "enum2" (`Benum [|"l1"; "l2"; "l3"|]);;
env;;
env#add_vars [
  ("q",`Bint(false,3));
  ("e",`Benum("enum2"));
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("b3",`Bool);
  ("x0",`Real);
  ("x1",`Real);
  ("x2",`Real);
  ("x3",`Real);
];;
env;;

let expr0 = Parser.expr1_of_string env "if b0 then x0 else 2*x1";;

let expr = Expr1.Bool.of_expr (Parser.expr1_of_string env "x0>=10 or x1>=10");;

let top = Domain1.top bddapron env;;
let abs0 = Domain1.meet_cond bddapron top expr;;






let expr0 = Expr1.Apron.ite 
  (Expr1.Bool.var env "b0")
  (Expr1.Apron.var env "x0")
  (Expr1.Apron.mul
    (Expr1.Apron.cst env (Apron.Coeff.s_of_int 2))
    (Expr1.Apron.var env "x1"))
;;
Cudd.Man.garbage_collect cudd;;

printf "%a@." Expr1.Apron.print expr0;;
let expr0 = expr0.Env.value;;
let leaves = Cudd.Mtbdd.leaves expr0;;
let leaves_u = Cudd.Mtbdd.leaves_u expr0;;
Cudd.Mtbdd.guard_of_leaf_u expr0 leaves_u.(1);;


printf "%a@."
  (Cudd.Mtbdd.print__minterm
    Apronexpr.print)
    expr0
;;
printf "%a@."
  (Cudd.Mtbdd.print_minterm
    pp_print_int
    Apronexpr.print)
    expr0
;;



let expr0 = Parser.expr1_of_string env "if b0 then x0 else 2*x1";;

printf "%a@." Expr1.Apron.print expr0;;
let expr1 = Expr1.Apron.ite 
  (Expr1.Bool.var env "b1")
  (Expr1.Apron.var env "x1")
  (Expr1.Apron.mul
    (Expr1.Apron.cst env (Apron.Coeff.s_of_int 2))
    (Expr1.Apron.var env "x2"))
;;

let expr2 = Expr1.Apron.mul expr0 expr1;;
let expr3 = Expr1.Apron.add expr1 expr2;;
let expr4 = Expr1.Apron.add expr3 expr3;;

let res = ref expr2;;
for i=0 to 9 do
  res := Expr1.Apron.add !res expr2
done;;

let f i =
  let q = Expr1.Bint.var env "q" in
  let qi = Expr1.Bint.eq_int q i in
  let (i,pol) = (i/2, ((i mod 2)=0)) in
  let b = Expr1.Bool.var env (sprintf "b%i" i) in
  let x = Expr1.Apron.var env (sprintf "x%i" i) in
  if pol then
    Expr1.Bool.dand
      (Expr1.Bool.dand qi b)
      (Expr1.Apron.supeq
	(Expr1.Apron.sub
	  x
	  (Expr1.Apron.cst env (Apron.Coeff.s_of_int 1))))
  else
    Expr1.Bool.dand
      (Expr1.Bool.dand qi (Expr1.Bool.dnot b))
      (Expr1.Bool.dand
	(Expr1.Apron.supeq
	  (Expr1.Apron.negate x))
	(Expr1.Apron.supeq
	  (Expr1.Apron.add
	    x
	    (Expr1.Apron.cst env (Apron.Coeff.s_of_int 3)))))
;;
let tcond = Array.init 8 f;;
let top = Domain1.top apron env;;
printf "After top@.";;

let cond = Expr1.Bool.dand
  (Expr1.Bool.var env "b0")
  (Expr1.Apron.supeq (Expr1.Apron.var env "x0"));;

let abs = Domain1.meet_cond apron top cond;;
printf "After meet_cond@.";;

let tabs =
  Array.map (fun cond -> Domain1.meet_cond apron top cond) tcond;;
let abs = Domain1.join apron tabs.(1) tabs.(2);;
Domain1.forget_list apron abs ["b0"];;
Domain1.assign_list apron abs [("b1", Expr1.Bool.to_expr (Expr1.Bool.dtrue env))] None;;
Domain1.assign_list apron abs [("b0", Expr1.Bool.to_expr (Expr1.Bool.dtrue env))] None ;;
Domain1.assign_list apron abs [("e", Expr1.Benum.to_expr (Expr1.Benum.var env "l2"))] None;;
let abs =
  Array.fold_left (Domain1.join apron) tabs.(0) tabs
;;
printf "After assign@.";;

let nabs = Domain1.assign_list apron abs [
  ("x0",
   Expr1.Apron.to_expr (Expr1.Apron.add (Expr1.Apron.var env "x1") (Expr1.Apron.var env "x1")));
  ("x3",
  Expr1.ite
    (Expr1.Bool.var env "b2")
    (Expr1.Apron.to_expr (Expr1.Apron.add (Expr1.Apron.var env "x1") (Expr1.Apron.var env "x2")))
    (Expr1.Apron.to_expr (Expr1.Apron.sub (Expr1.Apron.var env "x1") (Expr1.Apron.var env "x2"))))
]
None;;
printf "After nabs@.";;
printf "abs=%a@." Domain1.print abs;;
Gc.full_major();;
printf "nabs=%a@." Domain1.print nabs;;
printf "env=%a@." Domain1.print_env nabs.Bddenv.env;;
let env = nabs.Bddenv.env;;
let env2 = Domain1.rename_vars env [("x1","y"); ("b0","c");("q","r")];;
let nabs2 = Domain1.rename apron nabs [("x1","y"); ("b0","c");("q","r")];;
printf "After nabs2@.";;
printf "nabs2=%a@." Domain1.print nabs2;;
printf "env=%a@." Domain1.print_env nabs2.Bddenv.env;;


(*
module Leaf = struct
  type t = { min:int; max:int }
  let make a b = {min=a; max=b}
  let cst n = make n n
  let add t1 t2 =
    {
      min = t1.min + t2.min;
    max = t1.max + t2.max
    }
  let print fmt t =
    fprintf fmt "[%i,%i]" t.min t.max
  let background = make 1 (-1)
  let hash t = 11*t.min + 13*t.max
  let equal = (=)
end;;

let print_bdd = Cudd.Bdd.print_minterm string_of_int;;
let print_mtbdd = Mtbdd2.print print_bdd Leaf.print;;
let print_manager = Mtbdd2.print_manager Leaf.print;;

(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_manager;;
*)



let cudd = Cudd.Man.make 0 0 0 0 0;;

let man = Mtbdd2.make_manager
  ~background:Leaf.background
  ~hash:Leaf.hash
  ~equal:Leaf.equal
;;

let b = Array.init 5 (fun i -> Cudd.Bdd.ithvar cudd i);;
let i = Array.init 5 (fun i -> Mtbdd2.cst cudd man (Leaf.cst i));;

let add t1 t2 =
  Mtbdd2.mapleaf2 t1.Mtbdd2.man
    (fun _ x y -> Gc.compact(); Leaf.add x y) t1 t2;;

let proc1 () =
  let res = ref (Mtbdd2.background cudd man) in
  for j=2 to 4 do
    res := Mtbdd2.ite b.(j) i.(j) !res
  done;
  !res
;;
let proc2 () =
  let f = proc1 () in
  let g = add f f in
  printf "g=%a@." print_mtbdd g;;
  ()
;;

proc2();;
*)







(* essai MTBDD *)
(*
open Format;;
module Leaf = struct
  type t = { min:int; max:int }
  let make a b = {min=a; max=b}
  let cst n = make n n
  let add t1 t2 =
    {
      min = t1.min + t2.min;
    max = t1.max + t2.max
    }
  let print fmt t =
    fprintf fmt "[%i,%i]" t.min t.max
  let background = make 1 (-1)
  let hash t = 11*t.min + 13*t.max
  let equal = (=)
end;;

let print_bdd = Cudd.Bdd.print_minterm string_of_int;;
let print_mtbdd = Mtbdd2.print print_bdd Leaf.print;;
let print_manager = Mtbdd2.print_manager Leaf.print;;

(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_manager;;
*)



let cudd = Cudd.Man.make 0 0 0 0 0;;

let man = Mtbdd2.make_manager
  ~background:Leaf.background
  ~hash:Leaf.hash
  ~equal:Leaf.equal
;;

let b = Array.init 5 (fun i -> Cudd.Bdd.ithvar cudd i);;
let i = Array.init 5 (fun i -> Mtbdd2.cst cudd man (Leaf.cst i));;

let add t1 t2 =
  Mtbdd2.mapleaf2 t1.Mtbdd2.man
    (fun _ x y -> Gc.compact(); Leaf.add x y) t1 t2;;

let proc1 () =
  let res = ref (Mtbdd2.background cudd man) in
  for j=2 to 4 do
    res := Mtbdd2.ite b.(j) i.(j) !res
  done;
  !res
;;
let proc2 () =
  let f = proc1 () in
  let g = add f f in
  printf "g=%a@." print_mtbdd g;;
  ()
;;

proc2();;
*)


(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_manager;;
*)

(*
let var_z = "z";;
let var_w = "w";;
let var_x = "x";;
let var_y = "y";;
let var_a = "a";;
let var_v = "v";;
let var_u = "u";;
let var_b = "b";;

let man = Cudd.Man.make 0 0 0 0 0;;
let env = BddapronexprExpr1.make_env man
;;

let env1 =
  BddapronexprExpr1.add_typ env
    "enum1"
    (`Benum [|"label1"; "label2"; "label3"|])
;;

let env1 = BddapronexprExpr1.add_vars
  env1
  [
    (var_x,`Bool);
    (var_z,`Bint (false,3));
    (var_u,`Int);
    (var_a,`Benum "enum1")
  ]
;;

let t = (BddapronexprExpr1.Apron.sub
  (BddapronexprExpr1.Apron.mul
    (BddapronexprExpr1.Apron.cst env1 (Apron.Coeff.s_of_int 2))
    (BddapronexprExpr1.Apron.var env1 var_u))
  (BddapronexprExpr1.Apron.cst env1 (Apron.Coeff.s_of_int 1))
);;
BddapronexprExpr1.Apron.print Format.std_formatter t;;


let t = BddapronexprExpr1.Apron.supeq t
;;

Expr1.Bool.print Format.std_formatter t;;
*)

