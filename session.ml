(* essai BddApronDomain *)

open Format;;
module D = BddaprondomainE;;
module E = BddapronexprE;;

(*
bddaprontop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $FORMULA_INSTALL/lib -I $MLGMPIDL_INSTALL/lib -I $APRON_INSTALL/lib

#install_printer Apron.Abstract1.print;;
let print fmt x = Apron.Environment.print fmt x;;
#install_printer print;;
#install_printer E.print_expr;;
#install_printer E.Bool.print;;
#install_printer E.Apron.print;;
#install_printer D.print;;
#install_printer D.print_env;;
#install_printer D.print_env;;
*)

let cudd = Manager.make 0 0 0 0 0;;
Manager.print_limit := 200;;
Manager.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let apron = Polka.manager_alloc_loose ();;
let bddapron = D.make_manager apron;;
let env = D.make_env cudd;;
let env = Bddenv.add_typ env "enum2" (`Benum [|"l1"; "l2"; "l3"|]);;

let env = D.add_vars env [
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

let expr0 = E.Apron.ite 
  (E.Bool.var env "b0")
  (E.Apron.var env "x0")
  (E.Apron.mul
    (E.Apron.cst env (Apron.Coeff.s_of_int 2))
    (E.Apron.var env "x1"))
;;
let expr1 = E.Apron.ite 
  (E.Bool.var env "b1")
  (E.Apron.var env "x1")
  (E.Apron.mul
    (E.Apron.cst env (Apron.Coeff.s_of_int 2))
    (E.Apron.var env "x2"))
;;

let expr2 = E.Apron.mul expr0 expr1;;
let expr3 = E.Apron.add expr1 expr2;;
let expr4 = E.Apron.add expr3 expr3;;

let print fmt x = Mtbdd2.print (Bddexpr.print_bdd x.Bddenv.env)
  (fun fmt x -> fprintf fmt "@[leaf leaf leaf@]") fmt x.Bddenv.value;;

let print fmt x = Mtbdd2.print (Bddexpr.print_bdd x.Bddenv.env) 
  (fun fmt x -> fprintf fmt "%a@]" ApronExpr.print x)
  fmt x.Bddenv.value;;

printf "@.@[%a@ %a@ %a@ %a@]@ %a@]"
print expr0
print expr1
print expr2
print expr3
print expr4
;;

let res = ref expr2;;
for i=0 to 9 do
  res := E.Apron.add !res expr2
done;;

let f i =
  let q = E.Bint.var env "q" in
  let qi = E.Bint.eq_int q i in
  let (i,pol) = (i/2, ((i mod 2)=0)) in
  let b = E.Bool.var env (sprintf "b%i" i) in
  let x = E.Apron.var env (sprintf "x%i" i) in
  if pol then
    E.Bool.dand
      (E.Bool.dand qi b)
      (E.Apron.supeq
	(E.Apron.sub
	  x
	  (E.Apron.cst env (Apron.Coeff.s_of_int 1))))
  else
    E.Bool.dand
      (E.Bool.dand qi (E.Bool.dnot b))
      (E.Bool.dand
	(E.Apron.supeq
	  (E.Apron.negate x))
	(E.Apron.supeq
	  (E.Apron.add
	    x
	    (E.Apron.cst env (Apron.Coeff.s_of_int 3)))))
;;
let tcond = Array.init 8 f;;
let top = D.top apron env;;
printf "After top@.";;

let cond = E.Bool.dand
  (E.Bool.var env "b0")
  (E.Apron.supeq (E.Apron.var env "x0"));;

let abs = D.meet_cond apron top cond;;
printf "After meet_cond@.";;

let tabs =
  Array.map (fun cond -> D.meet_cond apron top cond) tcond;;
let abs = D.join apron tabs.(1) tabs.(2);;
D.forget_list apron abs ["b0"];;
D.assign_list apron abs [("b1", E.Bool.to_expr (E.Bool.dtrue env))] None;;
D.assign_list apron abs [("b0", E.Bool.to_expr (E.Bool.dtrue env))] None ;;
D.assign_list apron abs [("e", E.Benum.to_expr (E.Benum.var env "l2"))] None;;
let abs =
  Array.fold_left (D.join apron) tabs.(0) tabs
;;
printf "After assign@.";;

let nabs = D.assign_list apron abs [
  ("x0",
   E.Apron.to_expr (E.Apron.add (E.Apron.var env "x1") (E.Apron.var env "x1")));
  ("x3",
  E.ite
    (E.Bool.var env "b2")
    (E.Apron.to_expr (E.Apron.add (E.Apron.var env "x1") (E.Apron.var env "x2")))
    (E.Apron.to_expr (E.Apron.sub (E.Apron.var env "x1") (E.Apron.var env "x2"))))
]
None;;
printf "After nabs@.";;
printf "abs=%a@." D.print abs;;
Gc.full_major();;
printf "nabs=%a@." D.print nabs;;
printf "env=%a@." D.print_env nabs.Bddenv.env;;
let env = nabs.Bddenv.env;;
let env2 = D.rename_vars env [("x1","y"); ("b0","c");("q","r")];;
let nabs2 = D.rename apron nabs [("x1","y"); ("b0","c");("q","r")];;
printf "After nabs2@.";;
printf "nabs2=%a@." D.print nabs2;;
printf "env=%a@." D.print_env nabs2.Bddenv.env;;


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

let print_bdd = Bdd.print_minterm string_of_int;;
let print_mtbdd = Mtbdd2.print print_bdd Leaf.print;;
let print_manager = Mtbdd2.print_manager Leaf.print;;

(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_manager;;
*)



let cudd = Manager.make 0 0 0 0 0;;

let man = Mtbdd2.make_manager
  ~background:Leaf.background
  ~hash:Leaf.hash
  ~equal:Leaf.equal
;;

let b = Array.init 5 (fun i -> Bdd.ithvar cudd i);;
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

let print_bdd = Bdd.print_minterm string_of_int;;
let print_mtbdd = Mtbdd2.print print_bdd Leaf.print;;
let print_manager = Mtbdd2.print_manager Leaf.print;;

(*
#install_printer print_bdd;;
#install_printer print_mtbdd;;
#install_printer print_manager;;
*)



let cudd = Manager.make 0 0 0 0 0;;

let man = Mtbdd2.make_manager
  ~background:Leaf.background
  ~hash:Leaf.hash
  ~equal:Leaf.equal
;;

let b = Array.init 5 (fun i -> Bdd.ithvar cudd i);;
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

let man = Manager.make 0 0 0 0 0;;
let env = BddapronexprE.make_env man
;;

let env1 =
  BddapronexprE.add_typ env
    "enum1"
    (`Benum [|"label1"; "label2"; "label3"|])
;;

let env1 = BddapronexprE.add_vars
  env1
  [
    (var_x,`Bool);
    (var_z,`Bint (false,3));
    (var_u,`Int);
    (var_a,`Benum "enum1")
  ]
;;

let t = (BddapronexprE.Apron.sub
  (BddapronexprE.Apron.mul
    (BddapronexprE.Apron.cst env1 (Apron.Coeff.s_of_int 2))
    (BddapronexprE.Apron.var env1 var_u))
  (BddapronexprE.Apron.cst env1 (Apron.Coeff.s_of_int 1))
);;
BddapronexprE.Apron.print Format.std_formatter t;;


let t = BddapronexprE.Apron.supeq t
;;

BddapronexprE.Bool.print Format.std_formatter t;;
*)

