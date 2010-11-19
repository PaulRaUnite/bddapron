
(*
ocaml -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib -I $BDDAPRON_INSTALL/lib -I $MLGMPIDL_INSTALL/lib -I $APRON_INSTALL/lib

#load "bigarray.cma";;
#load "gmp.cma";;
#load "apron.cma";;
#load "camllib.cma";;
#load "cudd.cma";;
#load "bddapron.cma";;
#load "polkaMPQ.cma";;
*)
open Format;;
open Bddapron;;

let apron = Polka.manager_alloc_loose ();;
let man = Domain1.make_bdd apron;;
let cudd = Cudd.Man.make_v ();;
let env = Env.make ~symbol:Env.string_symbol cudd;;
let cond = Cond.make  ~symbol:Env.string_symbol cudd;;
(*
#install_printer p;;
#install_printer Apron.Abstract1.print;;
#install_printer Cudd.Bdd.print__minterm;;
let print fmt x = Apron.Environment.print fmt x;;
#install_printer print;;
#install_printer Apronexpr.print;;
let print fmt x = Cudd.Weakke.print Apronexpr.print fmt x;;
#install_printer print;;
let print fmt x = Cudd.Weakke.print Apron.Abstract1.print fmt x ;;
#install_printer print;;
#install_printer Domain1.print;;
#install_printer Env.print;;
#install_printer Expr2.Bool.print;;
let p = Cond.print env;;
#install_printer p;;
let p = Expr1.print cond;;
#install_printer p;;
let p = Expr1.Bool.print cond;;
#install_printer p;;
let p = Expr1.Apron.print cond;;
#install_printer p;;
let p x = Expr0.print env cond x;;
#install_printer p;;
let p = Expr0.Bool.print env cond;;
#install_printer p;;
let p = Expr0.Apron.print env cond;;
#install_printer p;;
let p fmt x = Domain0.print env fmt x;;
#install_printer p;;
let p fmt (x: Polka.loose Polka.t ApronDD.table) = Cudd.Mtbdd.print_table (fun x -> Apron.Abstract0.print string_of_int x) fmt x;;
#install_printer p;;
*)
Env.add_vars_with env [
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("x0",`Real);
  ("x1",`Real);
  ("x2",`Real);
];;
let string_of_dim i = "x"^(string_of_int i);;
let print_table fmt table = 
  Cudd.Mtbddc.print_table (fun x -> Apron.Abstract0.print string_of_dim x)
    fmt table
;;

let top = Domain0.top man env;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;
printf "table = %a@." print_table man.ApronDD.table;;

let bexpr1 = Parser.boolexpr2_of_string env cond
  "x0>=0";;
let bexpr1 = bexpr1.Cond.val1.Env.val0;;
let abs1 = Domain0.meet_condition man env cond top bexpr1;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.major();;


let bexpr1 = Parser.boolexpr2_of_string env cond
  "x0+2*x1>=0 and x0+x1<=4 and x2>=0 and x2<=10";;
let bexpr1 = bexpr1.Cond.val1.Env.val0;;
let bexpr2 = Parser.boolexpr2_of_string env cond
  "x0+2*x1>=2 and x0+x1<=6 and x2>=3 and x2<=11";;
let bexpr2 = bexpr2.Cond.val1.Env.val0;;

let abs1 = Domain0.meet_condition man env cond top bexpr1;;
printf "table = %a@." print_table man.ApronDD.table;;
let abs2 = Domain0.meet_condition man env cond top bexpr2;;
printf "table = %a@." print_table man.ApronDD.table;;
let abs = Domain0.join man abs1 abs2;;
printf "table = %a@." print_table man.ApronDD.table;;
Gc.compact();;


(* ********************************************************************** *)
let expr = Parser.expr1_of_string env cond "if (b0==b1) and b2 then x0+2 else if not (b0==b1) then x0+1 else x0";;

let nexpr = Expr1.substitute_by_var cond expr [("b0","b2")];;

