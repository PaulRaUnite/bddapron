open Format;;

let cudd = Cudd.Man.make_v ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let apron = Box.manager_alloc();;
let man = Bddapron.Domain1.make_man apron;;

let env = Bddapron.Env.make cudd;;
let cond = Bddapron.Cond.make cudd;;

let env = Bddapron.Env.add_vars env 
  [
    ("counter",`Int);
    ("bcounter",(`Bint(false,3)))
  ];;

let counter = Bddapron.Expr1.var env cond "counter";;
printf "counter = %a@." (Bddapron.Expr1.print cond) counter;;

let bcounter = Bddapron.Expr1.var env cond "bcounter";;
printf "bcounter = %a@." (Bddapron.Expr1.print cond) bcounter;;

let cst_0 =
  Bddapron.Expr1.Apron.to_expr
    (Bddapron.Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 0));;
printf "cst_0 = %a@." (Bddapron.Expr1.print cond) cst_0;;
let cst_2 =
  Bddapron.Expr1.Apron.to_expr
    (Bddapron.Expr1.Apron.cst env cond (Apron.Coeff.s_of_int 2));;
printf "cst_2 = %a@." (Bddapron.Expr1.print cond) cst_2;;

let bcst_2 =
  Bddapron.Expr1.Bint.to_expr
    (Bddapron.Expr1.Bint.of_int env cond (`Tbint(false,3)) 2);;
printf "bcst_2 = %a@." (Bddapron.Expr1.print cond) bcst_2;;

let bcst_3 =
  Bddapron.Expr1.Bint.to_expr
    (Bddapron.Expr1.Bint.of_int env cond (`Tbint(false,3)) 3);;
printf "bcst_3 = %a@." (Bddapron.Expr1.print cond) bcst_3;;

let expr0 = Bddapron.Expr1.eq cond counter cst_2;;
printf "expr0 = %a@." (Bddapron.Expr1.Bool.print cond) expr0;;

let expr1 = 
  Bddapron.Expr1.Apron.ite cond 
    expr0 
    (Bddapron.Expr1.Apron.of_expr counter)
    (Bddapron.Expr1.Apron.add cond 
      (Bddapron.Expr1.Apron.of_expr counter)
      (Bddapron.Expr1.Apron.of_expr cst_2))
;;
printf "expr1 = %a@." (Bddapron.Expr1.Apron.print cond) expr1;;

let expr2 = Bddapron.Expr1.eq cond bcounter bcst_2;;
printf "expr2 = %a@." (Bddapron.Expr1.Bool.print cond) expr2;;

let expr3 = Bddapron.Expr1.ite cond expr2 bcst_3 bcounter;;
printf "expr3 = %a@." (Bddapron.Expr1.print cond) expr3;;

let top = Bddapron.Domain1.top man env;;

let abs = Bddapron.Domain1.meet_condition man cond top expr0;;

printf "abs = %a@." Bddapron.Domain1.print abs;;
