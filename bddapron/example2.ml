
module E = BddapronexprE;;
module D = BddaprondomainE;;

open Format;;

let cudd = Cudd.Man.make_v ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;

let apron = Box.manager_alloc();;
let man = BddaprondomainE.make_man apron;;

let env = Bddapronenv.make cudd;;

let env = Bddenv.add_vars env [("counter",`Int)];;

let counter = E.var env "counter";;

let expr0 = E.eq counter (E.Apron.to_expr (E.Apron.cst env (Apron.Coeff.s_of_int 2)));;
let expr1 = E.eq counter (E.Apron.to_expr (E.Apron.cst env (Apron.Coeff.s_of_int 0)));;

let top = D.top man env;;

let abs = D.meet_cond man top expr0;;
