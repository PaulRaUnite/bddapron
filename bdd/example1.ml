(*
formulatop -I $CAMLLIB_INSTALL/lib -I $MLCUDDIDL_INSTALL/lib
*)

open Format;;

let cudd = Cudd.Man.make_d ();;
Cudd.Man.print_limit := 200;;
Cudd.Man.set_gc 10000
  (begin fun () -> printf "@.CUDD GC@." end)
  (begin fun () -> printf "@.CUDD REORDER@." end)
;;
let env = BdddomainE.make_env cudd;;
env#add_typ "enum2" (`Benum [|"l1"; "l2"; "l3"|]);;
env;;
env#add_vars [
  ("q1",`Bint(false,3));
  ("q2",`Bint(false,3));
  ("e",`Benum("enum2"));
  ("b0",`Bool);
  ("b1",`Bool);
  ("b2",`Bool);
  ("b3",`Bool);
];;
env;;

let expr = BddexprE.Bool.var env "b0";;
printf "%a@." BddexprE.Bool.print expr;;
