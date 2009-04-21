(** Decision diagrams on top of Apron abstract values *)

type 'a t = 'a Apron.Abstract1.t Cudd.Mtbdd.t
type 'a table = 'a Apron.Abstract1.t Cudd.Mtbdd.table
type 'a global = {
  op_is_leq :
    (('a Apron.Abstract1.t, 'a Apron.Abstract1.t) Cudd.Mtbdd.test2,
     Cudd.Mtbdd.global)
    Cudd.Mtbdd.op;
  op_join :
    (('a Apron.Abstract1.t, 'a Apron.Abstract1.t, 'a Apron.Abstract1.t)
     Cudd.Mtbdd.op2, Cudd.Mtbdd.global)
    Cudd.Mtbdd.op;
  op_meet :
    (('a Apron.Abstract1.t, 'a Apron.Abstract1.t, 'a Apron.Abstract1.t)
     Cudd.Mtbdd.op2, Cudd.Mtbdd.global)
    Cudd.Mtbdd.op;
  op_exist :
    (('a Apron.Abstract1.t, Cudd.Mtbdd.global) Cudd.Mtbdd.exist, Cudd.Mtbdd.global)
    Cudd.Mtbdd.op;
}
type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}
val make_table : 'a Apron.Manager.t -> 'a table
val neutral_join : 'a Apron.Abstract1.t -> bool
val special_is_leq :
  'a Apron.Manager.t -> 'a t -> 'a t -> bool option
val special_join :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val special_meet :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val make_global :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t Cudd.Mtbdd.table -> 'a global
val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man

val print :
  (Format.formatter -> Cudd.Man.v Cudd.Bdd.t -> unit) ->
  Format.formatter -> 'a t -> unit
val cst : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> 'a Apron.Abstract1.t -> 'a t
val bottom : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> Apron.Environment.t -> 'a t
val top : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> Apron.Environment.t -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet : 'a man -> 'a t -> 'a t -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val meet_tcons_array : 'a man -> 'a t -> Apron.Tcons1.earray -> 'a t
val forget_array : 'a man -> 'a t -> Apron.Var.t array -> 'a t
val change_environment : 'a man -> 'a t -> Apron.Environment.t -> 'a t
val rename_array :
  'a man -> 'a t -> Apron.Var.t array -> Apron.Var.t array -> 'a t

val asssub_texpr_array :
  ?asssub_bdd:(Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t) ->
  ('a Apron.Manager.t ->
   'a Apron.Abstract1.t ->
   Apron.Var.t array ->
   Apron.Texpr1.t array ->
   'a Apron.Abstract1.t option -> 'a Apron.Abstract1.t) ->
  'a man ->
  'a t ->
  Apron.Var.t array -> Apronexpr.t Cudd.Mtbdd.t array -> 'a t option -> 'a t


val assign_texpr_array :
  'a man ->
  'a t -> Apron.Var.t array -> ApronexprDD.t array -> 'a t option -> 'a t
val substitute_texpr_array :
  'a man ->
  'a t -> Apron.Var.t array -> ApronexprDD.t array -> 'a t option -> 'a t

val make_fun :
  'a man -> ('a Apron.Abstract1.t, Cudd.Mtbdd.global) Cudd.Mtbdd.mexist
val make_funop :
  'a man -> 'a t -> ('a Apron.Abstract1.t, Cudd.Mtbdd.global) Cudd.Mtbdd.mexist
val exist : 'a man -> supp:Cudd.Man.v Cudd.Bdd.t -> 'a t -> 'a t
val existand :
  'a man ->
  bottom:'a Apron.Abstract1.t Cudd.Mtbdd.unique -> 
  supp:Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t -> 'a t -> 'a t
