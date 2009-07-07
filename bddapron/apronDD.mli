(** Decision diagrams on top of Apron abstract values *)

type 'a leaf = 'a Apron.Abstract0.t
type 'a t = 'a leaf Cudd.Mtbddc.t
type 'a table = 'a leaf Cudd.Mtbddc.table

type 'a global = {
  op_is_leq : (('a leaf, 'a leaf) Cudd.Mtbddc.test2,  Cudd.Mtbddc.global) Cudd.Mtbddc.op;
  op_join : (('a leaf, 'a leaf, 'a leaf) Cudd.Mtbddc.op2, Cudd.Mtbddc.global) Cudd.Mtbddc.op;
  op_meet : (('a leaf, 'a leaf, 'a leaf) Cudd.Mtbddc.op2, Cudd.Mtbddc.global) Cudd.Mtbddc.op;
  op_exist : (('a leaf, Cudd.Mtbddc.global) Cudd.Mtbddc.exist, Cudd.Mtbddc.global) Cudd.Mtbddc.op;
}
type 'a man = {
  apron : 'a Apron.Manager.t;
  table : 'a table;
  oglobal : 'a global option;
}

val make_table : 'a Apron.Manager.t -> 'a table
val neutral_join : 'a Apron.Abstract0.t -> bool
val special_is_leq :
  'a Apron.Manager.t -> 'a t -> 'a t -> bool option
val special_join :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val special_meet :
  'a Apron.Manager.t -> 'a t -> 'a t -> 'a t option
val make_global :
  'a Apron.Manager.t -> 'a table -> 'a global
val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man

val print :
  (Format.formatter -> Cudd.Man.v Cudd.Bdd.t -> unit) ->
  (int -> string) ->
  Format.formatter -> 'a t -> unit
val cst : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> 'a Apron.Abstract0.t -> 'a t
val bottom : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> Apron.Dim.dimension -> 'a t
val top : cudd:Cudd.Man.v Cudd.Man.t -> 'a man -> Apron.Dim.dimension -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet : 'a man -> 'a t -> 'a t -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val meet_tcons_array : 'a man -> 'a t -> Apron.Tcons0.t array -> 'a t
val forget_array : 'a man -> 'a t -> Apron.Dim.t array -> 'a t
val permute_dimensions : 'a man -> 'a t -> Apron.Dim.perm -> 'a t
val add_dimensions : 'a man -> 'a t -> Apron.Dim.change -> bool -> 'a t
val remove_dimensions : 'a man -> 'a t -> Apron.Dim.change -> 'a t
val apply_dimchange2 : 'a man -> 'a t -> Apron.Dim.change2 -> bool -> 'a t

val asssub_texpr_array :
  ?asssub_bdd:(Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t) ->
  ('a Apron.Manager.t ->
    'a Apron.Abstract0.t ->
      Apron.Dim.t array ->
	Apron.Texpr0.t array ->
	  'a Apron.Abstract0.t option -> 'a Apron.Abstract0.t) ->
  'a man -> Apron.Environment.t ->
  'a t ->
  Apron.Dim.t array -> ApronexprDD.t array -> 'a t option -> 'a t


val assign_texpr_array :
  'a man -> Apron.Environment.t ->
  'a t -> Apron.Dim.t array -> ApronexprDD.t array -> 'a t option -> 'a t
val substitute_texpr_array :
  'a man -> Apron.Environment.t ->
  'a t -> Apron.Dim.t array -> ApronexprDD.t array -> 'a t option -> 'a t

val make_fun :
  'a man -> ('a Apron.Abstract0.t, Cudd.Mtbddc.global) Cudd.Mtbddc.mexist
val make_funop :
  'a man -> 'a t -> ('a Apron.Abstract0.t, Cudd.Mtbddc.global) Cudd.Mtbddc.mexist
val exist : 'a man -> supp:Cudd.Man.v Cudd.Bdd.t -> 'a t -> 'a t
val existand :
  'a man ->
  bottom:'a Apron.Abstract0.t Cudd.Mtbddc.unique -> 
  supp:Cudd.Man.v Cudd.Bdd.t -> Cudd.Man.v Cudd.Bdd.t -> 'a t -> 'a t
