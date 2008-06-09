
type 'a manager = 'a Mtbdd2.manager
type 'a t = 'a Mtbdd2.t

(*  ********************************************************************** *)
(** {2 Specific functions} *)
(*  ********************************************************************** *)

val make_manager : 'a Apron.Manager.t -> 'a Apron.Abstract1.t manager
val print :
  (Format.formatter -> Bdd.t -> unit) ->
  Format.formatter -> 'a Apron.Abstract1.t t -> unit
val print_manager :
  Format.formatter -> 'a Apron.Abstract1.t manager -> unit

val cst : 
  Manager.t -> 'a Apron.Abstract1.t manager -> 
  'a Apron.Abstract1.t -> 'a Apron.Abstract1.t t
val bottom :
  Manager.t -> 'a Apron.Abstract1.t manager -> 'a Apron.Manager.t -> 
  Apron.Environment.t -> 'a Apron.Abstract1.t t
val top :
  Manager.t -> 'a Apron.Abstract1.t manager -> 'a Apron.Manager.t -> 
  Apron.Environment.t -> 'a Apron.Abstract1.t t
val is_bottom : 'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> bool
val is_top : 'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> bool
val is_leq : 'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> bool
val is_eq : 'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> bool
val join : 
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> 
  'a Apron.Abstract1.t t
val meet :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> 
  'a Apron.Abstract1.t t
val meet_tcons_array :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> Apron.Tcons1.earray -> 
  'a Apron.Abstract1.t t
val assign_texpr_array :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 
  Apron.Var.t array -> Apron.Texpr1.t array ->
  'a Apron.Abstract1.t t option ->
  'a Apron.Abstract1.t t
val substitute_texpr_array :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 
  Apron.Var.t array -> Apron.Texpr1.t array ->
  'a Apron.Abstract1.t t option ->
  'a Apron.Abstract1.t t
val forget_array :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> Apron.Var.t array -> 
  'a Apron.Abstract1.t t
val change_environment :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> Apron.Environment.t -> 
  'a Apron.Abstract1.t t
val rename_array :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 
  Apron.Var.t array -> Apron.Var.t array  -> 
  'a Apron.Abstract1.t t
val widening :
  'a Apron.Manager.t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> 
  'a Apron.Abstract1.t t

val mapguardleaf :
  'a Apron.Manager.t -> 
  (Bdd.t * 'a Apron.Abstract1.t -> Bdd.t * 'a Apron.Abstract1.t) ->
  'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t -> 'a Apron.Abstract1.t t
val mapguardleaf2 :
  'a Apron.Manager.t -> 
  (Bdd.t ->
   'a Apron.Abstract1.t ->
   'a Apron.Abstract1.t -> Bdd.t * 'a Apron.Abstract1.t) ->
  'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t t -> 'a Apron.Abstract1.t -> 
  'a Apron.Abstract1.t t

(*  ********************************************************************** *)
(** {2 Standard Mtbdd2 functions} *)
(*  ********************************************************************** *)

val leaf_of_id : 'a manager -> int -> 'a
val id_of_leaf : 'a manager -> 'a -> int
val of_idd : 'a manager -> Idd.t -> 'a t
val to_idd : 'a t -> Idd.t
val manager : 'a t -> Manager.t
val is_cst : 'a t -> bool
val topvar : 'a t -> int
val dthen : 'a t -> 'a t
val delse : 'a t -> 'a t
val cofactor : 'a t -> Bdd.t -> 'a t
val dval : 'a t -> 'a
val is_background : 'a t -> bool
val support : 'a t -> Bdd.t
val supportsize : 'a t -> int
val is_var_in : int -> 'a t -> bool
val vectorsupport : 'a t array -> Bdd.t
val vectorsupport2 : Bdd.t array -> 'a t array -> Bdd.t
val background : Manager.t -> 'a manager -> 'a t
val ite : Bdd.t -> 'a t -> 'a t -> 'a t
val compose : int -> Bdd.t -> 'a t -> 'a t
val vectorcompose : Bdd.t array -> 'a t -> 'a t
val is_equal : 'a t -> 'a t -> bool
val is_equal_when : 'a t -> 'a t -> Bdd.t -> bool
val is_eval_cst : 'a option t -> Bdd.t -> 'a option
val is_ite_cst : Bdd.t -> 'a option t -> 'a option t -> 'a option
val size : 'a t -> int
val nbpaths : 'a t -> float
val nbnonzeropaths : 'a t -> float
val nbminterms : int -> 'a t -> float
val density : int -> 'a t -> float
val nbleaves : 'a t -> int
val varmap : 'a t -> 'a t
val permute : 'a t -> int array -> 'a t
val iter_cube : (Manager.tbool array -> 'a -> unit) -> 'a t -> unit
val guard_of_nonbackground : 'a t -> Bdd.t
val leaves : 'a t -> 'a array
val guardleafs : 'a t -> (Bdd.t * 'a) array
val guard_of_leaf : 'a t -> 'a -> Bdd.t
val constrain : 'a t -> Bdd.t -> 'a t
val tdconstrain : 'a t -> Bdd.t -> 'a t
val restrict : 'a t -> Bdd.t -> 'a t
val tdrestrict : 'a t -> Bdd.t -> 'a t
val mapleaf1 : 'b manager -> (Bdd.t -> 'a -> 'b) -> 'a t -> 'b t
val mapleaf2 : 'c manager -> (Bdd.t -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val mapunop : 'b manager -> ('a -> 'b) -> 'a t -> 'b t
val mapbinop :
  commutative:bool ->
  'c manager -> ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val mapterop :
  'd manager -> ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
