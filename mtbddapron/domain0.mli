(** Combined Boolean/Numerical domain *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)
type 'a man =
  'a ApronDD.man = {
  apron : 'a Apron.Manager.t;
  table : 'a ApronDD.table;
  oglobal : 'a ApronDD.global option;
}
type 'a t = 'a ApronDD.t
val make_man : ?global:bool -> 'a Apron.Manager.t -> 'a man

val size : 'a man -> 'a t -> int
val print :
  ('a,'b) #Env.O.t ->
  Format.formatter -> 'c t -> unit
val bottom : 'a man -> Env.t -> 'a t
val top : 'a man -> Env.t -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet_condition :
  'a man -> Env.t -> Cond.t -> 'a t -> Expr0.Bool.t -> 'a t
val assign_lexpr :
  ?relational:bool -> ?nodependency:bool ->
  'a man -> Env.t -> Cond. t ->
  'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
val substitute_lexpr :
  'a man -> Env.t -> Cond.t ->
  'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
val forget_list :
  'a man -> Env.t -> 'a t -> string list -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val cofactors :
  'a man -> Env.t -> Cond. t -> 'a t -> int -> 'a t * 'a t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows extensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig
  val print :
    ('a,'b) #Env.O.t ->
    Format.formatter -> 'd t -> unit
  val bottom : 'a man -> ('b,'c) #Env.O.t -> 'a t
  val top : 'a man -> ('b,'c) #Env.O.t -> 'a t

  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val widening : 'a man -> 'a t -> 'a t -> 'a t
  val meet_idcondb :
    'a man -> (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t -> 'a t -> int * bool -> 'a t
  val cofactors :
    'a man -> (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t -> 'a t -> int -> 'a t * 'a t

  module Descend : sig
    val texpr_cofactor :
      Expr0.t array ->
      Expr0.Bool.t -> Expr0.t array
    val texpr_support :
      (Cond.cond,'d) #Cond.O.t -> 
      Expr0.t array -> Cudd.Man.v Cudd.Bdd.t
    val texpr_cofactors :
      < cudd : Cudd.Man.v Cudd.Man.t; .. > ->
       Expr0.t array ->
      int -> Expr0.t array * Expr0.t array
    val descend_arith :
      'a man ->
      (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t -> 
      ('a t -> Expr0.t array -> 'a t) ->
      'a t -> Expr0.t array -> 'a t
  end
  val meet_condition :
    'a man -> (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t ->
    'a t -> Expr0.Bool.t -> 'a t
  val split_lvarlexpr :
    string list ->
    Expr0.t list ->
    string list * Cudd.Man.v Bdd.Expr0.t list * 
      Apron.Var.t array * ApronexprDD.t array
  val assign_lexpr :
    ?relational:bool ->
    ?nodependency:bool ->
    'a man -> 
    (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val substitute_lexpr :
    'a man ->
    (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t ->
    'a t -> string list -> Expr0.t list -> 'a t option -> 'a t
  val forget_list :
    'd man ->
    ('a,'b) #Env.O.t ->
    'd t -> string list -> 'd t
end
