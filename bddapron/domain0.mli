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
val make_env :
  ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t -> Env.t

val size : 'a man -> 'a t -> int
val print :
  ('a,'b,'c) #Env.O.t ->
  Format.formatter -> 'd t -> unit
val bottom : 'a man -> Env.t -> 'a t
val top : 'a man -> Env.t -> 'a t
val is_bottom : 'a man -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet_cond :
  'a man -> Env.t -> 'a t -> Expr0.Bool.t -> 'a t
val assign_list :
  ?relational:bool -> ?nodependency:bool ->
  'a man -> Env.t ->
  'a t -> (string * Expr0.t) list -> 'a t option -> 'a t
val substitute_list :
  'a man -> Env.t ->
  'a t -> (string * Expr0.t) list -> 'a t option -> 'a t
val forget_list :
  'a man -> Env.t -> 'a t -> string list -> 'a t
val widening : 'a man -> 'a t -> 'a t -> 'a t
val cofactors :
  'a man -> Env.t -> 'a t -> int -> 'a t * 'a t

(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows extensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig
  val make_env :
    ?boolfirst:bool -> ?relational:bool -> Cudd.Man.v Cudd.Man.t ->
    ('a, 'b, Env.cond) Env.O.t

  val print :
    ('a,'b,'c) #Env.O.t ->
    Format.formatter -> 'd t -> unit
  val bottom :
    'a man ->
    < apron_env : Apron.Environment.t; cudd : Cudd.Man.v Cudd.Man.t; .. > ->
      'a t
  val top :
    'a man ->
    < apron_env : Apron.Environment.t; cudd : Cudd.Man.v Cudd.Man.t; .. > ->
      'a t
  val is_bottom : 'a man -> 'a t -> bool
  val is_top : 'a man -> 'a t -> bool
  val is_leq : 'a man -> 'a t -> 'a t -> bool
  val is_eq : 'a man -> 'a t -> 'a t -> bool
  val meet : 'a man -> 'a t -> 'a t -> 'a t
  val join : 'a man -> 'a t -> 'a t -> 'a t
  val widening :
    'a man -> 'a t -> 'a t -> 'a t
  val meet_idcondb :
    'a man ->
    ([> Env.typ ], [> Env.typdef ], Env.cond)
      #Env.O.t -> 'a t -> MappeI.key * bool -> 'a t
  val cofactors :
    'a man ->
    ([> Env.typ ], [> Env.typdef ], Env.cond)
      #Env.O.t -> 'a t -> MappeI.key -> 'a t * 'a t

  module Descend : sig
    val texpr_cofactor :
      Expr0.t array ->
      Expr0.Bool.t -> Expr0.t array
    val texpr_support :
      Env.t -> Expr0.t array -> Cudd.Man.v Cudd.Bdd.t
    val texpr_cofactors :
      < cudd : Cudd.Man.v Cudd.Man.t; .. > ->
        Expr0.t array ->
          int -> Expr0.t array * Expr0.t array
    val descend_arith :
      'a man ->
      Env.t ->
      ('a t -> Expr0.t array -> 'a t) ->
      'a t -> Expr0.t array -> 'a t
  end
  val meet_cond :
    'a man -> Env.t -> 'a t -> Expr0.Bool.t -> 'a t
  val rev_split : ('a * 'b) list -> 'a list * 'b list
  val split_lvarlexpr :
    string list ->
    Expr0.t list ->
    (string * Cudd.Man.v Bdd.Expr0.t) list * Apron.Var.t array *
      ApronexprDD.t array
  val assign_list :
    ?relational:bool ->
    ?nodependency:bool ->
    'a man ->
    Env.t ->
    'a t ->
    (string * Expr0.t) list -> 'a t option -> 'a t
  val substitute_list :
    'a man ->
    Env.t ->
    'a t -> (string * Expr0.t) list -> 'a t option -> 'a t
  val forget_list :
    'd man ->
    ('a,'b,'c,Cudd.Man.v) #Bdd.Env.O.t ->
    'd t -> string list -> 'd t
end
