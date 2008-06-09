(** Combined Boolean/Numerical domain *)

type env = Bddapronexpr.env
type 'a manager = {
  apron: 'a Apron.Manager.t;
  aprondd : 'a Apron.Abstract1.t Mtbdd2.manager;
}
type 'a t = 'a Apron.Abstract1.t Mtbdd2.t

val make_manager : 'a Apron.Manager.t -> 'a manager
val size : 'd manager -> 'd t -> int

val rename_vars : env -> (string*string) list -> env

val print : env -> Format.formatter -> 'd t -> unit
val bottom : 'a manager -> env -> 'a t
val top : 'a manager -> env -> 'a t
val is_bottom : 'a manager -> 'a t -> bool
val is_top : 'a manager -> 'a t -> bool
val is_leq : 'a manager -> 'a t -> 'a t -> bool
val is_eq : 'a manager -> 'a t -> 'a t -> bool

val meet : 'a manager -> 'a t -> 'a t -> 'a t
val join : 'a manager -> 'a t -> 'a t -> 'a t
val meet_cond : 'a manager -> env  -> 'a t -> Bddapronexpr.Bool.t -> 'a t

val assign_list :
  'a manager -> env ->
  'a t -> (string * Bddapronexpr.expr) list -> 'a t option -> 'a t
val substitute_list :
  'a manager -> env ->
  'a t -> (string * Bddapronexpr.expr) list -> 'a t option -> 'a t
val forget_list : 'a manager -> env -> 'a t -> string list -> 'a t

val widening : 'a manager -> 'a t -> 'a t -> 'a t

val cofactors : 'a manager -> env -> 'a t -> int -> 'a t * 'a t


(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)

(** We provide here the same functions and modules as before, but with opened
  types (this allows extensions). The functions above are actually derived from
  the functions below by just constraining their types.  We provide here also
  more internal functions *)

module O : sig

val rename_vars :
  (('a,'b,'c) #Bddapronexpr.O.env as 'd) ->
  (string*string) list ->
  'd

val print : ('a,'b,'c) #Bddenv.t -> Format.formatter -> 'd t -> unit
val bottom : 'd manager -> ('a,'b,'c) #Bddapronexpr.O.env -> 'd t
val top : 'd manager -> ('a,'b,'c) #Bddapronexpr.O.env -> 'd t
val is_bottom : 'a manager -> 'a t -> bool
val is_top : 'a manager -> 'a t -> bool
val is_leq : 'a manager -> 'a t -> 'a t -> bool
val is_eq : 'a manager -> 'a t -> 'a t -> bool

val meet : 'a manager -> 'a t -> 'a t -> 'a t
val join : 'a manager -> 'a t -> 'a t -> 'a t
val meet_cond : 'a manager -> ('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env  -> 'a t -> Bddapronexpr.Bool.t -> 'a t

val assign_list :
  'a manager ->
  ('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env ->
  'a t -> (string * Bddapronexpr.expr) list -> 'a t option -> 'a t
val substitute_list :
  'a manager ->
  ('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env ->
  'a t -> (string * Bddapronexpr.expr) list -> 'a t option -> 'a t
val forget_list :
  'a manager -> ('b,'c,'d) #Bddapronexpr.O.env -> 'a t -> string list -> 'a t

val widening : 'a manager -> 'a t -> 'a t -> 'a t

val cofactors :
  'a manager ->
  ('b,'c,Bddapronexpr.cond) #Bddapronexpr.O.env -> 'a t -> int -> 'a t * 'a t

module Descend : sig
  val descend :
   'a manager ->
    ([> Bddapronexpr.typ ], [> Bddapronexpr.typdef ], Bddapronexpr.cond)
    #Bddapronexpr.O.env ->
    ('a t -> Bddexpr.expr list -> Apronexpr.expr array -> 'a t option -> 'a t) ->
    'a t -> Bddapronexpr.expr array -> 'a t option -> 'a t
  val descend_cond :
   'a manager ->
    ([> Bddapronexpr.typ ], [> Bddapronexpr.typdef ], Bddapronexpr.cond)
    #Bddapronexpr.O.env ->
    ('a t -> Bddapronexpr.Bool.t -> 'a t) ->
    'a t -> Bddapronexpr.Bool.t -> 'a t
end
module Asssub : sig
  val lbvar_tavar_texpr_of_sub :
    (string * Bddapronexpr.expr) list ->
    string list * Apron.Var.t array * Bddapronexpr.expr array
end
module Internal : sig
  val rename_vars :
    (('a,'b,'c) #Bddapronexpr.O.env as 'e) ->
    (string*string) list ->
    'e * int array
end

end
