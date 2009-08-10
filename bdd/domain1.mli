(** Boolean (abstract) domain with integrated environment *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Abstract domain} *)
(*  ********************************************************************** *)

type 'a t = 'a Expr1.Bool.t
  (** Abstract value *)
  
val size : 'a t -> int
  (** Size of an abstract value (number of nodes) *)
val print : Format.formatter -> 'a t -> unit
  
val bottom : 'a Env.t -> 'a t
val top : 'a Env.t -> 'a t
  (** Constructors *)
  
val is_bottom : 'a t -> bool
val is_top : 'a t -> bool
val is_leq : 'a t -> 'a t -> bool
val is_eq : 'a t -> 'a t -> bool
val is_variable_unconstrained : 'a t -> string -> bool
  (** Tests *)
  
val meet : 'a t -> 'a t -> 'a t
val join : 'a t -> 'a t -> 'a t
val meet_condition : 'a t -> 'a Expr1.Bool.t -> 'a t
  (** Lattice operations *)

val assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'a t -> string list -> 'a Expr1.t list -> 'a t
val assign_listexpr : ?relational:bool -> ?nodependency:bool -> 'a t -> string list -> 'a Expr1.List.t -> 'a t
  (** Assignement
      
      If [nodependency=true], which means that no expression depends on the
      assigned variables, it uses an optimized algorithm.
      
      If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
      a pair index. It is also advised to have paired variables in groups.
      
      [rel=true] is most probably much better for assignements of a few
      variables.  *)
val substitute_lexpr : 'a t -> string list -> 'a Expr1.t list -> 'a t
val substitute_listexpr : 'a t -> string list -> 'a Expr1.List.t -> 'a t
  (** Substitution *)
  
val forget_list : 'a t -> string list -> 'a t
  (** Eliminating variables *)
  
val change_environment : 'a t -> 'a Env.t -> 'a t
val rename :'a t -> (string*string) list -> 'a t
  (** Change of environments *)
  
(*  ********************************************************************** *)
(** {2 Opened signature and Internal functions} *)
(*  ********************************************************************** *)
  
(** We provide here the same functions and modules as before, but with opened
    types (this allows etxensions). The functions above are axtually derived from
    the functions below by just constraining their types.  We provide here also
    more internal functions *)
  
module O : sig
  
  val check_value :
    ('a -> int array -> 'a) ->
    (('b, 'c, 'd, 'e) Env.O.t, 'a) Env.value ->
    ('b, 'c, 'd, 'e) Env.O.t -> 'a
  val check_lvalue :
    ('a -> int array -> 'a) ->
    (('b, 'c, 'd, 'e) Env.O.t, 'a) Env.value list -> 
    ('b, 'c, 'd, 'e) Env.O.t -> 'a list
    
  type ('a,'b) t = ('a,'b) Expr1.O.Bool.t
  constraint 'a = ('c,'d,'b,'e) Env.O.t
    
  val size : ('a,'b) t -> int
  val print : Format.formatter -> ('a,'b) t -> unit
    
  val bottom : 'a -> ('a,'b) t
  val top : 'a -> ('a,'b) t
  (** Constructors *)
    
  val is_bottom : ('a,'b) t -> bool
  val is_top : ('a,'b) t -> bool
  val is_leq : ('a,'b) t -> ('a,'b) t -> bool
  val is_eq : ('a,'b) t -> ('a,'b) t -> bool
  val is_variable_unconstrained : ('a,'b) t -> string -> bool
  (** Tests *)
    
  val meet : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val join : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val meet_condition : ('a,'b) t -> ('a,'b) Expr1.O.Bool.t -> ('a,'b) t
  (** Lattice operations *)

  val assign_lexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b) t -> string list -> ('a,'b) Expr1.O.t list -> ('a,'b) t
  val assign_listexpr : ?relational:bool -> ?nodependency:bool -> ('a,'b) t -> string list -> ('a,'b) Expr1.O.List.t -> ('a,'b) t
  (** Assignement
      
      If [rel=true], it is assumed that [env#bddincr=2] (checked), starting from
      a pair index. It is also advised to have paired variables in groups.
      
      [rel=true] is most probably much better for assignements of a few
      variables.  *)
  val substitute_lexpr : ('a,'b) t -> string list -> ('a,'b) Expr1.O.t list -> ('a,'b) t
  val substitute_listexpr : ('a,'b) t -> string list -> ('a,'b) Expr1.O.List.t -> ('a,'b) t
  (** Substitution *)
    
  val forget_list : ('a,'b) t -> string list -> ('a,'b) t
  (** Eliminating variables *)
    
  val change_environment : ('a,'b) t -> 'a -> ('a,'b) t
  val rename :('a,'b) t -> (string*string) list -> ('a,'b) t
    (** Change of environments *)
    
end
  
