
type 'a man = {
  apron : 'a Apron.Manager.t;
  mutable bddrestrict : Cudd.Bdd.vt -> Cudd.Bdd.vt -> Cudd.Bdd.vt;
  mutable meet_exclusive : bool;
  mutable join_exclusive : bool;
  mutable meet_cond_nodoublon : bool;
  mutable meet_cond_exclusive : bool;
  mutable meet_cond_depth : int;
}

type 'a elt = { guard : Cudd.Bdd.vt; abs : 'a Apron.Abstract0.t; }
type 'a t = {
  mutable list : 'a elt list;
  bottom : 'a elt;
  mutable nodoublon : bool;
  mutable exclusive : bool;
}

val canonicalize : ?nodoublon:bool -> ?exclusive:bool -> 'a man -> 'a t -> unit
val size : 'a man -> 'a t -> int
val print : 
  ('a,'b) #Env.O.t ->
  Format.formatter -> 'c t -> unit
val bottom : 'a man -> ('b,'c) #Env.O.t -> 'a t
val top : 'a man -> ('b,'c) #Env.O.t -> 'a t
val is_bottom : 'a -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
val meet_cond : 'a man -> (('b,'c) #Env.O.t as 'd) -> (Cond.cond,'d) #Cond.O.t -> 'a t -> Expr0.Bool.t -> 'a t
