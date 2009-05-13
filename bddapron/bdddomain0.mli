
type 'a man = {
  apron : 'a Apron.Manager.t;
  mutable meet_canonical : bool;
  mutable join_canonical : bool;
}
type 'a elt = { mutable guard : Cudd.Bdd.vt; abs : 'a Apron.Abstract0.t; }
type 'a t = {
  mutable list : 'a elt list;
  bottom : 'a elt;
  mutable normalized : bool;
}

val canonicalize : 'a man -> 'a t -> unit
val size : 'a man -> 'a t -> int
val print : 
  ('a,'b) #Env.O.t ->
  Format.formatter -> 'c t -> unit
val bottom : 'a man -> ('a,'b) #Env.O.t -> 'a t
val top : 'a man -> ('a,'b) #Env.O.t -> 'a t
val is_bottom : 'a -> 'a t -> bool
val is_top : 'a man -> 'a t -> bool
val is_leq : 'a man -> 'a t -> 'a t -> bool
val is_eq : 'a man -> 'a t -> 'a t -> bool
val meet : 'a man -> 'a t -> 'a t -> 'a t
val join : 'a man -> 'a t -> 'a t -> 'a t
