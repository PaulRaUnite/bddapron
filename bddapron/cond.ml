(** Environments for [Bddapron] modules *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

type cond = [`Apron of Apronexpr.Condition.t]

let print_cond env fmt (cond:[< cond]) =
  match cond with
  | `Apron x -> Apronexpr.Condition.print fmt x

let compare_cond c1 c2 = match (c1,c2) with
  (`Apron c1, `Apron c2) -> Apronexpr.Condition.compare c1 c2

let negate_cond env (c:cond) : cond = match c with
  | `Apron x -> `Apron (Apronexpr.Condition.negate env x)

let cond_support env cond = match cond with
  | `Apron x -> Apronexpr.Condition.support x

module O = struct
  class type ['a,'b] t =
	  [[> cond] as 'a,
	  [> Apronexpr.typ ] #Apronexpr.env as 'b,
	  Cudd.Man.v]
	    Bdd.Cond.t

  class ['a,'b] make = [[> cond] as 'a,[> Apronexpr.typ ] #Apronexpr.env as 'b,Cudd.Man.v] Bdd.Cond.make

  let make = new make
  let (print: 'b -> Format.formatter -> ('a,'b) #t -> unit) = Bdd.Cond.print
end

class type t = [cond,Env.t] O.t

class make ?bddindex0 ?bddsize cudd =
  [cond,Env.t] O.make ?bddindex0 ?bddsize cudd
    ~compare_cond
    ~negate_cond
    ~support_cond:cond_support
    ~print_cond
let make = new make

let (print : Env.t -> Format.formatter -> t -> unit) = O.print
let (check_normalized: Env.t -> t -> bool) = Bdd.Cond.check_normalized
let (is_leq : t -> t -> bool) = Bdd.Cond.is_leq
let (is_eq : t -> t -> bool) = Bdd.Cond.is_eq
let (shift : t -> int -> t) = Bdd.Cond.shift
let (lce : t -> t -> t) = Bdd.Cond.lce
let (permutation12 : t -> t -> int array) = Bdd.Cond.permutation12
let (permutation21 : t -> t -> int array) = Bdd.Cond.permutation21

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}

let make_value = Bdd.Cond.make_value
