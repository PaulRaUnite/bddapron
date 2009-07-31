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

let negate_cond (env:('a,'b,'c) Env.O.t) (c:cond) : cond = match c with
  | `Apron x -> 
      `Apron (Apronexpr.Condition.negate (Env.typ_of_var env) x)

let cond_support env cond = match cond with
  | `Apron x -> Apronexpr.Condition.support x

module O = struct
  type 'a t = (cond,'a,Cudd.Man.v) Bdd.Cond.t
  constraint 'a = ('b,'c,'d) Env.O.t
    
  let make ?bddindex0 ?bddsize (cudd:Cudd.Man.vt) : 'a t =
    Bdd.Cond.make ?bddindex0 ?bddsize cudd 
      ~compare_cond
      ~negate_cond
      ~support_cond:cond_support
      ~print_cond
end

type t = Env.t O.t

let make = O.make
let copy = Bdd.Cond.copy
let print = Bdd.Cond.print
let cond_of_idb = Bdd.Cond.cond_of_idb
let idb_of_cond = Bdd.Cond.idb_of_cond
let support_cond = cond_support
let check_normalized = Bdd.Cond.check_normalized
let is_leq = Bdd.Cond.is_leq
let is_eq = Bdd.Cond.is_eq
let shift = Bdd.Cond.shift
let lce = Bdd.Cond.lce
let permutation12 = Bdd.Cond.permutation12
let permutation21 = Bdd.Cond.permutation21

type ('a,'b) value = ('a,'b) Bdd.Cond.value = {
  cond : 'a;
  val1 : 'b
}

let make_value = Bdd.Cond.make_value

