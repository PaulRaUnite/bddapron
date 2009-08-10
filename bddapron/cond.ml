(** Normalized condition environments *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
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

let support_cond env cond = match cond with
  | `Apron x -> Apronexpr.Condition.support x

module O = struct
  type 'a t = (cond,'a,Cudd.Man.v) Bdd.Cond.t
  constraint 'a = ('b,'c,'d) Env.O.t
    
  let make ?bddindex0 ?bddsize (cudd:Cudd.Man.vt) : 'a t =
    Bdd.Cond.make ?bddindex0 ?bddsize cudd 
      ~compare_cond
      ~negate_cond
      ~support_cond
      ~print_cond
end

type t = Env.t O.t

let make = O.make

