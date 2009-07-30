(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

type ('a,'b,'c,'d) man = {
  typ : 'b;
  man : 'c;
  size : 'c -> 'd -> int;
  print : Env.t -> Format.formatter -> 'd -> unit;
  bottom : 'c -> Env.t -> 'd;
  top : 'c -> Env.t -> 'd;
  of_apron : 'c -> Env.t -> 'a Apron.Abstract0.t -> 'd;
  is_bottom : 'c -> 'd -> bool;
  is_top : 'c -> 'd -> bool;
  is_leq : 'c -> 'd -> 'd -> bool;
  is_eq : 'c -> 'd -> 'd -> bool;
  to_bddapron : 'c -> 'd -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list;
  meet : 'c -> 'd -> 'd -> 'd;
  join : 'c -> 'd -> 'd -> 'd;
  meet_condition : 'c -> Env.t -> Cond.t -> 'd -> Expr0.Bool.t -> 'd;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'c ->  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd;
  substitute_lexpr : 'c ->  Env.t -> Cond.t -> 'd -> string list -> Expr0.t list -> 'd option -> 'd;
  forget_list : 'c -> Env.t -> 'd -> string list -> 'd;
  widening : 'c -> 'd -> 'd -> 'd;
}

let size man = man.size man.man
let print man = man.print
let bottom man = man.bottom man.man
let top man = man.top man.man
let of_apron man = man.of_apron man.man
let is_bottom man = man.is_bottom man.man
let is_top man = man.is_top man.man
let is_leq man = man.is_leq man.man
let is_eq man = man.is_eq man.man
let to_bddapron man = man.to_bddapron man.man
let meet man = man.meet man.man
let join man = man.join man.man
let meet_condition man = man.meet_condition man.man
let assign_lexpr ?relational ?nodependency man = man.assign_lexpr ?relational ?nodependency man.man
let substitute_lexpr man = man.substitute_lexpr man.man
let forget_list man = man.forget_list man.man
let widening man = man.widening man.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    [`Mtbdd of 'a Mtbdddomain0.man],
    'a Mtbdddomain0.man,
    'a Mtbdddomain0.t
  ) man

let make_mtbdd ?global (apron:'a Apron.Manager.t) : 'a mtbdd =
  let man = Mtbdddomain0.make_man ?global apron in
  {
    typ = `Mtbdd man;
    man = man;
    size = Mtbdddomain0.size;
    print = Mtbdddomain0.print;
    bottom = Mtbdddomain0.bottom;
    top = Mtbdddomain0.top;
    of_apron = Mtbdddomain0.of_apron;
    is_bottom = Mtbdddomain0.is_bottom;
    is_top = Mtbdddomain0.is_top;
    is_leq = Mtbdddomain0.is_leq;
    is_eq = Mtbdddomain0.is_eq;
    to_bddapron = Mtbdddomain0.to_bddapron;
    meet = Mtbdddomain0.meet;
    join = Mtbdddomain0.join;
    meet_condition = Mtbdddomain0.meet_condition;
    assign_lexpr = Mtbdddomain0.assign_lexpr;
    substitute_lexpr = Mtbdddomain0.substitute_lexpr;
    forget_list = Mtbdddomain0.forget_list;
    widening = Mtbdddomain0.widening
  }

let to_mtbdd (man:('a, [> `Mtbdd of 'a Mtbdddomain0.man], 'c, 'd) man) = match man.typ with
  | `Mtbdd man -> man
  | _ -> failwith ""

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    [`Bdd of 'a Bdddomain0.man],
    'a Bdddomain0.man,
    'a Bdddomain0.t
  ) man

let make_bdd (apron:'a Apron.Manager.t) : 'a bdd =
  let man = Bdddomain0.make_man apron in
  {
    typ = `Bdd man;
    man = man;
    size = Bdddomain0.size;
    print = Bdddomain0.print;
    bottom = Bdddomain0.bottom;
    top = Bdddomain0.top;
    of_apron = Bdddomain0.of_apron;
    is_bottom = Bdddomain0.is_bottom;
    is_top = Bdddomain0.is_top;
    is_leq = Bdddomain0.is_leq;
    is_eq = Bdddomain0.is_eq;
    to_bddapron = Bdddomain0.to_bddapron;
    meet = Bdddomain0.meet;
    join = Bdddomain0.join;
    meet_condition = Bdddomain0.meet_condition;
    assign_lexpr = Bdddomain0.assign_lexpr;
    substitute_lexpr = Bdddomain0.substitute_lexpr;
    forget_list = Bdddomain0.forget_list;
    widening = Bdddomain0.widening
  }

let to_bdd (man:('a, [> `Bdd of 'a Bdddomain0.man], 'c, 'd) man) = match man.typ with
  | `Bdd man -> man
  | _ -> failwith ""

(* *)

let cudd = Cudd.Man.make_v ();;
let env = Env.make cudd;;
let apron = Oct.manager_alloc ();;
let bdd = make_bdd apron;;
let mtbdd = make_mtbdd apron;;

let abs man = 
  begin
    try
      let bdd = to_bdd man in
      bdd.Bdddomain0.meet_disjoint <- false;
    with Failure _ -> ()
  end;
  begin
    try
      let mtbdd = to_mtbdd man in
      ()
    with Failure _ -> ()
  end;
  bottom man env
