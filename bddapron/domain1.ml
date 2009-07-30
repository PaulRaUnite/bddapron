(** Combined Boolean/Numerical domain *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

(** {3 Generic manager (normally a private type)} *)

type ('a,'b,'c,'d) man = {
  typ : 'b;
  man : 'c;
  size : 'c -> 'd -> int;
  print : Format.formatter -> 'd -> unit;
  bottom : 'c -> Env.t -> 'd;
  top : 'c -> Env.t -> 'd;
  of_apron : 'c -> Env.t -> 'a Apron.Abstract1.t -> 'd;
  is_bottom : 'c -> 'd -> bool;
  is_top : 'c -> 'd -> bool;
  is_leq : 'c -> 'd -> 'd -> bool;
  is_eq : 'c -> 'd -> 'd -> bool;
  to_bddapron : 'c -> 'd -> (Expr1.Bool.t * 'a Apron.Abstract1.t) list;
  meet : 'c -> 'd -> 'd -> 'd;
  join : 'c -> 'd -> 'd -> 'd;
  meet_condition : 'c -> Cond.t -> 'd -> Expr1.Bool.t -> 'd;
  meet_condition2 : 'c -> 'd -> Expr2.Bool.t -> 'd;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'c ->  Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd;
  substitute_lexpr : 'c -> Cond.t -> 'd -> string list -> Expr1.t list -> 'd option -> 'd;
  assign_listexpr2 : ?relational:bool -> ?nodependency:bool -> 'c -> 'd -> string list -> Expr2.List.t -> 'd option -> 'd;
  substitute_listexpr2 : 'c -> 'd -> string list -> Expr2.List.t -> 'd option -> 'd;
  forget_list : 'c -> 'd -> string list -> 'd;
  widening : 'c -> 'd -> 'd -> 'd;
}

(** {3 Functions} *)

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
let meet_condition2 man = man.meet_condition2 man.man
let assign_lexpr ?relational ?nodependency man = man.assign_lexpr ?relational ?nodependency man.man
let substitute_lexpr man = man.substitute_lexpr man.man
let assign_listexpr2 ?relational ?nodependency man = man.assign_listexpr2 ?relational ?nodependency man.man
let substitute_listexpr2 man = man.substitute_listexpr2 man.man
let forget_list man = man.forget_list man.man
let widening man = man.widening man.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain1}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    [`Mtbdd of 'a Mtbdddomain1.man],
    'a Mtbdddomain1.man,
    'a Mtbdddomain1.t
  ) man

let make_mtbdd ?global (apron:'a Apron.Manager.t) : 'a mtbdd =
  let man = Mtbdddomain0.make_man ?global apron in
  {
    typ = `Mtbdd man;
    man = man;
    size = Mtbdddomain1.size;
    print = Mtbdddomain1.print;
    bottom = Mtbdddomain1.bottom;
    top = Mtbdddomain1.top;
    of_apron = Mtbdddomain1.of_apron;
    is_bottom = Mtbdddomain1.is_bottom;
    is_top = Mtbdddomain1.is_top;
    is_leq = Mtbdddomain1.is_leq;
    is_eq = Mtbdddomain1.is_eq;
    to_bddapron = Mtbdddomain1.to_bddapron;
    meet = Mtbdddomain1.meet;
    join = Mtbdddomain1.join;
    meet_condition = Mtbdddomain1.meet_condition;
    meet_condition2 = Mtbdddomain1.meet_condition2;
    assign_lexpr = Mtbdddomain1.assign_lexpr;
    substitute_lexpr = Mtbdddomain1.substitute_lexpr;
    assign_listexpr2 = Mtbdddomain1.assign_listexpr2;
    substitute_listexpr2 = Mtbdddomain1.substitute_listexpr2;
    forget_list = Mtbdddomain1.forget_list;
    widening = Mtbdddomain1.widening
  }

let to_mtbdd (man:('a, [> `Mtbdd of 'a Mtbdddomain1.man], 'c, 'd) man) = match man.typ with
  | `Mtbdd man -> man
  | _ -> failwith ""

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain1}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    [`Bdd of 'a Bdddomain1.man],
    'a Bdddomain1.man,
    'a Bdddomain1.t
  ) man

let make_bdd (apron:'a Apron.Manager.t) : 'a bdd =
  let man = Bdddomain0.make_man apron in
  {
    typ = `Bdd man;
    man = man;
    size = Bdddomain1.size;
    print = Bdddomain1.print;
    bottom = Bdddomain1.bottom;
    top = Bdddomain1.top;
    of_apron = Bdddomain1.of_apron;
    is_bottom = Bdddomain1.is_bottom;
    is_top = Bdddomain1.is_top;
    is_leq = Bdddomain1.is_leq;
    is_eq = Bdddomain1.is_eq;
    to_bddapron = Bdddomain1.to_bddapron;
    meet = Bdddomain1.meet;
    join = Bdddomain1.join;
    meet_condition = Bdddomain1.meet_condition;
    meet_condition2 = Bdddomain1.meet_condition2;
    assign_lexpr = Bdddomain1.assign_lexpr;
    substitute_lexpr = Bdddomain1.substitute_lexpr;
    assign_listexpr2 = Bdddomain1.assign_listexpr2;
    substitute_listexpr2 = Bdddomain1.substitute_listexpr2;
    forget_list = Bdddomain1.forget_list;
    widening = Bdddomain1.widening
  }

let to_bdd (man:('a, [> `Bdd of 'a Bdddomain1.man], 'c, 'd) man) = match man.typ with
  | `Bdd man -> man
  | _ -> failwith ""
