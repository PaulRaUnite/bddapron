(** Boolean/Numerical domain: generic interface *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(*  ********************************************************************** *)
(** {2 Generic interface} *)
(*  ********************************************************************** *)

type ('a,'b,'c) man = {
  typ : string;
  man : 'b;
  canonicalize : ?apron:bool -> 'b -> 'c -> unit;
  size : 'b -> 'c -> int;
  print : Env.t -> Format.formatter -> 'c -> unit;
  bottom : 'b -> Env.t -> 'c;
  top : 'b -> Env.t -> 'c;
  of_apron : 'b -> Env.t -> 'a Apron.Abstract0.t -> 'c;
  is_bottom : 'b -> 'c -> bool;
  is_top : 'b -> 'c -> bool;
  is_leq : 'b -> 'c -> 'c -> bool;
  is_eq : 'b -> 'c -> 'c -> bool;
  to_bddapron : 'b -> 'c -> (Expr0.Bool.t * 'a Apron.Abstract0.t) list;
  meet : 'b -> 'c -> 'c -> 'c;
  join : 'b -> 'c -> 'c -> 'c;
  meet_condition : 'b -> Env.t -> Cond.t -> 'c -> Expr0.Bool.t -> 'c;
  assign_lexpr : ?relational:bool -> ?nodependency:bool -> 'b ->  Env.t -> Cond.t -> 'c -> string list -> Expr0.t list -> 'c option -> 'c;
  substitute_lexpr : 'b ->  Env.t -> Cond.t -> 'c -> string list -> Expr0.t list -> 'c option -> 'c;
  forget_list : 'b -> Env.t -> 'c -> string list -> 'c;
  widening : 'b -> 'c -> 'c -> 'c;
  apply_change :  bottom:'c -> 'b -> 'c -> Env.change -> 'c;
  apply_permutation : 'b -> 'c -> int array option * Apron.Dim.perm option -> 'c;
}
(** Type of generic managers.

    - ['a]: as in ['a Apron.Manager.t]
	    ([Box.t], [Polka.strict Polka.t], etc);
    - ['b]: type of the underlying manager;
    - ['c]: type of the underlying abstract values of level 0.
*)

type 'c t = 'c
(** Type of generic abstract values *)

let canonicalize ?apron man = man.canonicalize ?apron man.man
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
let apply_change ~bottom man = man.apply_change ~bottom man.man
let apply_permutation man = man.apply_permutation man.man

(*  ********************************************************************** *)
(** {2 Implementation based on {!Mtbdddomain0}} *)
(*  ********************************************************************** *)

type 'a mtbdd =
  (
    'a,
    'a Mtbdddomain0.man,
    'a Mtbdddomain0.t
  ) man

let make_mtbdd ?global (apron:'a Apron.Manager.t) : 'a mtbdd =
  let man = Mtbdddomain0.make_man ?global apron in
  {
    typ = "mtbdd";
    man = man;
    canonicalize = (fun ?apron _ _ -> ());
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
    widening = Mtbdddomain0.widening;
    apply_change = Mtbdddomain0.apply_change;
    apply_permutation = Mtbdddomain0.apply_permutation;
  }

let man_of_mtbdd (man:'a mtbdd) : ('a,'b,'c) man =
  Obj.magic man
let of_mtbdd (manabs:'a mtbdd * 'a Mtbdddomain0.t) : ('a,'b,'c) man * 'c t =
  Obj.magic manabs
    
let man_is_mtbdd man = 
  man.typ="mtbdd"

let man_to_mtbdd (man:('a,'b,'c) man) : 'a mtbdd =
  if man_is_mtbdd man then
    Obj.magic man
  else
    failwith ""
let to_mtbdd (manabs:('a,'b,'c) man * 'c t) : 'a mtbdd * 'a Mtbdddomain0.t =
  if man_is_mtbdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""

(*  ********************************************************************** *)
(** {2 Implementation based on {!Bdddomain0}} *)
(*  ********************************************************************** *)

type 'a bdd =
  (
    'a,
    'a Bdddomain0.man,
    'a Bdddomain0.t
  ) man

let make_bdd (apron:'a Apron.Manager.t) : 'a bdd =
  let man = Bdddomain0.make_man apron in
  {
    typ = "bdd";
    man = man;
    canonicalize = (Bdddomain0.canonicalize ~unique:true ~disjoint:true);
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
    widening = Bdddomain0.widening;
    apply_change = Bdddomain0.apply_change;
    apply_permutation = Bdddomain0.apply_permutation;
  }

let man_of_bdd (man:'a bdd) : ('a,'b,'c) man =
  Obj.magic man
let of_bdd (manabs:'a bdd * 'a Bdddomain0.t) : ('a,'b,'c) man * 'c t =
  Obj.magic manabs

let man_is_bdd man = 
  man.typ="bdd"

let man_to_bdd (man:('a,'b,'c) man) : 'a bdd =
  if man_is_bdd man then
    Obj.magic man
  else
    failwith ""
let to_bdd (manabs:('a,'b,'c) man * 'c t) : 'a bdd * 'a Bdddomain0.t =
  if man_is_bdd (fst manabs) then
    Obj.magic manabs
  else
    failwith ""

(*
let cudd = Cudd.Man.make_v ();;
let env = Env.make cudd;;
let apron = Oct.manager_alloc ();;

let make () =
  let man =
    if true then 
      man_of_bdd (make_bdd apron)
    else
      man_of_mtbdd (make_mtbdd apron)
  in
  let bottom = bottom man env in
  ((man,bottom),to_bdd (man,bottom))

let bdd = make_bdd apron;;
let mtbdd = make_mtbdd apron;;

let abs man =
  begin
    try
      let bdd = man_to_bdd man in
      bdd.man.Bdddomain0.meet_disjoint <- false;
    with Failure _ -> ()
  end;
  begin
    try
      let mtbdd = man_to_mtbdd man in
      ()
    with Failure _ -> ()
  end;
  bottom man env
*)
