(** Utility types and functions for normalization *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {3 Types} *)
(*  ********************************************************************** *)

(** Conjunction *)
type 'a conjunction =
  | Conjunction of 'a list
      (** Conjunction of terms. Empty list means true. *)
  | Cfalse

(** Disjunction *)
type 'a disjunction =
  | Disjunction of 'a list
      (** Disjunction of conjunctions. Empty list means false *)
  | Dtrue

(** CNF *)
type 'a cnf = 'a disjunction conjunction
(** DNF *)
type 'a dnf = 'a conjunction disjunction

(*  ********************************************************************** *)
(** {3 Constants} *)
(*  ********************************************************************** *)

let conjunction_false = Cfalse
let conjunction_true = Conjunction []
let disjunction_false = Disjunction []
let disjunction_true = Dtrue
let (cnf_false:'a cnf) = conjunction_false
let (cnf_true:'a cnf) = conjunction_true
let (dnf_false:'a dnf) = disjunction_false
let (dnf_true:'a dnf) = disjunction_true

(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

let conjunction_and ?(merge=List.rev_append) c1 c2 =
  match (c1,c2) with
  | (Conjunction []), (Conjunction l) -> c2
  | (Conjunction l), (Conjunction []) -> c1
  | (Conjunction l1, Conjunction l2) -> Conjunction (merge l1 l2)
  | _ -> Cfalse

let disjunction_or ?(merge=List.rev_append) c1 c2 =
  match (c1,c2) with
  | (Disjunction []), (Disjunction l) -> c2
  | (Disjunction l), (Disjunction []) -> c1
  | (Disjunction l1, Disjunction l2) -> Disjunction (merge l1 l2)
  | _ -> Dtrue

let rev_map_conjunction f = function
  | Conjunction l -> Conjunction (List.rev_map f l)
  | Cfalse -> Cfalse
let rev_map_disjunction f = function
  | Disjunction l -> Disjunction (List.rev_map f l)
  | Dtrue -> Dtrue
let rev_map2_conjunction f c1 c2 =
  match (c1,c2) with
  | (Conjunction l1),(Conjunction l2) -> Conjunction (List.rev_map2 f l1 l2)
  | Cfalse,Cfalse -> Cfalse
  | _ -> raise (Invalid_argument "Bdd.Normalform.rev_map2_conjunction")
let rev_map2_disjunction f d1 d2 =
  match (d1,d2) with
  | (Disjunction l1),(Disjunction l2) -> Disjunction (List.rev_map2 f l1 l2)
  | Dtrue,Dtrue -> Dtrue
  | _ -> raise (Invalid_argument "Bdd.Normalform.rev_map2_conjunction")
let map_conjunction f = function
  | Conjunction l -> Conjunction (List.map f l)
  | Cfalse -> Cfalse
let map_disjunction f = function
  | Disjunction l -> Disjunction (List.map f l)
  | Dtrue -> Dtrue
let map_cnf f cnf =
  map_conjunction (map_disjunction f) cnf
let map_dnf f dnf =
  map_disjunction (map_conjunction f) dnf

(*  ********************************************************************** *)
(** {3 Printing functions} *)
(*  ********************************************************************** *)

let print_conjunction
    ?(firstconj=("@[<hov>":(unit,Format.formatter,unit) format))
    ?(sepconj=("@ and ":(unit,Format.formatter,unit) format))
    ?(lastconj=("@]":(unit,Format.formatter,unit) format))
    print fmt
    =
  function
    | Cfalse -> pp_print_string fmt "false"
    | Conjunction([]) -> pp_print_string fmt "true"
    | Conjunction(l) ->
	Print.list ~first:firstconj ~sep:sepconj ~last:lastconj print fmt l
let print_disjunction
    ?(firstdisj=("@[<hov>":(unit,Format.formatter,unit) format))
    ?(sepdisj=("@ or ":(unit,Format.formatter,unit) format))
    ?(lastdisj=("@]":(unit,Format.formatter,unit) format))
    print fmt
    =
  function
    | Dtrue -> pp_print_string fmt "true"
    | Disjunction([]) -> pp_print_string fmt "false"
    | Disjunction(l) ->
	Print.list ~first:firstdisj ~sep:sepdisj ~last:lastdisj print fmt l

let print_cnf
    ?(firstconj=("@[<hv>":(unit,Format.formatter,unit) format))
    ?sepconj
    ?lastconj
    ?firstdisj
    ?sepdisj
    ?lastdisj
    print fmt conjunction
    =
  print_conjunction
    ~firstconj ?sepconj ?lastconj
    (print_disjunction ?firstdisj ?sepdisj ?lastdisj print)
    fmt conjunction

let print_dnf
    ?(firstdisj=("@[<hv>":(unit,Format.formatter,unit) format))
    ?sepdisj
    ?lastdisj
    ?firstconj
    ?sepconj
    ?lastconj
    print fmt disjunction
    =
  print_disjunction
    ~firstdisj ?sepdisj ?lastdisj
    (print_conjunction ?firstconj ?sepconj ?lastconj print)
    fmt disjunction
