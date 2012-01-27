(** Utility types and functions for normalization *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)


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

val conjunction_false : 'a conjunction
val conjunction_true : 'a conjunction
val disjunction_false : 'a disjunction
val disjunction_true : 'a disjunction
val cnf_false : 'a cnf
val cnf_true : 'a cnf
val dnf_false : 'a dnf
val dnf_true : 'a dnf

(*  ********************************************************************** *)
(** {3 Operations} *)
(*  ********************************************************************** *)

val conjunction_and :
  ?merge:('a list -> 'a list -> 'a list) ->
  'a conjunction -> 'a conjunction -> 'a conjunction
  (** Default merge is [List.rev_append] *)
val disjunction_or :
  ?merge:('a list -> 'a list -> 'a list) ->
  'a disjunction -> 'a disjunction -> 'a disjunction
  (** Default merge is [List.rev_append] *)

(** {4 Map functions} *)

val rev_map_conjunction : ('a -> 'b) -> 'a conjunction -> 'b conjunction
val rev_map_disjunction : ('a -> 'b) -> 'a disjunction -> 'b disjunction
val rev_map2_conjunction : ('a -> 'b -> 'c) -> 'a conjunction -> 'b conjunction -> 'c conjunction
val rev_map2_disjunction : ('a -> 'b -> 'c) -> 'a disjunction -> 'b disjunction -> 'c disjunction
val map_conjunction : ('a -> 'b) -> 'a conjunction -> 'b conjunction
val map_disjunction : ('a -> 'b) -> 'a disjunction -> 'b disjunction
val map_cnf : ('a -> 'b) -> 'a cnf -> 'b cnf
val map_dnf : ('a -> 'b) -> 'a dnf -> 'b dnf

(*  ********************************************************************** *)
(** {3 Printing functions} *)
(*  ********************************************************************** *)

val print_conjunction :
  ?firstconj:(unit,Format.formatter,unit) format ->
  ?sepconj:(unit,Format.formatter,unit) format ->
  ?lastconj:(unit,Format.formatter,unit) format ->
   (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a conjunction -> unit
val print_disjunction :
  ?firstdisj:(unit,Format.formatter,unit) format ->
  ?sepdisj:(unit,Format.formatter,unit) format ->
  ?lastdisj:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a disjunction -> unit
val print_cnf :
  ?firstconj:(unit,Format.formatter,unit) format ->
  ?sepconj:(unit,Format.formatter,unit) format ->
  ?lastconj:(unit,Format.formatter,unit) format ->
  ?firstdisj:(unit,Format.formatter,unit) format ->
  ?sepdisj:(unit,Format.formatter,unit) format ->
  ?lastdisj:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a cnf -> unit
val print_dnf :
  ?firstdisj:(unit,Format.formatter,unit) format ->
  ?sepdisj:(unit,Format.formatter,unit) format ->
  ?lastdisj:(unit,Format.formatter,unit) format ->
  ?firstconj:(unit,Format.formatter,unit) format ->
  ?sepconj:(unit,Format.formatter,unit) format ->
  ?lastconj:(unit,Format.formatter,unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a dnf -> unit