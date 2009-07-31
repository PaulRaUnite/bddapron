(** Enumerated expressions with BDDs *)

(* This file is part of the FORMULA Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

type label = string
  (** A label is just a name *)

type typ = [
  `Benum of string
]
  (** A type is just a name *)

type typdef = [
  `Benum of string array
]
  (** An enumerated type is defined by its (ordered) set of labels *)

(** {3 Datatype representing a BDD register of enumerated type} *)

type 'a t = {
  typ: string;
    (** Type of the value (refers to the database, see below) *)
  reg: 'a Reg.t
    (** Value itself *)
}

(** {3 Database} *)

(** We need a global store where we register type names with their type
  definitions, and also an auxiliary table to efficiently associate types to
  labels. *)

type ('a,'b,'c,'d) env0 = {
  cudd : 'c Cudd.Man.t;
    (** CUDD manager *)
  mutable typdef : (string, 'b) PMappe.t;
    (** Named types definitions *)
  mutable vartyp : (string, 'a) PMappe.t;
    (** Associate to a var/label its type *)
  mutable bddindex0 : int;
    (** First index for finite-type variables *)
  mutable bddsize : int;
    (** Number of indices dedicated to finite-type variables *)
  mutable bddindex : int;
    (** Next free index in BDDs used by [self#add_var]. *)
  bddincr : int;
    (** Increment used by [self#add_var] for incrementing
	[self#_bddindex] *)
  mutable idcondvar : (int, string) PMappe.t;
    (** Associates to a BDD index the variable involved by it *)
  mutable vartid : (string, int array) PMappe.t;
    (** (Sorted) array of BDD indices associated to finite-type variables. *)
  mutable varset : (string, 'c Cudd.Bdd.t) PMappe.t;
    (** Associates to enumerated variable the (care)set of
	possibled values. *)
  mutable print_external_idcondb : Format.formatter -> int*bool -> unit;
    (** Printing conditions not managed by the environment..
	By default, [pp_print_int]. *)
  mutable ext : 'd;
  copy_ext : 'd -> 'd;
}
type ('a,'b,'c,'d) env = ('a,'b,'c,'d) env0
constraint 'a = [>typ]
constraint 'b = [>typdef]

(*  ********************************************************************** *)
(** {2 Database} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Printing} *)
(*  ====================================================================== *)

let print_typdef (fmt:Format.formatter) (typdef:[>typdef]) : unit
  =
  begin match typdef with
  | `Benum tlabel ->
      Print.array pp_print_string fmt tlabel
  end

let print_typ (fmt:Format.formatter) (typ:[>typ]) : unit
  =
  begin match typ with
  | `Benum name ->
      pp_print_string fmt name
  end

(*  ====================================================================== *)
(** {3 Associations} *)
(*  ====================================================================== *)

let labels_of_typ env (typ:string) : string array
  =
  let typdef = PMappe.find typ env.typdef in
  begin match typdef with
  | `Benum tlabel -> tlabel
  | _ ->
      failwith (sprintf "Bddenum.labels_of_typ: type %s not defined as an enumerated type" typ)
  end

let size_of_typ env (typ:string) : int
  =
  let labels = labels_of_typ env typ in
  let nb = Array.length labels in
  Reg.min_size (nb-1)

let maxcode_of_typ env (typ:string) : int
  =
  let labels = labels_of_typ env typ in
  pred(Array.length labels)

let mem_typcode env (typ:string) (code:int) : bool
  =
  code <= maxcode_of_typ env typ

let findcode (label:string) (tlabels:string array) : int
  =
  let i = ref 0 in
  while !i < Array.length tlabels && tlabels.(!i) <> label do incr i done;
  if !i = Array.length tlabels then
    raise Not_found
  else
    !i

let typ_of_label (env:('a,'b,'c,'d) env) (label:string) : string
  =
  let typ = PMappe.find label env.vartyp in
  match typ with
  | `Benum typ -> typ
  | _ -> failwith ""

let code_of_label (env:('a,'b,'c,'d) env) (label:string) : int
  =
  let typ = typ_of_label env label in
  let labels = labels_of_typ env typ in
  let code = findcode label labels in
  code

let label_of_typcode (env:('a,'b,'c,'d) env) (typ:string) (code:int) : string
  =
  let t = labels_of_typ env typ in
  if code < Array.length t then
    t.(code)
  else
    failwith (sprintf "Bddenum.label_of_typcode: no label for typ=%s, code=%i"
      typ code)


(*  *********************************************************************** *)
(** {2 Constants and Operation(s)} *)
(*  *********************************************************************** *)

let of_label (env:('a,'b,'c,'d) env) (label:string) :'c t  =
  let typ = typ_of_label env label in
  let labels = labels_of_typ env typ in
  let size = size_of_typ env typ in
  let code = findcode label labels in
  let t = {
    typ = typ;
    reg = Reg.of_int env.cudd size code
  } in
(*
  printf "Bddenum.of_typlabel typ=%a label=%a code=%i@.reg = %a@."
    pp_print_string typ pp_print_string label code
    (Reg.print string_of_int) t.reg;
*)
  t

let is_cst (x:'c t) :bool
  =
  Reg.is_cst x.reg

let to_code (x:'c t) : int
  =
  Reg.to_int ~signed:false x.reg

let to_label (env:('a,'b,'c,'d) env) (x:'c t) : string
  =
  let code = to_code x in
  let label = label_of_typcode env x.typ code in
(*
  printf "to_label %a = %i,%a@."
    (Reg.print string_of_int) x.reg
    code pp_print_string label;
*)
  label

let equal_label (env:('a,'b,'c,'d) env) (x:'c t) (label:string) : 'c Cudd.Bdd.t
  =
  Reg.equal_int env.cudd x.reg (code_of_label env label)

let equal (env:('a,'b,'c,'d) env) (x:'c t) (y:'c t) : 'c Cudd.Bdd.t
  =
  if x.typ<>y.typ then
    failwith (sprintf "Bddenum.equal: applied between different types %s and %s" x.typ y.typ)
  ;
  Reg.equal env.cudd x.reg y.reg

let ite bdd a b =
  if a.typ <> b.typ then
    failwith (Format.sprintf "Bddenum.ite: types %s and %s are different" a.typ b.typ)
  ;
  { typ = a.typ;
    reg = Reg.ite bdd a.reg b.reg }

(*  *********************************************************************** *)
(** {2 Decomposition in guarded form} *)
(*  *********************************************************************** *)

module Minterm = struct

  let iter (env:('a,'b,'c,'d) env) (typ:string) (f:string -> unit) (minterm:Reg.Minterm.t) : unit =
    let maxcode = maxcode_of_typ env typ in
    Int.Minterm.iter ~signed:false
      (begin fun code ->
	if code<=maxcode then
	  f (label_of_typcode env typ code)
      end)
      minterm

  let map (env:('a,'b,'c,'d) env) (typ:string) (f:string -> 'd) (minterm:Reg.Minterm.t) : 'd list =
    let res = ref [] in
    let nf minterm = begin res := (f minterm) :: !res end in
    iter env typ nf minterm;
    List.rev !res

end

let guard_of_label (env:('a,'b,'c,'d) env) (x:'c t) (label:string) : 'c Cudd.Bdd.t
  =
  Reg.guard_of_int env.cudd x.reg (code_of_label env label)

let guardlabels (env:('a,'b,'c,'d) env) (x:'c t) : ('c Cudd.Bdd.t * string) list
  =
  let lguardints = Reg.guardints env.cudd ~signed:false x.reg in
  let maxcode = maxcode_of_typ env x.typ in
  let res =
    List.fold_left
      (begin fun res (bdd,code) ->
	if code<=maxcode then
	  (bdd, label_of_typcode env x.typ code)::res
	else
	  res
      end)
      []
      lguardints
  in
  List.rev res

(*  ********************************************************************** *)
(** {2 Evaluation} *)
(*  ********************************************************************** *)

let cofactor x bdd = { x with reg = Reg.cofactor x.reg bdd }
let restrict x bdd = { x with reg = Reg.restrict x.reg bdd }
let tdrestrict x bdd = { x with reg = Reg.tdrestrict x.reg bdd }

(*  ********************************************************************** *)
(** {2 Printing} *)
(*  ********************************************************************** *)

open Format

let print f fmt t =
  fprintf fmt "{ @[<hv>typ=%s;@ reg=%a@] }"
    t.typ
    (Reg.print f) t.reg

let print_minterm
  (print_bdd: Format.formatter -> 'c Cudd.Bdd.t -> unit)
  env
  fmt
  (x:'c t)
  =
  if is_cst x then begin
    let label = to_label env x in
    fprintf fmt "{ %s }" label
  end
  else begin
    let lguardlabels = guardlabels env x in
    Print.list ~first:"{ @[<v>" ~sep:"@; " ~last:"@] }"
      (fun fmt (guard,label) ->
	fprintf fmt "%s IF %a" label print_bdd guard)
      fmt
      lguardlabels
  end

let permute x tab = { x with reg = Reg.permute x.reg tab }
let vectorcompose tab x =  { x with reg = Reg.vectorcompose tab x.reg }
