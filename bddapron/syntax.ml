(** Abstract syntax tree for BDDAPRON expressions *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

open Format

(*  ********************************************************************** *)
(** {2 Types} *)
(*  ********************************************************************** *)

(** Constant *)
type cst = [
  | `Bool of bool
  | `Bint of (bool * int) * int
  | `Apron of Apron.Coeff.t
]

(** Unary operators *)
type unop = [
| `Not
| `Apron of Apron.Texpr1.unop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Boolean/finite-type binary operators *)
type bbinop = Or | And | EQ | NEQ | GT | GEQ | LEQ | LT

(** Binary operators *)
type binop = [
| `Bool of bbinop
| `Apron of Apron.Texpr1.binop * Apron.Texpr1.typ * Apron.Texpr1.round
]

(** Expressions *)
type expr =
  | Cst of cst
  | Ref of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | If of expr * expr * expr
  | In of expr * expr list

exception Error of string

(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)

let error format =
  let buffer = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      raise (Error s)
    end)
    fmt
    format

let print_typdef fmt (typdef:string Env.typdef) =
  match typdef with
  | `Benum array ->
      fprintf fmt "enum { %a }"
      (Print.array ~first:"@[" ~sep:",@ " ~last:"@]" pp_print_string)
      array
  | `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size
let print_typ fmt (typ:[<string Env.typ]) =
  match typ with
  | `Bool -> pp_print_string fmt "bool"
  | `Bint(sign,size) -> fprintf fmt "%cint[%i]" (if sign then 's' else 'u') size
  | `Benum s -> pp_print_string fmt s
  | `Int -> pp_print_string fmt "int"
  | `Real -> pp_print_string fmt "real"

let print_cst fmt (cst:cst)
  =
  match cst with
  | `Bool(b) -> pp_print_bool fmt b
  | `Apron(x) -> Apron.Coeff.print fmt x
  | `Bint((sign,size),c) ->
      fprintf fmt "%cint[%i](%i)" (if sign then 's' else 'u') size c

let print_unop fmt (op:unop) : unit =
  pp_print_string fmt
    begin match op with
    | `Not ->  "not"
    | `Apron (unop,typ,round) ->
	Apron.Texpr0.print_sprint_unop unop typ round
    end

let print_bbinop fmt (op:bbinop) : unit =
  pp_print_string fmt
    begin match op with
    | Or -> "or"
    | And -> "and"
    | EQ -> "=="
    | NEQ -> "!="
    | GT -> ">"
    | GEQ -> ">="
    | LEQ -> "<="
    | LT -> "<"
    end

let print_binop fmt (op:binop) =
  match op with
  | `Bool op -> print_bbinop fmt op
  | `Apron (op,typ,round) ->
      pp_print_string fmt
	(Apron.Texpr0.print_sprint_binop op typ round)

let is_zero (e:expr) =
  match e with
  | Cst(`Apron x) -> Apron.Coeff.is_zero x
  | _ -> false

let precedence_of_unop = function
  | `Not -> 4
  | `Apron(_,_,_) -> 8
let precedence_of_binop = function
  | `Bool op ->
      begin match op with
      | Or -> 1
      | And -> 2
      | EQ | NEQ -> 3
      | GT | GEQ | LEQ | LT -> 5
      end
  | `Apron(op,_,_) ->
      begin match op with
      | Apron.Texpr1.Add | Apron.Texpr1.Sub -> 6
      | Apron.Texpr1.Mul | Apron.Texpr1.Div | Apron.Texpr1.Mod -> 7
      end
let precedence_of_expr = function
  | Cst _
  | Ref _ -> 8
  | Unop(op,_) -> precedence_of_unop op
  | Binop(op,_,_) -> precedence_of_binop op
  | If(_,_,_) -> 0
  | In(_,_) -> 3

let print_expr fmt expr =
  (* priority: correspondance with priorities in [parser.mly] *)
  let rec print_expr ?(specialif=false) fmt expr =
    match expr with
    | Cst(cst) -> print_cst fmt cst
    | Ref(var) -> pp_print_string fmt var
    | Unop(op,e) ->
	let prec = precedence_of_unop op in
	let prec1 = precedence_of_expr e in
	let par = prec1<=prec in
	fprintf fmt "%a %s%a%s"
	  print_unop op
	  (if par then "(" else "")
	  (print_expr ~specialif:false) e
	  (if par then ")" else "")
    | Binop(op,e1,e2) ->
	let prec = precedence_of_binop op in
	let prec1 = precedence_of_expr e1 in
	let prec2 = precedence_of_expr e2 in
	let par1 = prec1<prec in
	let par2 = prec2<=prec in
	fprintf fmt "@[<hov>%s%a%s %a@ %s%a%s@]"
	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) e1
	  (if par1 then ")" else "")
	  print_binop op
	  (if par2 then "(" else "")
	  (print_expr ~specialif:false) e2
	  (if par2 then ")" else "")
    | If(e1,e2,e3) ->
	let nif = match e3 with
	  | If _ -> true
	  | _ -> false
	in
	let prec = 0 in
	let prec1 = precedence_of_expr e1 in
	let par1 = prec1<=prec in
	if not specialif then fprintf fmt "@[<hov>";
	fprintf fmt "if %s%a%s@ then %a@ else %a"
	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) e1
	  (if par1 then ")" else "")
	  (print_expr ~specialif:false) e2
	  (print_expr ~specialif:nif) e3
	;
	if not specialif then fprintf fmt "@]";
    | In(expr,lexpr) ->
	let prec = 3 in
	let prec1 = precedence_of_expr expr in
	let par1 = prec1<=prec in
	fprintf fmt "%s%a%s in {%a}"
	  (if par1 then "(" else "")
	  (print_expr ~specialif:false) expr
	  (if par1 then ")" else "")
	  (Print.list
	    ~first:"@[<hov>" ~sep:",@," ~last:"@]"
	    (fun fmt expr ->
	      let prec1 = precedence_of_expr expr in
	      let par1 = prec1<=prec in
	      fprintf fmt "%s%a%s"
		(if par1 then "(" else "")
		(print_expr ~specialif:false) expr
		(if par1 then ")" else "")
	    )
	  )
	  lexpr
  in
  print_expr fmt expr
