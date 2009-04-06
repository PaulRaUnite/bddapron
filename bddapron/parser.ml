(** Translating expressions from abstract syntax tree *)

open Syntax
open Format

(*  ********************************************************************** *)
(** {2 Translation from [Syntax.expr] to [Bddapron.Expr0.t]} *)
(*  ********************************************************************** *)

let translate_cst env (cst:Syntax.cst)
    =
  match cst with
  | `Bool b ->
      Expr0.Bool.to_expr
	((if b then Expr0.Bool.dtrue else Expr0.Bool.dfalse)
	  env)
  | `Bint (typ,n) ->
      Expr0.Bint.to_expr
	(Expr0.Bint.of_int env (`Tbint typ) n)
  | `Apron coeff ->
      Expr0.Apron.to_expr
	(Expr0.Apron.cst env coeff)

let apply_bbinop env (op:Syntax.bbinop) e1 e2
    =
  match op with
  | Or | And ->
      let e1 = Expr0.Bool.of_expr e1 in
      let e2 = Expr0.Bool.of_expr e2 in
      begin match op with
      | Or -> Expr0.Bool.dor env e1 e2
      | And -> Expr0.Bool.dand env e1 e2
      | _ -> failwith ""
      end
  | EQ | NEQ ->
      let typexpr1 = Expr0.typ_of_expr e1 in
      let typexpr2 = Expr0.typ_of_expr e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic test %a applied to expressions of different types %a and %a"
	  Syntax.print_bbinop op
	  Syntax.print_typ typexpr1
	  Syntax.print_typ typexpr2)
      end;
      let res = Expr0.eq env e1 e2 in
      if op = EQ
      then res
      else Expr0.Bool.dnot env res
  | GT | GEQ | LEQ | LT ->
      let typexpr1 = Expr0.typ_of_expr e1 in
      let typexpr2 = Expr0.typ_of_expr e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic test %a applied to expressions of different types %a and %a"
	  Syntax.print_bbinop op
	  Syntax.print_typ typexpr1
	  Syntax.print_typ typexpr2)
      end;
      begin match typexpr1 with
      | `Bint(b,size) ->
	  let e1 = Expr0.Bint.of_expr e1 in
	  let e2 = Expr0.Bint.of_expr e2 in
	  begin match op with
	  | GT ->  Expr0.Bint.sup env e1 e2
	  | GEQ -> Expr0.Bint.supeq env e1 e2
	  | LEQ -> Expr0.Bint.supeq env e2 e1
	  | LT ->  Expr0.Bint.sup env e2 e1
	  | _ -> failwith ""
	  end
      | `Real ->
	  let e1 = Expr0.Apron.of_expr e1 in
	  let e2 = Expr0.Apron.of_expr e2 in
	  begin match op with
	  | GT ->
	      let e = Expr0.Apron.sub env e1 e2 in
	      Expr0.Apron.sup env e
	  | GEQ ->
	      let e = Expr0.Apron.sub env e1 e2 in
	      Expr0.Apron.supeq env e
	  | LEQ ->
	      let e = Expr0.Apron.sub env e2 e1 in
	      Expr0.Apron.supeq env e
	  | LT ->
	      let e = Expr0.Apron.sub env e2 e1 in
	      Expr0.Apron.sup env e
	  | _ -> failwith ""
	  end
      | _ ->
	  (error
	    "arithmetic test %a applied to non arithmetic expressions of type %a"
	    Syntax.print_bbinop op
	    Syntax.print_typ typexpr1)
      end

let apply_binop env (binop:Syntax.binop) e1 e2
    =
  match binop with
  | `Bool op ->
      let e = apply_bbinop env op e1 e2 in
      Expr0.Bool.to_expr e
  | `Apron(op,typ,round) ->
      let typexpr1 = Expr0.typ_of_expr e1 in
      let typexpr2 = Expr0.typ_of_expr e2 in
      if typexpr1<>typexpr2 then begin
	(error
	  "arithmetic operation %a applied to expressions of different types %a and %a"
	  Apron.Texpr0.print_binop op
	  Syntax.print_typ typexpr1
	  Syntax.print_typ typexpr2
	)
      end;
      begin match typexpr1 with
      | `Bint(b,size) ->
	  let e1 = Expr0.Bint.of_expr e1 in
	  let e2 = Expr0.Bint.of_expr e2 in
	  let fop = match op with
	    | Apron.Texpr1.Add -> Expr0.Bint.add
	    | Apron.Texpr1.Sub -> Expr0.Bint.sub
	    | Apron.Texpr1.Mul -> Expr0.Bint.mul
	    | Apron.Texpr1.Div
	    | Apron.Texpr1.Mod ->
		error
		  "operation %a not permitted on bounded integer expressions"
		  Apron.Texpr0.print_binop op
	  in
	  let e = fop env e1 e2 in
	  Expr0.Bint.to_expr e
      | `Real ->
	  let e1 = Expr0.Apron.of_expr e1 in
	  let e2 = Expr0.Apron.of_expr e2 in
	  let fop = match op with
	    | Apron.Texpr1.Add -> Expr0.Apron.add
	    | Apron.Texpr1.Sub -> Expr0.Apron.sub
	    | Apron.Texpr1.Mul -> Expr0.Apron.mul
	    | Apron.Texpr1.Div -> Expr0.Apron.div
	    | Apron.Texpr1.Mod -> Expr0.Apron.gmod
	  in
	  let e = fop env ~typ ~round e1 e2 in
	  Expr0.Apron.to_expr e
      | _ ->
	  error
	    "arithmetic operation %a applied to non-arithmetic expressions of type %a"
	    Apron.Texpr0.print_binop op
	    Syntax.print_typ typexpr1
      end

let rec translate_expr
    env
    (expr:Syntax.expr)
    :
    Expr0.t
    =
  try
    let res = match expr with
      | Cst cst -> translate_cst env cst
      | Ref var ->
	  if not (env#mem_var var) then
	    (error
	      "unknown label/variable %s" var)
	  else
	    Expr0.var env var
      | Unop(op,e) ->
	  let e = translate_expr env e in
	  begin match op with
	  | `Not ->
	      Expr0.Bool.to_expr
		(Expr0.Bool.dnot env (Expr0.Bool.of_expr e))
	  | `Apron (op,typ,round) ->
	      let typexpr = Expr0.typ_of_expr e in
	      begin match typexpr with
	      | `Bint(b,size) ->
		  let e = Expr0.Bint.of_expr e in
		  let e = begin match op with
		    | Apron.Texpr1.Neg ->
			if not b then
			  failwith (
			    Print.sprintf
			      "@[<v>@ negation cannot be applied to the expression@ %a@ of type %a (unsigned integer)@ @]"
			      (Expr0.Bint.print env) e
			      Syntax.print_typ typexpr
			  )
			;
			Expr0.Bint.neg env e
		    | Apron.Texpr1.Cast
		    | Apron.Texpr1.Sqrt ->
			failwith (
			  Print.sprintf
			    "@[<v>@ cast or sqrt operators cannot be applied to the expression@ %a@ of type %a@ @]"
			    (Expr0.Bint.print env) e
			    Syntax.print_typ typexpr
			)
		  end
		  in
		  Expr0.Bint.to_expr e
	      | `Real ->
		  let e = Expr0.Apron.of_expr e in
		  let e = match op with
		    | Apron.Texpr1.Neg -> Expr0.Apron.negate env e
		    | Apron.Texpr1.Cast -> Expr0.Apron.cast env ~typ ~round e
		    | Apron.Texpr1.Sqrt -> Expr0.Apron.sqrt env ~typ ~round e
		  in
		  Expr0.Apron.to_expr e
	      | _ ->
		  failwith (
		    Print.sprintf
		      "@[<v>@ neg, cast or sqrt operators cannot be applied to the expression@ %a@ of type %a@ @]"
		      (Expr0.print env) e
		      Syntax.print_typ typexpr
		  )
	      end
	  end
      | Binop(op,e1,e2) ->
	  let e1 = translate_expr env e1 in
	  let e2 = translate_expr env e2 in
	  apply_binop env op e1 e2
      | If(e1,e2,e3) ->
	  let e1 = translate_expr env e1 in
	  let e1 = Expr0.Bool.of_expr e1 in
	  let e2 = translate_expr env e2 in
	  let e3 = translate_expr env e3 in
	  Expr0.ite env e1 e2 e3
      | In(e0,le) ->
	  let e0 = translate_expr env e0 in
	  let acc = Expr0.Bool.dfalse env in
	  let res =
	    List.fold_left
	      (begin fun acc e ->
		let e = translate_expr env e in
		Expr0.Bool.dor env
		  acc
		  (Expr0.eq env e0 e)
	      end)
	      acc le
	  in
	  Expr0.Bool.to_expr res
    in
    res
  with Error(s) ->
    error "@[<v>%s@ in expression@   %a@]@."
      s Syntax.print_expr expr

(*  ********************************************************************** *)
(** {2 Parsing} *)
(*  ********************************************************************** *)

let expr1_of_expr env expr : Expr1.t =
  let expr0 = translate_expr env expr in
  Expr1.O.make (Oo.copy env) expr0

let expr1_of_lexbuf env lexbuf =
  let x = Yacc.expr Lex.lex lexbuf in
  expr1_of_expr env x

let expr1_of_string env str =
  try
    let lexbuf = Lexing.from_string str in
    try expr1_of_lexbuf env lexbuf
    with Parsing.Parse_error ->
      error
	"Syntaxical error, characters %d-%d in expression %s"
	(Lexing.lexeme_start lexbuf)
	(Lexing.lexeme_end lexbuf)
	str
  with Lex.Error (s,e) ->
    error
      "Lexical error, characters %d-%d in expression %s"
      s e str
