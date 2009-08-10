(** Parsing BDDAPRON expressions from AST or strings *)

(* This file is part of the BDDAPRON Library, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

(** {2 From strings} *)
val expr0_of_string : Env.t -> Cond.t -> string -> Expr0.t

val expr1_of_string : Env.t -> Cond.t -> string -> Expr1.t
val listexpr1_of_lstring : 
  Env.t -> Cond.t -> string list -> Expr1.List.t

val listexpr2_of_lstring : 
  ?normalize:bool -> ?reduce:bool -> ?careset:bool -> 
  Env.t -> Cond.t -> string list -> Expr2.List.t
val boolexpr2_of_string : 
  ?normalize:bool -> ?reduce:bool -> ?careset:bool -> 
  Env.t -> Cond.t -> string -> Expr2.Bool.t

(** {2 From abstract syntax tree} *)
val expr0_of_expr : Env.t -> Cond.t -> Syntax.expr -> Expr0.t
val expr1_of_expr : Env.t -> Cond.t -> Syntax.expr -> Expr1.t
val listexpr1_of_lexpr : 
  Env.t -> Cond.t -> Syntax.expr list -> Expr1.List.t
val listexpr2_of_lexpr : 
  ?normalize:bool -> ?reduce:bool -> ?careset:bool -> 
  Env.t -> Cond.t -> Syntax.expr list -> Expr2.List.t
val boolexpr2_of_expr : 
  ?normalize:bool -> ?reduce:bool -> ?careset:bool -> 
  Env.t -> Cond.t -> Syntax.expr -> Expr2.Bool.t

(** {2 Misc.} *)

val expr0_of_lexbuf : Env.t -> Cond.t -> Lexing.lexbuf -> Expr0.t
