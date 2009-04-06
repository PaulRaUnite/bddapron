(** Translating expressions from abstract syntax tree *)

val translate_expr : Env.t -> Syntax.expr -> Expr0.t

val expr1_of_expr : Env.t -> Syntax.expr -> Expr1.t
val expr1_of_lexbuf : Env.t -> Lexing.lexbuf -> Expr1.t
val expr1_of_string : Env.t -> string -> Expr1.t
