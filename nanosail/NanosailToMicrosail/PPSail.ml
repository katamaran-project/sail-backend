open Base
open Monads.Notations.Star(GenerationContext)


let pp_bind (arg, t) =
  PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)


(*
   exp_var "<id>"
*)
let pp_expression_of_identifier (id : Ast.Identifier.t) : PP.document =
  PP.simple_app [ PP.string "exp_var"; PP.dquotes @@ Identifier.pp id ]


(*
   stm_exp expression
*)
let pp_statement_of_expression (expression : PP.document) : PP.document =
  PP.simple_app [ PP.string "stm_exp"; PP.parens expression ]

