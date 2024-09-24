open Base
open Monads.Notations.Star(GenerationContext)


(*
   "<arg>" :: <t>
*)
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


(*
   (call <function_identifier> <arguments[0]> <arguments[1]> ...)%exp
*)
let pp_call
    (function_identifier : Ast.Identifier.t)
    (arguments           : PP.document list) : PP.document
  =
  let terms =
    Auxlib.build_list @@ fun { add; addall; _ } -> begin
      add @@ PP.string "call";
      add @@ Identifier.pp function_identifier;
      addall @@ arguments
    end
  in
  Coq.pp_scope (PP.string "exp") (PP.simple_app terms)
