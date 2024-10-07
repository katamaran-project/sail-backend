open Base
open Monads.Notations.Star(GenerationContext)


(*
   "<argument>" ∷ <typ>
*)
let pp_bind
    (argument : PP.document)
    (typ      : PP.document) : PP.document
  =
  PP.(separate space [dquotes argument; utf8string " ∷ "; typ ])


(*
   exp_var "<id>"
*)
let pp_expression_of_identifier (identifier : PP.document) : PP.document =
  PP.simple_app [ PP.string "exp_var"; PP.dquotes @@ identifier ]


(*
   stm_exp (<expression>)
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



let pp_expression_value
    (typ   : PP.document)
    (value : PP.document) : PP.document
  =
  PP.simple_app [
    PP.string "exp_val";
    PP.parens typ;
    PP.parens value
  ]


let string_of_pprint_document (document : PPrint.document) =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer

