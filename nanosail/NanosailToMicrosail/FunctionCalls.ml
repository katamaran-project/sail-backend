open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let default_translation
    (function_identifier : Ast.Identifier.t)
    (arguments           : PP.document list) : PP.document GC.t
  =
  let terms =
    Auxlib.build_list @@ fun { add; addall; _ } -> begin
      add @@ PP.string "call";
      add @@ Identifier.pp function_identifier;
      addall @@ arguments
    end
  in
  GC.return @@ PP.simple_app terms


let translate_as_binary_operator
    (original_function_name : Ast.Identifier.t )
    (operator               : string           )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x; y] -> GC.return @@ PP.(parens @@ separate space [x; string operator; y])
  | _      -> begin
      let message =
        PP.string @@ Printf.sprintf
          "%s should receive 2 arguments but instead received %d; falling back on default translation for function calls"
          (Ast.Identifier.string_of original_function_name)
          (List.length operands)
      in
      let* annotation_index = GC.add_annotation message
      and* translation      = default_translation original_function_name operands
      in
      GC.return @@ PP.(separate space [
          translation;
          Coq.pp_inline_comment (string @@ Int.to_string annotation_index)
        ])
    end


let translate
    (function_identifier : Ast.Identifier.t )
    (arguments           : PP.document list ) : PP.document GC.t
  =
  match Ast.Identifier.string_of function_identifier with
  | "add_bits_int" -> translate_as_binary_operator function_identifier "+" arguments
  | _ -> default_translation function_identifier arguments
