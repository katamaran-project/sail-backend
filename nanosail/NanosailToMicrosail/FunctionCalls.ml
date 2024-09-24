open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


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
      in
      let translation = PPSail.pp_call original_function_name operands
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
  | "add_bits_int" -> begin
      (* todo check this; could need to be bitvector addition, which does not use + (see Expressions.v in Katamaran codebase) *)
      translate_as_binary_operator function_identifier "+" arguments
    end
  | _              -> GC.return @@ PPSail.pp_call function_identifier arguments
