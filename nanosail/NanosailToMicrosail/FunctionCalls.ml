open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let translate_as_unary_operator
    (original_function_name : Ast.Identifier.t )
    (operator               : string           )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x] -> begin
      GC.pp_annotate [%here] begin
        GC.return begin
          MuSail.Statement.pp_expression begin
            Coq.pp_application
              (PP.string "exp_unop")
              [PP.string operator; x]
          end
        end
      end
    end
  | _ -> begin
      let message =
        PP.annotate [%here] @@ PP.string @@ Printf.sprintf
          "%s should receive 1 argument but instead received %d; falling back on default translation for function calls"
          (Ast.Identifier.string_of original_function_name)
          (List.length operands)
      in
      let* annotation_index =
        GC.add_annotation message
      in
      let translation =
        PP.annotate [%here] begin
            MuSail.Statement.pp_call original_function_name operands
          end
      in
      GC.return begin
          PP.annotate [%here] begin
              PP.(separate_horizontally ~separator:space [
                      translation;
                      Coq.pp_inline_comment (string @@ Int.to_string annotation_index)
              ])
            end
        end
    end


let translate_as_binary_operator_using_infix_notation
    (original_function_name : Ast.Identifier.t )
    (operator               : string           )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x; y] -> begin
      GC.return begin
          PP.annotate [%here] @@ begin
              MuSail.Statement.pp_expression @@ PP.(surround parens @@ separate_horizontally ~separator:space [x; string operator; y])
            end
        end
    end

  | _      -> begin
      let message =
        PP.annotate [%here] @@ PP.string @@ Printf.sprintf
          "%s should receive 2 arguments but instead received %d; falling back on default translation for function calls"
          (Ast.Identifier.string_of original_function_name)
          (List.length operands)
      in
      let* annotation_index =
        GC.add_annotation message
      in
      let translation =
        PP.annotate [%here] begin
            MuSail.Statement.pp_call original_function_name operands
          end
      in
      GC.return begin
          PP.annotate [%here] begin
              PP.(separate_horizontally ~separator:space [
                      translation;
                      Coq.pp_inline_comment (string @@ Int.to_string annotation_index)
              ])
            end
        end
    end


let translate_as_binary_operator_using_function_notation
    (original_function_name : Ast.Identifier.t )
    (operator               : string           )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x; y] -> begin
      GC.pp_annotate [%here] begin
        GC.return begin
          MuSail.Statement.pp_expression begin
            Coq.pp_application
              (PP.string "exp_binop")
              [PP.string operator; x; y]
          end
        end
      end
    end
  | _ -> begin
      let message =
        PP.annotate [%here] @@ PP.string @@ Printf.sprintf
          "%s should receive 2 arguments but instead received %d; falling back on default translation for function calls"
          (Ast.Identifier.string_of original_function_name)
          (List.length operands)
      in
      let* annotation_index =
        GC.add_annotation message
      in
      let translation =
        PP.annotate [%here] begin
            MuSail.Statement.pp_call original_function_name operands
          end
      in
      GC.return begin
          PP.annotate [%here] begin
              PP.(separate_horizontally ~separator:space [
                      translation;
                      Coq.pp_inline_comment (string @@ Int.to_string annotation_index)
              ])
            end
        end
    end


let translate_as_binary_operator 
    (original_function_name : Ast.Identifier.t )
    (infix_operator         : string           )
    (function_operator      : string           )
    (operands               : PP.document list ) : PP.document GC.t
  =
  if Configuration.(get pretty_print_binary_operators)
  then translate_as_binary_operator_using_infix_notation original_function_name infix_operator operands
  else translate_as_binary_operator_using_function_notation original_function_name function_operator operands


let translate
    (function_identifier : Ast.Identifier.t )
    (arguments           : PP.document list ) : PP.document GC.t
  =
  match Ast.Identifier.string_of function_identifier with
  | "add_bits_int" -> begin
      (* todo check this; could need to be bitvector addition, which does not use + (see Expressions.v in Katamaran codebase) *)
      GC.pp_annotate [%here] begin
        translate_as_binary_operator_using_infix_notation function_identifier "+" arguments
      end
    end
  | "add_bits"     -> GC.pp_annotate [%here] @@ translate_as_binary_operator function_identifier "!=" "(bop.bvadd)" arguments
  | "not_bool"     -> GC.pp_annotate [%here] @@ translate_as_unary_operator function_identifier "uop.not" arguments
  | "eq_bool"      -> GC.pp_annotate [%here] @@ translate_as_binary_operator function_identifier "=" "(bop.relop bop.eq)" arguments
  | "neq_bool"     -> GC.pp_annotate [%here] @@ translate_as_binary_operator function_identifier "!=" "(bop.relop bop.neq)" arguments
  | _              -> GC.return @@ MuSail.Statement.pp_call function_identifier arguments
