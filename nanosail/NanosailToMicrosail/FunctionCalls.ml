open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let report_incorrect_argument_count
    (original_function_name : Ast.Identifier.t )
    (expected_operand_count : int              )
    (operands               : PP.document list ) : PP.document GC.t
  =
  let message =
    PP.annotate [%here] @@ PP.string @@ Printf.sprintf
      "%s should receive %d argument(s) but instead received %d; falling back on default translation for function calls"
      (Ast.Identifier.string_of original_function_name)
      expected_operand_count
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


let translate_unary_operator
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
  | _ -> report_incorrect_argument_count original_function_name 1 operands


let translate_binary_operator_using_infix_notation
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
  | _ -> report_incorrect_argument_count original_function_name 2 operands


let translate_binary_operator_using_function_notation
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
  | _ -> report_incorrect_argument_count original_function_name 2 operands    


let translate_binary_operator 
    (original_function_name : Ast.Identifier.t)
    (infix_operator         : string option   )
    (function_operator      : string          )
    (operands               : PP.document list) : PP.document GC.t
  =
  match Configuration.(get pretty_print_binary_operators), infix_operator with
  | true, Some infix_operator -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | _                         -> translate_binary_operator_using_function_notation original_function_name function_operator operands


let translate_sail_zeros (arguments : Ast.Expression.t list) : PP.document GC.t =
  match arguments with
  | [ Ast.Expression.Val (Ast.Value.Int n) ] -> begin
      GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size:(Z.to_int n) ~value:Z.zero
    end
  | [ argument ] -> begin
      let message =
        let formatted_argument =
          FExpr.to_string @@ Ast.Expression.to_fexpr argument
        in
        Printf.sprintf "expected sail_zeros to receive an integer argument; instead got %s" formatted_argument
      in
      GC.fail message
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead for %d" (List.length arguments)
      in
      GC.fail message
    end


let translate_sail_ones (arguments : Ast.Expression.t list) : PP.document GC.t =
  match arguments with
  | [ Ast.Expression.Val (Ast.Value.Int n) ] -> begin
      let size = Z.to_int n
      and value = Z.sub (Z.shift_left Z.one (Z.to_int n)) Z.one
      in
      GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size ~value
    end
  | [ argument ] -> begin
      let message =
        let formatted_argument =
          FExpr.to_string @@ Ast.Expression.to_fexpr argument
        in
        Printf.sprintf "expected sail_zeros to receive an integer argument; instead got %s" formatted_argument
      in
      GC.fail message
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead for %d" (List.length arguments)
      in
      GC.fail message
    end


let translate
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.document GC.t
  =
  let* pp_arguments =
    GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
  in
  match Ast.Identifier.string_of function_identifier with
  | "add_bits_int" -> begin
      (* todo check this; could need to be bitvector addition, which does not use + (see Expressions.v in Katamaran codebase) *)
      GC.pp_annotate [%here] begin
        translate_binary_operator_using_infix_notation function_identifier "+" pp_arguments
      end
    end
  | "add_bits"     -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier (Some "+ᵇ") "(bop.bvadd)" pp_arguments
  | "sub_bits"     -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier (Some "-ᵇ") "(bop.bvsub)" pp_arguments
  | "and_vec"      -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier None "(bop.bvand)" pp_arguments
  | "not_bool"     -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.not" pp_arguments
  | "eq_bool"      -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier (Some "=") "(bop.relop bop.eq)" pp_arguments
  | "neq_bool"     -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier (Some "!=") "(bop.relop bop.neq)" pp_arguments
  | "sail_zeros"   -> GC.pp_annotate [%here] @@ translate_sail_zeros arguments
  | "sail_ones"    -> GC.pp_annotate [%here] @@ translate_sail_ones arguments
  | _              -> GC.return @@ MuSail.Statement.pp_call function_identifier pp_arguments
