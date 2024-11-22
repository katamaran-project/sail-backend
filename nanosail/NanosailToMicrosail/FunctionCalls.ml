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
      GC.pp_annotate [%here] begin
        GC.return begin
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
    (original_function_name : Ast.Identifier.t      )
    ?(infix                 : string option   = None)
    ?(name                  : string option   = None)
    (operands               : PP.document list      ) : PP.document GC.t
  =
  match Configuration.(get pretty_print_binary_operators), infix, name with
  | true , Some infix_operator, _                      -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | false, Some infix_operator, None                   -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | _    , _                  , Some function_operator -> translate_binary_operator_using_function_notation original_function_name function_operator operands
  | _                                                  -> failwith "bug! this really shouldn't happen"


(*
  Looks for a value definition for <identifier>.
  This function expects the value to be an integer; if not, it causes failure.
*)                                                                                     
let lookup_integer_value_bound_to (identifier : Ast.Identifier.t) : Z.t GC.t =
  let* program = GC.get_program
  in
  match Ast.Definition.Select.(select (value_definition ~identifier:identifier) program.definitions) with
  | [ (_, value_definition) ] -> begin
      match value_definition.value with
      | Int n -> GC.return @@ n
      | _     -> GC.fail @@ Printf.sprintf "identifier %s should be bound to integer" (Ast.Identifier.string_of identifier)
    end
  | []        -> GC.fail @@ Printf.sprintf "%s is not bound to compile time value" (Ast.Identifier.string_of identifier)
  | _         -> GC.fail @@ Printf.sprintf "bug? multiple matches found for %s" (Ast.Identifier.string_of identifier)


let translate_sail_zeros (arguments : Ast.Expression.t list) : PP.document GC.t =
  match arguments with
  | [ Ast.Expression.Val (Ast.Value.Int n) ] -> begin
      GC.pp_annotate [%here] begin
        GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size:(Z.to_int n) ~value:Z.zero
      end
    end
  | [ Ast.Expression.Variable identifier ] -> begin
      GC.pp_annotate [%here] begin
        let* number_of_bits = lookup_integer_value_bound_to identifier
        in
        GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size:(Z.to_int number_of_bits) ~value:Z.zero
      end
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
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail message
    end


let translate_sail_ones (arguments : Ast.Expression.t list) : PP.document GC.t =
  match arguments with
  | [ Ast.Expression.Val (Ast.Value.Int n) ] -> begin
      GC.pp_annotate [%here] begin
        let size = Z.to_int n
        and value = Z.sub (Z.shift_left Z.one (Z.to_int n)) Z.one
        in
        GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size ~value
      end
    end
  | [ Ast.Expression.Variable identifier ] -> begin
      GC.pp_annotate [%here] begin
        let* number_of_bits = lookup_integer_value_bound_to identifier
        in
        let size = Z.to_int number_of_bits
        and value = Z.sub (Z.shift_left Z.one (Z.to_int number_of_bits)) Z.one
        in
        GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_bitvector ~size ~value
      end
    end
  | [ argument ] -> begin
      let message =
        let formatted_argument =
          FExpr.to_string @@ Ast.Expression.to_fexpr argument
        in
        Printf.sprintf "expected sail_ones to receive an integer argument; instead got %s" formatted_argument
      in
      GC.fail message
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_ones to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail message
    end


let translate_unit_equality () : PP.document GC.t =
  GC.pp_annotate [%here] begin
    GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_true ()
  end


let translate_add_bits_int (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "add_bits_int"
  in
  let pp_addition
      (bitvector_argument : Ast.Expression.t)
      (integer_argument   : Z.t             ) : PP.document GC.t
    =
    let* pp_bitvector_argument =
      let* doc = Expressions.pp_expression bitvector_argument
      in
      GC.return @@ PP.(surround parens) doc
    in
    let pp_integer_argument =
      PP.(surround parens) @@ MuSail.Expression.pp_bitvector ~size:32 ~value:integer_argument (* todo fix size *)
    in
    GC.pp_annotate [%here] begin
      translate_binary_operator_using_function_notation (* todo support infix notation *)
        (Ast.Identifier.mk sail_name)
        "bop.bvadd"
        [ pp_bitvector_argument; pp_integer_argument ]
    end
  in
  match arguments with
  | [ bitvector_argument; shift_argument ] -> begin
      match shift_argument with
      | Variable identifier -> begin
          let* integer_value = lookup_integer_value_bound_to identifier
          in
          pp_addition bitvector_argument integer_value
        end
      | Val value -> begin
          match value with
          | Int integer_value -> pp_addition bitvector_argument integer_value
          | _ -> GC.fail "should never happen: the second argument has the wrong type"
        end
      | _ -> GC.fail @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


(*
   Sail definition:

      val sail_shiftleft = pure "shiftl" : forall 'n ('ord : Order).
        (bitvector('n, 'ord), int) -> bitvector('n, 'ord)


   MuSail definition:

       shiftl {m n} : BinOp (bvec m) (bvec n) (bvec m)


   In other words, the parameter types differ.
   We currently support only cases where the second parameter is a integer literal in the Sail code.
*)
let translate_shift
    ~(sail_name : string)
    ~(musail_name : string)
    ~(arguments : Ast.Expression.t list) : PP.document GC.t
  =
  let pp_shift
      (bitvector_argument : Ast.Expression.t)
      (shift_argument     : Z.t             ) : PP.document GC.t
    =
    let* pp_bitvector_argument =
      let* doc = Expressions.pp_expression bitvector_argument
      in
      GC.return @@ PP.(surround parens) doc
    in
    let pp_shift_argument =
      let size =
        (*
           pick the smallest bitvector size that can hold integer_value
           it should also be at least 1
        *)
        Z.log2up (Z.max (Z.of_int 2) shift_argument)
      in
      PP.(surround parens) @@ MuSail.Expression.pp_bitvector ~size ~value:shift_argument
    in
    GC.pp_annotate [%here] begin
      translate_binary_operator_using_function_notation
        (Ast.Identifier.mk sail_name)
        musail_name
        [ pp_bitvector_argument; pp_shift_argument ]
    end
  in
  match arguments with
  | [ bitvector_argument; shift_argument ] -> begin
      match shift_argument with
      | Variable identifier -> begin
          let* integer_value = lookup_integer_value_bound_to identifier
          in
          pp_shift bitvector_argument integer_value
        end
      | Val value -> begin
          match value with
          | Int integer_value -> pp_shift bitvector_argument integer_value
          | _ -> GC.fail "should never happen: the second argument has the wrong type"
        end
      | _ -> GC.fail @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_shift_left (arguments : Ast.Expression.t list) : PP.document GC.t =
  GC.pp_annotate [%here] @@ translate_shift ~sail_name:"sail_shiftleft" ~musail_name:"bop.shiftl" ~arguments


let translate_shift_right (arguments : Ast.Expression.t list) : PP.document GC.t =
  GC.pp_annotate [%here] @@ translate_shift ~sail_name:"sail_shiftright" ~musail_name:"bop.shiftr" ~arguments


let translate
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.document GC.t
  =
  let* pp_arguments =
    GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
  in
  match Ast.Identifier.string_of function_identifier with
  | "not_bool"        -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.not" pp_arguments
  | "signed"          -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.signed" pp_arguments
  | "unsigned"        -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.unsigned" pp_arguments
  | "eq_bits"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "=")                                     pp_arguments
  | "add_bits"        -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "+ᵇ") ~name:(Some "bop.bvadd")           pp_arguments
  | "sub_bits"        -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "-ᵇ") ~name:(Some "bop.bvsub")           pp_arguments
  | "and_vec"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvand")           pp_arguments
  | "or_vec"          -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvor")            pp_arguments
  | "xor_vec"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvxor")           pp_arguments
  | "eq_bool"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "=")  ~name:(Some "(bop.relop bop.eq)")  pp_arguments
  | "neq_bool"        -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "!=") ~name:(Some "(bop.relop bop.neq)") pp_arguments
  | "eq_unit"         -> GC.pp_annotate [%here] @@ translate_unit_equality ()
  | "add_bits_int"    -> GC.pp_annotate [%here] @@ translate_add_bits_int arguments
  | "sail_zeros"      -> GC.pp_annotate [%here] @@ translate_sail_zeros arguments
  | "sail_ones"       -> GC.pp_annotate [%here] @@ translate_sail_ones arguments
  | "sail_shiftleft"  -> GC.pp_annotate [%here] @@ translate_shift_left arguments
  | "sail_shiftright" -> GC.pp_annotate [%here] @@ translate_shift_right arguments      
  | _                 -> GC.pp_annotate [%here] @@ GC.return @@ MuSail.Statement.pp_call function_identifier pp_arguments
