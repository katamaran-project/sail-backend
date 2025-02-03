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
      (Ast.Identifier.to_string original_function_name)
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
    (original_function_name : Ast.Identifier.t)
    (operator               : PP.document     )
    (operands               : PP.document list) : PP.document GC.t
  =
  match operands with
  | [x] -> begin
      GC.pp_annotate [%here] begin
        GC.return begin
          MuSail.Statement.pp_expression begin
            Coq.pp_application
              (PP.string "exp_unop")
              [operator; x]
          end
        end
      end
    end
  | _ -> report_incorrect_argument_count original_function_name 1 operands


let translate_binary_operator_using_infix_notation
    (original_function_name : Ast.Identifier.t )
    (operator               : PP.document      )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x; y] -> begin
      GC.pp_annotate [%here] begin
        GC.return begin
          MuSail.Statement.pp_expression @@ PP.(surround parens @@ separate_horizontally ~separator:space [x; operator; y])
        end
      end
    end
  | _ -> report_incorrect_argument_count original_function_name 2 operands


let translate_binary_operator_using_function_notation
    (original_function_name : Ast.Identifier.t )
    (operator               : PP.document      )
    (operands               : PP.document list ) : PP.document GC.t
  =
  match operands with
  | [x; y] -> begin
      GC.pp_annotate [%here] begin
        GC.return begin
          MuSail.Statement.pp_expression begin
            Coq.pp_application
              (PP.string "exp_binop")
              [operator; x; y]
          end
        end
      end
    end
  | _ -> report_incorrect_argument_count original_function_name 2 operands


let translate_binary_operator
    (original_function_name : Ast.Identifier.t         )
    ?(infix                 : PP.document option = None)
    ?(name                  : PP.document option = None)
    (operands               : PP.document list         ) : PP.document GC.t
  =
  match Configuration.(get pretty_print_binary_operators), infix, name with
  | true , Some infix_operator, _                      -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | false, Some infix_operator, None                   -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | _    , _                  , Some function_operator -> translate_binary_operator_using_function_notation original_function_name function_operator operands
  | _                                                  -> failwith "bug! this really shouldn't happen"


(* todo check if this function is used anywhere *)
(*
  Looks for a value definition for <identifier>.
  This function expects the value to be an integer; if not, it causes failure.
*)
let lookup_integer_value_bound_to (identifier : Ast.Identifier.t) : Z.t GC.t =
  let* program = GC.get_program
  in
  match Ast.Definition.Select.(select (value_definition_named identifier) (drop_sail_definitions program.definitions)) with
  | [ value_definition ] -> begin
      match value_definition.value with
      | Int n -> GC.return @@ n
      | _     -> GC.fail [%here] @@ Printf.sprintf "identifier %s should be bound to integer" (Ast.Identifier.to_string identifier)
    end
  | []        -> GC.fail [%here] @@ Printf.sprintf "%s is not bound to compile time value" (Ast.Identifier.to_string identifier)
  | _         -> GC.fail [%here] @@ Printf.sprintf "bug? multiple matches found for %s" (Ast.Identifier.to_string identifier)


(*
  Looks for a value definition for <identifier>.
  This function expects the value to be an integer; if not, it returns none.
*)
let try_lookup_integer_value_bound_to (identifier : Ast.Identifier.t) : Z.t option GC.t =
  let* program = GC.get_program
  in
  match Ast.Definition.Select.(select (value_definition_named identifier) (drop_sail_definitions program.definitions)) with
  | [ value_definition ] -> begin
      match value_definition.value with
      | Int n -> GC.return @@ Some n
      | _     -> GC.return None
    end
  | _ -> GC.return None


(*
  Looks for a value definition for <identifier>.
  This function expects the value to be a string; if not, it returns none.
*)
let try_lookup_string_value_bound_to (identifier : Ast.Identifier.t) : string option GC.t =
  let* program = GC.get_program
  in
  match Ast.Definition.Select.(select (value_definition_named identifier) (drop_sail_definitions program.definitions)) with
  | [ value_definition ] -> begin
      match value_definition.value with
      | String s -> GC.return @@ Some s
      | _        -> GC.return None
    end
  | _ -> GC.return None


(*
   Checks if the expression represents a compile time known integer.
   This can take the form of either a literal or a constant variable.
*)
let extract_compile_time_integer (expression : Ast.Expression.t) : Z.t option GC.t =
  match expression with
  | Ast.Expression.Val (Ast.Value.Int value)   -> GC.return @@ Some value
  | Ast.Expression.Variable (identifier, _typ) -> try_lookup_integer_value_bound_to identifier
  | _                                          -> GC.return None


(*
   Checks if the expression represents a compile time known string.
   This can take the form of either a literal or a constant variable.
*)
let extract_compile_time_string (expression : Ast.Expression.t) : string option GC.t =
  match expression with
  | Ast.Expression.Val (Ast.Value.String value) -> GC.return @@ Some value
  | Ast.Expression.Variable (identifier, _typ)  -> try_lookup_string_value_bound_to identifier
  | _                                           -> GC.return None


let translate_sail_zeros (arguments : Ast.Expression.t list) : PP.document GC.t =
  let pp_zeros (number_of_bits : int) : PP.document GC.t =
    GC.pp_annotate [%here] begin
      GC.return begin
        if
          Configuration.(get bitvectors_zeros_ones_as_literal)
        then
          MuSail.Statement.pp_expression @@ MuSail.Expression.pp_zero_bitvector_using_literal number_of_bits
        else
          MuSail.Statement.pp_expression @@ MuSail.Expression.pp_zero_bitvector_using_function number_of_bits
      end
    end
  in
  match arguments with
  | [ argument ] -> begin
      let* argument_value = extract_compile_time_integer argument
      in
      match argument_value with
      | Some number_of_bits -> begin
          GC.pp_annotate [%here] begin
            pp_zeros (Z.to_int number_of_bits)
          end
        end
      | None -> GC.fail [%here] "sail_zeros expects compile time known integer argument"
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail [%here] message
    end


let translate_sail_ones (arguments : Ast.Expression.t list) : PP.document GC.t =
  let pp_ones (number_of_bits : int) =
      GC.return begin
        if
          Configuration.(get bitvectors_zeros_ones_as_literal)
        then
          MuSail.Statement.pp_expression @@ MuSail.Expression.pp_ones_bitvector_using_literal number_of_bits
        else
          MuSail.Statement.pp_expression @@ MuSail.Expression.pp_ones_bitvector_using_function number_of_bits
      end
  in
  match arguments with
  | [ argument ] -> begin
      let* argument_value = extract_compile_time_integer argument
      in
      match argument_value with
      | Some number_of_bits -> begin
          GC.pp_annotate [%here] begin
            pp_ones (Z.to_int number_of_bits)
          end
        end
      | None -> GC.fail [%here] "sail_ones expects compile time known integer argument"
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_ones to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail [%here] message
    end


let translate_unit_equality () : PP.document GC.t =
  GC.pp_annotate [%here] begin
    GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_true ()
  end


(*
   Sail allows to add bitvectors and integers.
   MuSail does not appear to support this.

   We translate bv + n as bv + bv where bv should represent the same value as n.
   How to do this conversion in MuSail is at the moment of this writing unknown.
   Instead, we chose to only support cases where the integer n is known at compile time.
   which makes it possible for this code to generate a bv with the same value.

   MuSail expects both operands in a bv+bv operation to have the same length.
   In order to determine the size of the second bv, we take a look at the type of the first operand,
   which we know it a bitvector.
*)
let translate_add_bits_int (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "add_bits_int"
  in
  let pp_addition
      (bitvector_argument : Ast.Expression.t)
      (integer_argument   : Z.t             ) : PP.document GC.t
    =
    (* Start by inferring type of left operand. We know this to be a bitvector, but we need to know its size *)
    match Ast.Expression.infer_type bitvector_argument with
    | Ast.Type.Bitvector (Ast.Definition.NumericExpression.Constant bitvector_size) -> begin
        (* Pretty print left operand *)
        let* pp_bitvector_argument =
          let* doc = Expressions.pp_expression bitvector_argument
          in
          GC.return @@ PP.(surround parens) doc
        in
        (* Pretty print right operand *)
        let pp_integer_argument =
          PP.(surround parens) @@ MuSail.Expression.pp_bitvector ~size:(Z.to_int bitvector_size) ~value:integer_argument
        in
        (* Wrap it all up in a nice bvadd operation *)
        GC.pp_annotate [%here] begin
          translate_binary_operator (* todo support infix notation *)
            (Ast.Identifier.mk sail_name)
            ~infix:(Some (PP.string "+ᵇ"))
            ~name:(Some (PP.string "bop.bvadd"))
            [ pp_bitvector_argument; pp_integer_argument ]
        end
      end
    | _ -> GC.fail [%here] "failed to determine bitvector size"
  in
  match arguments with
  | [ bitvector_argument; shift_argument ] -> begin
      let* shift_argument_value = extract_compile_time_integer shift_argument
      in
      match shift_argument_value with
      | Some integer_value -> pp_addition bitvector_argument integer_value
      | None               -> GC.fail [%here] @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


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
    ~(sail_name   : string               )
    ~(musail_name : PP.document          )
    ~(arguments   : Ast.Expression.t list) : PP.document GC.t
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
      let* shift_argument_value = extract_compile_time_integer shift_argument
      in
      match shift_argument_value with
      | Some bit_count -> pp_shift bitvector_argument bit_count
      | None           -> GC.fail [%here] @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_shift_left (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name   = "sail_shiftleft"
  and musail_name = PP.string "bop.shiftl"
  in
  GC.pp_annotate [%here] @@ translate_shift ~sail_name ~musail_name ~arguments


let translate_shift_right (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name   = "sail_shiftright"
  and musail_name = PP.string "bop.shiftr"
  in
  GC.pp_annotate [%here] @@ translate_shift ~sail_name ~musail_name ~arguments


let translate_extend
    ~(sail_name   : string               )
    ~(musail_name : string               )
    ~(arguments   : Ast.Expression.t list) : PP.document GC.t
  =
  (* "Raw" version of pp_zero_extend that takes arguments in PP.document form *)
  let pp_zero_extend_raw
      (bitvector    : PP.document)
      (new_bit_size : PP.document) : PP.document GC.t
    =
    GC.pp_annotate [%here] begin
      GC.return begin
        MuSail.Statement.pp_expression begin
          Coq.pp_application
            (PP.string "exp_unop")
            [
              new_bit_size;
              bitvector
            ]
        end
      end
    end
  in
  let pp_zero_extend
      (bitvector    : Ast.Expression.t)
      (new_bit_size : int             ) : PP.document GC.t
    =
    let* pp_bitvector =
      let* doc =
        Expressions.pp_expression bitvector
      in
      GC.return @@ PP.(surround parens) doc
    in
    let pp_new_bit_size =
      PP.string @@ Printf.sprintf "(uop.%s (n := %d))" musail_name new_bit_size
    in
    pp_zero_extend_raw pp_bitvector pp_new_bit_size
  in
  match arguments with
  | [ bitvector_argument; bit_size_argument ] -> begin
      let* bit_size_value = extract_compile_time_integer bit_size_argument
      in
      match bit_size_value with
      | Some new_bit_size -> pp_zero_extend bitvector_argument (Z.to_int new_bit_size)
      | None              -> begin
          let* pp_bitvector =
            let* doc =
              Expressions.pp_expression bitvector_argument
            in
            GC.return @@ PP.(surround parens) doc
          in
          let message =
            Printf.sprintf
              "only calls to %s supported where second argument's value is known at compile time; was given %s instead"
              sail_name
              (FExpr.to_string @@ Ast.Expression.to_fexpr bit_size_argument)
          in
          let* pp_new_bit_size = GC.not_yet_implemented ~message [%here]
          in
          pp_zero_extend_raw pp_bitvector pp_new_bit_size
        end
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_zero_extend (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "sail_zero_extend"
  and musail_name = "zext"
  in
  translate_extend ~sail_name ~musail_name ~arguments


let translate_sign_extend (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "sail_sign_extend"
  and musail_name = "sext"
  in
  translate_extend ~sail_name ~musail_name ~arguments


let translate_assertion (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "sail_assert"
  in
  match arguments with
  | [ condition; error_message ] -> begin
      let* result = extract_compile_time_string error_message
      in
      match result with
      | Some error_message -> begin
          let* pp_condition = Expressions.pp_expression condition
          in
          let pp_error_message =
            MuSail.Expression.pp_string @@ PP.(surround dquotes) @@ PP.string error_message
          in
          GC.return @@ MuSail.Statement.pp_assert ~condition:pp_condition ~message:pp_error_message
        end
      | None -> GC.fail [%here] @@ Printf.sprintf "%s should have its second argument known at compile time" sail_name
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "expected %s to receive two arguments" sail_name


let translate_bitvector_concatenation (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "bitvector_concat"
  in
  let derive_vector_length (expression : Ast.Expression.t) : PP.document GC.t =
    match Ast.Expression.infer_type expression with
    | Ast.Type.Bitvector (Ast.Numeric.Expression.Constant n) -> GC.return @@ PP.string @@ Z.to_string n
    | _                                                      -> GC.not_yet_implemented ~message:"expected constant in bitvector type" [%here]
  in
  match arguments with
  | [ bv1; bv2 ] -> begin
      let* pp_bv1        = Expressions.pp_expression bv1
      and* pp_bv2        = Expressions.pp_expression bv2
      and* pp_bv1_length = derive_vector_length bv1
      and* pp_bv2_length = derive_vector_length bv2
      in
      let binop_name =
        PP.(surround parens) begin
            Coq.pp_explicit_application
              (PP.string "bop.bvapp")
              [
                PP.string "_";
                pp_bv1_length;
                pp_bv2_length;
              ]
          end
      in
      translate_binary_operator
        (Ast.Identifier.mk sail_name)
        ~name:(Some binop_name)
        [
          PP.(surround parens) pp_bv1;
          PP.(surround parens) pp_bv2
        ]
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "%s should receive two bitvector arguments" sail_name


let translate_bitvector_slicing (arguments : Ast.Expression.t list) : PP.document GC.t =
  let sail_name = "subrange_bits"
  in
  match arguments with
  | [bitvector; first_index; second_index] -> begin
      let pp_slice
          (low_index : PP.document)
          (length    : PP.document)
          (bitvector : PP.document) : PP.document GC.t
        =
        translate_unary_operator
          (Ast.Identifier.mk sail_name)
          (
            PP.(surround parens) begin
              Coq.pp_application
                (PP.string "uop.vector_subrange")
                [ low_index; length ]
            end
          )
          [ PP.(surround parens) bitvector ]
      in
      (*
        How indices need to be interpreted depends on the order
        Here we assume that "backward slices" are not possible, i.e.,
        that the range goes from the lowest index to the highest index.
      *)
      let* pp_bitvector = Expressions.pp_expression bitvector
      and* first_index  = extract_compile_time_integer first_index
      and* second_index = extract_compile_time_integer second_index
      in
      match first_index, second_index with
      | Some first_index, Some second_index -> begin
          let first_index   = Z.to_int first_index
          and second_index  = Z.to_int second_index
          in
          let low_index     = Int.min first_index second_index
          and high_index    = Int.max first_index second_index
          in
          let length        = high_index - low_index + 1
          in
          pp_slice
            (PP.integer low_index)
            (PP.integer length   )
            pp_bitvector
        end
      | _ -> begin
          let* low_index = GC.not_yet_implemented ~message:"%s's indices should be known at compile time" [%here]
          and* length    = GC.not_yet_implemented ~message:"%s's indices should be known at compile time" [%here]
          in
          pp_slice low_index length pp_bitvector
        end
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "%s should receive three arguments" sail_name


let translate
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.document GC.t
  =
  let* () =
    let log_message = lazy begin
      let string_of_function_name =
        Ast.Identifier.to_string function_identifier
      and _string_of_arguments =
        FExpr.to_string begin
          FExpr.mk_list begin
            List.map ~f:Ast.Expression.to_fexpr arguments
          end
        end
      in
      Printf.sprintf
        "Translating function %s"
        string_of_function_name
    end
    in
    GC.log [%here] Logging.debug log_message
  in
  let* pp_arguments =
    GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
  in
  match Ast.Identifier.to_string function_identifier with
  | "not_bool" -> begin
      GC.pp_annotate [%here] begin
        translate_unary_operator function_identifier (PP.string "uop.not") pp_arguments
      end
    end
  | "signed" -> begin
      GC.pp_annotate [%here] begin
        translate_unary_operator function_identifier (PP.string "uop.signed") pp_arguments
      end
    end
  | "unsigned" -> begin
      GC.pp_annotate [%here] begin
        translate_unary_operator function_identifier (PP.string "uop.unsigned") pp_arguments
      end
    end
  | "eq_bit" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bit_equality)
          pp_arguments
      end
    end
  | "eq_bits" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.equality)
          pp_arguments
      end
    end
  | "add_bits" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.addition)
          ~name:(Some MuSail.Operator.Name.bitvector_addition)
          pp_arguments
      end
    end
  | "sub_bits" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.subtraction)
          ~name:(Some MuSail.Operator.Name.bitvector_subtraction)
          pp_arguments
      end
    end
  | "and_vec" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_conjunction)
          pp_arguments
      end
    end
  | "or_vec" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_disjunction)
          pp_arguments
      end
    end
  | "xor_vec" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_xor)
          pp_arguments
      end
    end
  | "eq_bool" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bool_equality)
          ~name:(Some MuSail.Operator.Name.bool_equality)
          pp_arguments
      end
    end
  | "neq_bool" -> begin
      GC.pp_annotate [%here] begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bool_equality)
          ~name:(Some MuSail.Operator.Name.bool_inequality)
          pp_arguments
      end
    end
  | "eq_unit" -> begin
      GC.pp_annotate [%here] begin
        translate_unit_equality ()
      end
    end
  | "add_bits_int" -> begin
      GC.pp_annotate [%here] begin
        translate_add_bits_int arguments
      end
    end
  | "sail_zeros" -> begin
      GC.pp_annotate [%here] begin
        translate_sail_zeros arguments
      end
    end
  | "sail_ones" -> begin
      GC.pp_annotate [%here] begin
        translate_sail_ones arguments
      end
    end
  | "sail_shiftleft" -> begin
      GC.pp_annotate [%here] begin
        translate_shift_left arguments
      end
    end
  | "sail_shiftright" -> begin
      GC.pp_annotate [%here] begin
        translate_shift_right arguments
      end
    end
  | "sail_zero_extend" -> begin
      GC.pp_annotate [%here] begin
        translate_zero_extend arguments
      end
    end
  | "sail_sign_extend" -> begin
      GC.pp_annotate [%here] begin
        translate_sign_extend arguments
      end
    end
  | "sail_assert" -> begin
      GC.pp_annotate [%here] begin
        translate_assertion arguments
      end
    end
  | "bitvector_concat" -> begin
      GC.pp_annotate [%here] begin
        translate_bitvector_concatenation arguments
      end
    end
  | "subrange_bits" -> begin
      GC.pp_annotate [%here] begin
        translate_bitvector_slicing arguments
      end
    end
  | _ -> begin
      GC.pp_annotate [%here] begin
        GC.return @@ MuSail.Statement.pp_call function_identifier pp_arguments
      end
    end
