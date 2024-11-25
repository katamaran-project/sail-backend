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


(* todo check if this function is used anywhere *)
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


(*
  Looks for a value definition for <identifier>.
  This function expects the value to be an integer; if not, it returns none.
*)                                                                                     
let try_lookup_integer_value_bound_to (identifier : Ast.Identifier.t) : Z.t option GC.t =
  let* program = GC.get_program
  in
  match Ast.Definition.Select.(select (value_definition ~identifier:identifier) program.definitions) with
  | [ (_, value_definition) ] -> begin
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
  match Ast.Definition.Select.(select (value_definition ~identifier:identifier) program.definitions) with
  | [ (_, value_definition) ] -> begin
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
      | None -> GC.fail "sail_zeros expects compile time known integer argument"
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail message
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
      | None -> GC.fail "sail_ones expects compile time known integer argument"
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
            ~infix:(Some "+ᵇ")
            ~name:(Some "bop.bvadd")
            [ pp_bitvector_argument; pp_integer_argument ]
        end
      end
    | _ -> GC.fail "failed to determine bitvector size"
  in
  match arguments with
  | [ bitvector_argument; shift_argument ] -> begin
      let* shift_argument_value = extract_compile_time_integer shift_argument
      in
      match shift_argument_value with
      | Some integer_value -> pp_addition bitvector_argument integer_value
      | None               -> GC.fail @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
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
      let* shift_argument_value = extract_compile_time_integer shift_argument
      in
      match shift_argument_value with
      | Some bit_count -> pp_shift bitvector_argument bit_count
      | None           -> GC.fail @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_shift_left (arguments : Ast.Expression.t list) : PP.document GC.t =
  GC.pp_annotate [%here] @@ translate_shift ~sail_name:"sail_shiftleft" ~musail_name:"bop.shiftl" ~arguments


let translate_shift_right (arguments : Ast.Expression.t list) : PP.document GC.t =
  GC.pp_annotate [%here] @@ translate_shift ~sail_name:"sail_shiftright" ~musail_name:"bop.shiftr" ~arguments


let translate_extend
    ~(sail_name   : string               )
    ~(musail_name : string               )
    ~(arguments   : Ast.Expression.t list) : PP.document GC.t
  =
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
    GC.pp_annotate [%here] begin
      GC.return begin
        MuSail.Statement.pp_expression begin
          Coq.pp_application
            (PP.string "exp_unop")
            [
              PP.string @@ Printf.sprintf "(uop.%s (n := %d))" musail_name new_bit_size;
              pp_bitvector
            ]
        end
      end
    end
  in
  match arguments with
  | [ bitvector_argument; bit_size_argument ] -> begin
      let* bit_size_value = extract_compile_time_integer bit_size_argument
      in
      match bit_size_value with
      | Some new_bit_size -> pp_zero_extend bitvector_argument (Z.to_int new_bit_size)
      | None              -> GC.fail @@ Printf.sprintf "only calls to %s supported where second argument's value is known at compile time" sail_name
    end
  | _ -> GC.fail @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


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
          let* condition = Expressions.pp_expression condition
          in
          let when_true =
            MuSail.Statement.pp_fail @@ PP.(surround dquotes) @@ PP.string error_message
          and when_false =
            MuSail.Statement.pp_nop
          in
          GC.return @@ MuSail.Statement.pp_conditional ~condition ~when_true ~when_false
        end
      | None -> GC.fail @@ Printf.sprintf "%s should have its second argument known at compile time" sail_name
    end
  | _ -> GC.fail @@ Printf.sprintf "expected %s to receive two arguments" sail_name


let translate
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.document GC.t
  =
  let* () = GC.log Logging.debug @@ lazy (Printf.sprintf "Translating function %s" (Ast.Identifier.string_of function_identifier))
  in
  let* pp_arguments =
    GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
  in
  match Ast.Identifier.string_of function_identifier with
  | "not_bool"         -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.not" pp_arguments
  | "signed"           -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.signed" pp_arguments
  | "unsigned"         -> GC.pp_annotate [%here] @@ translate_unary_operator  function_identifier "uop.unsigned" pp_arguments
  | "eq_bit"           -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "=")                                     pp_arguments
  | "eq_bits"          -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "=")                                     pp_arguments
  | "add_bits"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "+ᵇ") ~name:(Some "bop.bvadd")           pp_arguments
  | "sub_bits"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "-ᵇ") ~name:(Some "bop.bvsub")           pp_arguments
  | "and_vec"          -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvand")           pp_arguments
  | "or_vec"           -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvor")            pp_arguments
  | "xor_vec"          -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvxor")           pp_arguments
  | "eq_bool"          -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "=")  ~name:(Some "(bop.relop bop.eq)")  pp_arguments
  | "neq_bool"         -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier ~infix:(Some "!=") ~name:(Some "(bop.relop bop.neq)") pp_arguments
  | "bitvector_concat" -> GC.pp_annotate [%here] @@ translate_binary_operator function_identifier                    ~name:(Some "bop.bvapp")           pp_arguments
  | "eq_unit"          -> GC.pp_annotate [%here] @@ translate_unit_equality ()
  | "add_bits_int"     -> GC.pp_annotate [%here] @@ translate_add_bits_int arguments
  | "sail_zeros"       -> GC.pp_annotate [%here] @@ translate_sail_zeros arguments
  | "sail_ones"        -> GC.pp_annotate [%here] @@ translate_sail_ones arguments
  | "sail_shiftleft"   -> GC.pp_annotate [%here] @@ translate_shift_left arguments
  | "sail_shiftright"  -> GC.pp_annotate [%here] @@ translate_shift_right arguments
  | "sail_zero_extend" -> GC.pp_annotate [%here] @@ translate_zero_extend arguments
  | "sail_sign_extend" -> GC.pp_annotate [%here] @@ translate_sign_extend arguments
  | "sail_assert"      -> GC.pp_annotate [%here] @@ translate_assertion arguments
  | _                  -> GC.pp_annotate [%here] @@ GC.return @@ MuSail.Statement.pp_call function_identifier pp_arguments
