open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(*
   Generates function call using the syntax specified in the configuration.
*)
let pp_function_call_using_configured_syntax
    (function_name : PP.t     )
    (arguments     : PP.t list) : PP.t
  =
  if
    Configuration.(get pretty_print_function_calls)
  then
    MuSail.Statement.pp_call_using_notation function_name arguments
  else
    MuSail.Statement.pp_call function_name arguments


(*
   If the function is an extern that can be a foreign function (see ForeignFunctions),
   the call as a foreign call: stm_foreign f args. The extern gets a constructor of FunX.
*)
let try_foreign_call
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t option GC.t
  =
  let* foreign_function = ForeignFunctions.find function_identifier
  in
  match foreign_function with
  | None                  -> GC.return None
  | Some foreign_function -> begin
      let* () = GC.use_foreign_function foreign_function.identifier
      in
      let* pp_arguments =
        GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
      in
      GC.return @@ Some (PP.annotate [%here] @@ MuSail.Statement.pp_foreign_call (Identifier.pp foreign_function.identifier) pp_arguments)
    end


(*
   A call that is translated as a call: a foreign call if the function is an extern, a stm_call otherwise.
*)
let translate_call
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let* foreign_call = try_foreign_call function_identifier arguments
  in
  match foreign_call with
  | Some pp_foreign_call -> GC.return pp_foreign_call
  | None                 -> begin
      let* pp_arguments =
        GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
      in
      GC.return @@ pp_function_call_using_configured_syntax (Identifier.pp function_identifier) pp_arguments
    end


(*
   A call that the translator cannot turn into a muSail operation: a foreign call if the function
   is an extern, a hole (NYI) otherwise.
*)
let translate_call_or_hole
    ~(message            : string               )
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let* foreign_call = try_foreign_call function_identifier arguments
  in
  match foreign_call with
  | Some pp_foreign_call -> GC.return pp_foreign_call
  | None                 -> GC.not_yet_implemented ~message [%here]


(*
   The width of a bitvector expression, if it is known at compile time (looking through aliases).
*)
let constant_bitvector_width (expression : Ast.Expression.t) : Z.t option =
  match Ast.Type.simplify @@ Ast.Type.strip_aliases @@ Ast.Expression.infer_type expression with
  | Bitvector (Constant width) -> Some width
  | _                          -> None


(*
   Whether the width of a bitvector expression mentions a size variable (kid__n), such as the width
   parameter of a size-polymorphic function (see TypeLevelParameters). muSail operations whose
   side conditions mention the width (IsTrue (n <=? m)) cannot be proved for it without Sail's constraints.
*)
let width_depends_on_size_variable (expression : Ast.Expression.t) : bool =
  match Ast.Type.simplify @@ Ast.Type.strip_aliases @@ Ast.Expression.infer_type expression with
  | Bitvector width -> begin
      List.exists (TypeLevelParameters.numeric_identifiers width) ~f:(fun identifier ->
          String.is_prefix (Ast.Identifier.to_string identifier) ~prefix:"kid__"
        )
    end
  | _ -> false


let report_incorrect_argument_count
    (original_function_name : Ast.Identifier.t )
    (expected_operand_count : int              )
    (operands               : PP.t list        ) : PP.t GC.t
  =
  let message =
    PP.string @@ Printf.sprintf
      "%s should receive %d argument(s) but instead received %d; falling back on default translation for function calls"
      (Ast.Identifier.to_string original_function_name)
      expected_operand_count
      (List.length operands)
  in
  let* annotation_index =
    GC.add_annotation message
  in
  let translation =
    pp_function_call_using_configured_syntax (Identifier.pp original_function_name) operands
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
    (operator               : PP.t            )
    (operands               : PP.t list       ) : PP.t GC.t
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
    (original_function_name : Ast.Identifier.t)
    (operator               : PP.t            )
    (operands               : PP.t list       ) : PP.t GC.t
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
    (original_function_name : Ast.Identifier.t)
    (operator               : PP.t            )
    (operands               : PP.t list       ) : PP.t GC.t
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
    (original_function_name : Ast.Identifier.t       )
    ?(infix                 : PP.t option      = None)
    ?(name                  : PP.t option      = None)
    (operands               : PP.t list              ) : PP.t GC.t
  =
  match Configuration.(get pretty_print_binary_operators), infix, name with
  | true , Some infix_operator, _                      -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | false, Some infix_operator, None                   -> translate_binary_operator_using_infix_notation original_function_name infix_operator operands
  | _    , _                  , Some function_operator -> translate_binary_operator_using_function_notation original_function_name function_operator operands
  | _                                                  -> failwith "bug! this really shouldn't happen"


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
   Attempts to determine the value of the expression at generation time.

   Supported are integer literals, arithmetic over such literals, local variables
   whose value is known (see GenerationContext.with_compile_time_integer) and
   top level value definitions.

   Returns None if the value cannot be determined.
*)
let rec extract_compile_time_integer (expression : Ast.Expression.t) : Z.t option GC.t =
  let extract_binary
      (operator : Z.t -> Z.t -> Z.t)
      (left     : Ast.Expression.t )
      (right    : Ast.Expression.t ) : Z.t option GC.t
    =
    let* left = extract_compile_time_integer left
    in
    (* only bother with the right operand if the left one worked out *)
    match left with
    | None      -> GC.return None
    | Some left -> begin
        let* right = extract_compile_time_integer right
        in
        match right with
        | None       -> GC.return None
        | Some right -> GC.return @@ Some (operator left right)
      end
  in
  match expression with
  | Value (Ast.Value.Int value)          -> GC.return @@ Some value
  | UnaryOperation (Neg, operand)        -> GC.lift ~f:(Option.map ~f:Z.neg) @@ extract_compile_time_integer operand
  | BinaryOperation (Plus , left, right) -> extract_binary Z.add left right
  | BinaryOperation (Minus, left, right) -> extract_binary Z.sub left right
  | BinaryOperation (Times, left, right) -> extract_binary Z.mul left right
  | Variable (identifier, _typ)          -> begin
      let* local = GC.lookup_compile_time_integer identifier
      in
      match local with
      (* locally bound: the local binding decides, even when it is unknown *)
      | Some value -> GC.return value
      (* not locally bound: fall back on the top level value definitions *)
      | None       -> try_lookup_integer_value_bound_to identifier
    end
  | TopLevelValue (identifier, _typ)     -> try_lookup_integer_value_bound_to identifier
  | _                                    -> GC.return None


(*
   Checks if the expression represents a compile time known string.
   This can take the form of either a literal or a constant variable.
*)
let extract_compile_time_string (expression : Ast.Expression.t) : string option GC.t =
  match expression with
  | Value (Ast.Value.String value) -> GC.return @@ Some value
  | Variable (identifier, _typ)    -> try_lookup_string_value_bound_to identifier
  | TopLevelValue (identifier, _)  -> try_lookup_string_value_bound_to identifier
  | _                              -> GC.return None


let translate_sail_zeros (arguments : Ast.Expression.t list) : PP.t GC.t =
  let pp_zeros (number_of_bits : int) : PP.t GC.t =
    GC.return begin
      if
        Configuration.(get bitvectors_zeros_ones_as_literal)
      then
        MuSail.Statement.pp_expression @@ MuSail.Expression.pp_zero_bitvector_using_literal number_of_bits
      else
        MuSail.Statement.pp_expression @@ MuSail.Expression.pp_zero_bitvector_using_function number_of_bits
    end
  in
  match arguments with
  | [ argument ] -> GC.with_context "argument 1" begin
      let* argument_value = extract_compile_time_integer argument
      in
      match argument_value with
      | Some number_of_bits -> begin
          GC.pp_annotate [%here] begin
            pp_zeros (Z.to_int number_of_bits)
          end
        end
      | None -> begin
          (* The width is not known at compile time: a foreign call (sail_zeros is an extern), whose semantics is bv.zero *)
          translate_call_or_hole ~message:"sail_zeros with a width not known at compile time" (Ast.Identifier.mk "sail_zeros") arguments
        end
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_zeros to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail [%here] message
    end


let translate_sail_ones (arguments : Ast.Expression.t list) : PP.t GC.t =
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
  | [ argument ] -> GC.with_context "argument 1" begin
      let* argument_value = extract_compile_time_integer argument
      in
      match argument_value with
      | Some number_of_bits -> begin
          GC.pp_annotate [%here] begin
            pp_ones (Z.to_int number_of_bits)
          end
        end
      | None -> begin
          (* The width is not known at compile time: a call to sail_ones (a Sail function, not_vec(sail_zeros(n))) *)
          translate_call (Ast.Identifier.mk "sail_ones") arguments
        end
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected sail_ones to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail [%here] message
    end


(*
   Sail's atoms_to_singletons rewrite wraps an integer n in make_the_value(n) to
   turn it into a value of the singleton type itself('n). muSail has no counterpart
   for this, and none is needed: the wrapper does not change the value, so we
   translate a call to make_the_value as its argument.
   The same holds for size_itself_int, its inverse, which turns a value of type
   itself('n) back into an int('n) (e.g., the implicit size argument of a function).
*)
let translate_make_the_value (arguments : Ast.Expression.t list) : PP.t GC.t =
  match arguments with
  | [ argument ] -> begin
      GC.pp_annotate [%here] begin
        let* value = extract_compile_time_integer argument
        in
        match value with
        (*
           Prefer the compile time value when we know it: the argument may well be
           a top level Sail variable, which has no muSail counterpart to refer to.
        *)
        | Some value -> GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_integer @@ MuSail.Value.pp_integer value
        | None       -> GC.lift ~f:MuSail.Statement.pp_expression @@ Expressions.pp_expression argument
      end
    end
  | _ -> begin
      let message =
        Printf.sprintf "expected make_the_value or size_itself_int to receive exactly one argument; instead got %d" (List.length arguments)
      in
      GC.fail [%here] message
    end


let translate_unit_equality () : PP.t GC.t =
  GC.pp_annotate [%here] begin
    GC.return @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_true ()
  end


(*
   Sail allows to add bitvectors and integers: add_bits_int(bv, n) : bits('n).
   MuSail only adds bitvectors to bitvectors of the same width,
   so we translate bv + n as bv + bv' where bv' represents n, wrapped around modulo 2^'n,
   which is what Sail's add_bits_int does too.

   * If n is known at compile time and the width of bv is a known constant (looking through
     aliases such as xlenbits), bv' is a bitvector literal of that width.
     This also covers top level Sail constants, which have no muSail counterpart to refer to.
   * Otherwise, bv' is the run time conversion uop.get_slice_int n,
     whose semantics is bv.of_Z, i.e., n modulo 2^'n.
     Rocq infers its width from bv, so the width of bv need not be known here.
     If n is known at compile time, it is passed as an integer literal, for the same reason
     as above.
*)
let translate_bits_int_operation
    ~(sail_name : string               )
    ~(infix     : PP.t                 )
    ~(name      : PP.t                 )
    (arguments  : Ast.Expression.t list) : PP.t GC.t
  =
  let pp_bitvector_addition
      (pp_bitvector_argument : PP.t)
      (pp_bitvector_addend   : PP.t) : PP.t GC.t
    =
    GC.pp_annotate [%here] begin
      translate_binary_operator (* todo support infix notation *)
        (Ast.Identifier.mk sail_name)
        ~infix:(Some infix)
        ~name:(Some name)
        [ PP.(surround parens) pp_bitvector_argument; PP.(surround parens) pp_bitvector_addend ]
    end
  in
  let pp_run_time_conversion (pp_integer : PP.t) : PP.t =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "exp_unop")
        [
          PP.string "uop.get_slice_int";
          PP.(surround parens) pp_integer;
        ]
    end
  in
  let pp_compile_time_addition
      (bitvector_argument : Ast.Expression.t)
      (integer_value      : Z.t             ) : PP.t GC.t
    =
    let* pp_bitvector_argument = Expressions.pp_expression bitvector_argument
    in
    (* We need the size of the left operand to build a literal of the same size *)
    match Ast.Type.strip_aliases @@ Ast.Expression.infer_type bitvector_argument with
    | Bitvector (Constant bitvector_size) -> begin
        let pp_bitvector_addend =
          MuSail.Expression.pp_bitvector ~size:(Z.to_int bitvector_size) ~value:integer_value
        in
        pp_bitvector_addition pp_bitvector_argument pp_bitvector_addend
      end
    | _ -> begin
        let pp_integer_literal =
          MuSail.Expression.pp_integer @@ MuSail.Value.pp_integer integer_value
        in
        pp_bitvector_addition pp_bitvector_argument (pp_run_time_conversion pp_integer_literal)
      end
  in
  let pp_run_time_addition
      (bitvector_argument : Ast.Expression.t)
      (integer_argument   : Ast.Expression.t) : PP.t GC.t
    =
    let* pp_bitvector_argument = Expressions.pp_expression bitvector_argument
    and* pp_integer_argument   = Expressions.pp_expression integer_argument
    in
    pp_bitvector_addition pp_bitvector_argument (pp_run_time_conversion pp_integer_argument)
  in
  match arguments with
  | [ bitvector_argument; integer_argument ] -> begin
      let* integer_argument_value = extract_compile_time_integer integer_argument
      in
      match integer_argument_value with
      | Some integer_value -> pp_compile_time_addition bitvector_argument integer_value
      | None               -> pp_run_time_addition bitvector_argument integer_argument
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_add_bits_int (arguments : Ast.Expression.t list) : PP.t GC.t =
  translate_bits_int_operation ~sail_name:"add_bits_int" ~infix:(PP.string "+ᵇ") ~name:(PP.string "bop.bvadd") arguments


(*
   Sail definition:

      val sail_shiftleft = pure "shiftl" : forall 'n ('ord : Order).
        (bitvector('n, 'ord), int) -> bitvector('n, 'ord)


   MuSail definition:

       shiftl {m n} : BinOp (bvec m) (bvec n) (bvec m)


   In other words, the parameter types differ.

   If the shift amount is known at compile time, it becomes a bitvector literal.
   Otherwise, it is converted at run time with uop.get_slice_int (bv.of_Z, i.e., modulo 2^n) into a bitvector
   as wide as the shifted one, so that amounts below 2^width, all those that do not shift out every bit, are exact.
   This needs the width of the shifted bitvector.
*)
let translate_shift
    ~(function_identifier : Ast.Identifier.t     )
    ~(musail_name         : PP.t                 )
    ~(arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let sail_name = Ast.Identifier.to_string function_identifier
  in
  let pp_shift
      (bitvector_argument : Ast.Expression.t)
      (shift_argument     : Z.t             ) : PP.t GC.t
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
           (Z.log2up was one bit short for powers of two: 2 became [bv 2] of width 1)
        *)
        Int.max 1 (Z.numbits shift_argument)
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
      | None           -> begin
          match Ast.Type.simplify @@ Ast.Type.strip_aliases @@ Ast.Expression.infer_type bitvector_argument with
          | Bitvector width -> begin
              let* pp_bitvector_argument =
                GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression bitvector_argument
              and* pp_width =
                Numeric.Expression.pp_argument width
              and* pp_shift_argument =
                Expressions.pp_expression shift_argument
              in
              let pp_converted_shift_argument =
                PP.(surround parens) begin
                  Coq.pp_application
                    (PP.string "exp_unop")
                    [
                      PP.(surround parens) @@ PP.horizontal [ PP.string "uop.get_slice_int (n := "; pp_width; PP.string ")" ];
                      PP.(surround parens) pp_shift_argument;
                    ]
                end
              in
              GC.pp_annotate [%here] begin
                translate_binary_operator_using_function_notation
                  (Ast.Identifier.mk sail_name)
                  musail_name
                  [ pp_bitvector_argument; pp_converted_shift_argument ]
              end
            end
          | _ -> translate_call_or_hole ~message:(Printf.sprintf "%s with a shift amount not known at compile time needs the width of the shifted bitvector" sail_name) function_identifier arguments
        end
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_shift_left
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let musail_name = PP.string "bop.shiftl"
  in
  GC.pp_annotate [%here] begin
    translate_shift ~function_identifier ~musail_name ~arguments
  end


let translate_shift_right
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let musail_name = PP.string "bop.shiftr"
  in
  GC.pp_annotate [%here] begin
    translate_shift ~function_identifier ~musail_name ~arguments
  end


let translate_extend
    ~(sail_name   : string               )
    ~(musail_name : string               )
    ~(arguments   : Ast.Expression.t list) : PP.t GC.t
  =
  let function_identifier = Ast.Identifier.mk sail_name
  in
  (* "Raw" version of pp_zero_extend that takes arguments in PP.t form *)
  let pp_zero_extend_raw
      (bitvector    : PP.t)
      (new_bit_size : PP.t) : PP.t GC.t
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
      (new_bit_size : int             ) : PP.t GC.t
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
      match bit_size_value, width_depends_on_size_variable bitvector_argument with
      | Some new_bit_size, false -> pp_zero_extend bitvector_argument (Z.to_int new_bit_size)
      | Some new_bit_size, true  -> begin
          (*
             The width of the extended bitvector is not a constant (a size parameter, kid__n):
             muSail's uop.zext/sext would need a proof that it is at most the new width, which only Sail's
             constraints give (P13). A foreign call to the extern has the same value and needs none.
          *)
          let* foreign_call = try_foreign_call function_identifier arguments
          in
          match foreign_call with
          | Some pp_foreign_call -> GC.return pp_foreign_call
          | None                 -> pp_zero_extend bitvector_argument (Z.to_int new_bit_size)
        end
      | None, _           -> begin
          (* The new width is not known at compile time: a foreign call if sail_name is an extern *)
          let* foreign_call = try_foreign_call function_identifier arguments
          in
          match foreign_call with
          | Some pp_foreign_call -> GC.return pp_foreign_call
          | None                 -> begin
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
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_zero_extend (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "sail_zero_extend"
  and musail_name = "zext"
  in
  translate_extend ~sail_name ~musail_name ~arguments


(*
   Sail

     val truncate : forall 'm 'n, 'm >= 0 & 'm <= 'n. (bits('n), int('m)) -> bits('m)

   becomes muSail's uop.truncate m, which keeps the m least significant bits.
   The new width must be known at compile time.
*)
let translate_truncate (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "truncate"
  in
  match arguments with
  | [ bitvector_argument; width_argument ] -> begin
      let* width = extract_compile_time_integer width_argument
      in
      match width, width_depends_on_size_variable bitvector_argument with
      | Some _, true -> begin
          (* as for sail_zero_extend: uop.truncate would need a proof that only Sail's constraints give *)
          translate_call (Ast.Identifier.mk sail_name) arguments
        end
      | None, _ -> begin
          (* The new width is not known at compile time: a call (a foreign call, as truncate is an extern) *)
          translate_call (Ast.Identifier.mk sail_name) arguments
        end
      | Some width, false -> begin
          let* pp_bitvector_argument =
            GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression bitvector_argument
          in
          GC.pp_annotate [%here] begin
            GC.return begin
              MuSail.Statement.pp_expression begin
                Coq.pp_application
                  (PP.string "exp_unop")
                  [
                    PP.string @@ Printf.sprintf "(uop.truncate %s)" (Z.to_string width);
                    pp_bitvector_argument
                  ]
              end
            end
          end
        end
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "wrong number of parameters for %s; should never occur" sail_name


let translate_sign_extend (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "sail_sign_extend"
  and musail_name = "sext"
  in
  translate_extend ~sail_name ~musail_name ~arguments


let translate_assertion (arguments : Ast.Expression.t list) : PP.t GC.t =
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


let translate_bitvector_concatenation (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "bitvector_concat"
  in
  let derive_vector_length (expression : Ast.Expression.t) : PP.t GC.t =
    match Ast.Expression.infer_type expression with
    | Bitvector (Constant n) -> GC.return @@ PP.string @@ Z.to_string n
    | _                      -> GC.not_yet_implemented ~message:"expected constant in bitvector type" [%here]
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
                pp_bv2_length;
                pp_bv1_length;
              ]
          end
      in
      translate_binary_operator
        (Ast.Identifier.mk sail_name)
        ~name:(Some binop_name)
        [
          PP.(surround parens) pp_bv2;
          PP.(surround parens) pp_bv1
        ]
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "%s should receive two bitvector arguments" sail_name


let translate_bitvector_slicing (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "subrange_bits"
  in
  match arguments with
  | [bitvector; first_index; second_index] -> begin
      let pp_slice
          (low_index : PP.t)
          (length    : PP.t)
          (bitvector : PP.t) : PP.t GC.t
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


let translate_bitvector_update_subrange (arguments : Ast.Expression.t list) : PP.t GC.t =
  let sail_name = "update_subrange_bits"
  in
  match arguments with
  | [bitvector; first_index; second_index; new_bitvector] -> begin
      let pp_slice
          (low_index     : PP.t)
          (length        : PP.t)
          (bitvector     : PP.t)
          (new_bitvector : PP.t) : PP.t GC.t
        =
        translate_binary_operator_using_function_notation
          (Ast.Identifier.mk sail_name)
          (
            PP.(surround parens) begin
              Coq.pp_application
                (PP.string "bop.update_vector_subrange")
                [ low_index; length ]
            end
          )
          [ PP.(surround parens) bitvector; PP.(surround parens) new_bitvector ]
      in
      (*
        How indices need to be interpreted depends on the order
        Here we assume that "backward slices" are not possible, i.e.,
        that the range goes from the lowest index to the highest index.
      *)
      let* pp_bitvector = Expressions.pp_expression bitvector
      and* first_index  = extract_compile_time_integer first_index
      and* second_index = extract_compile_time_integer second_index
      and* pp_new_bitvector = Expressions.pp_expression new_bitvector
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
            pp_new_bitvector
        end
      | _ -> begin
          let* low_index = GC.not_yet_implemented ~message:"%s's indices should be known at compile time" [%here]
          and* length    = GC.not_yet_implemented ~message:"%s's indices should be known at compile time" [%here]
          in
          pp_slice low_index length pp_bitvector pp_new_bitvector
        end
    end
  | _ -> GC.fail [%here] @@ Printf.sprintf "%s should receive three arguments" sail_name


(*
   A call to an extern (see ForeignFunctions).
   Externs that correspond to muSail operations become those operations, when their arguments allow it;
   all other calls become foreign calls (stm_foreign). The externs are recognised by their binding.

     shiftl, shiftr (bits, int)              bop.shiftl, bop.shiftr (see translate_shift)
     shift_bits_left, shift_bits_right       bop.shiftl, bop.shiftr
     sub_vec                                 bop.bvsub
     sub_vec_int (bits, int)                 bop.bvsub, the integer converted as in add_bits_int
     eq_anything                             bop.relop bop.eq
     vector_access (bits, i)                 uop.vector_subrange i 1 (a bit is a bitvector(1));
                                             i and the width must be known at compile time
     get_slice_int (w, n, 0)                 uop.get_slice_int (n := w); w must be known at compile time
     sail_truncateLSB (v, m)                 uop.vector_subrange (|v| - m) m; m and |v| must be known at compile time
*)
let translate_extern_call
    (foreign_function : ForeignFunctions.t   )
    (arguments        : Ast.Expression.t list)
    (pp_arguments     : PP.t list            ) : PP.t GC.t
  =
  let function_identifier = foreign_function.identifier
  in
  let foreign_call () : PP.t GC.t =
    translate_call function_identifier arguments
  in
  let pp_unop (operator : PP.t) (pp_operand : PP.t) : PP.t =
    MuSail.Statement.pp_expression @@ Coq.pp_application (PP.string "exp_unop") [ PP.(surround parens) operator; PP.(surround parens) pp_operand ]
  in
  GC.pp_annotate [%here] begin
    match ForeignFunctions.binding_key foreign_function with
    | "shiftl"           -> translate_shift_left function_identifier arguments
    | "shiftr"           -> translate_shift_right function_identifier arguments
    | "shift_bits_left"  -> translate_binary_operator_using_function_notation function_identifier (PP.string "bop.shiftl") pp_arguments
    | "shift_bits_right" -> translate_binary_operator_using_function_notation function_identifier (PP.string "bop.shiftr") pp_arguments
    | "sub_vec"          -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.subtraction)
          ~name:(Some MuSail.Operator.Name.bitvector_subtraction)
          pp_arguments
      end
    | "sub_vec_int"      -> begin
        translate_bits_int_operation
          ~sail_name:(Ast.Identifier.to_string function_identifier)
          ~infix:MuSail.Operator.Infix.Bitvector.subtraction
          ~name:MuSail.Operator.Name.bitvector_subtraction
          arguments
      end
    | "eq_anything"      -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bool_equality)
          ~name:(Some MuSail.Operator.Name.bool_equality)
          pp_arguments
      end
    | "vector_access"    -> begin
        match arguments, pp_arguments with
        | [ bitvector; index ], [ pp_bitvector; _ ] -> begin
            let* index = extract_compile_time_integer index
            in
            (* the result is a bit, i.e., a bitvector(1) *)
            match index, constant_bitvector_width bitvector with
            | Some index, Some _ -> GC.return @@ pp_unop (PP.string @@ Printf.sprintf "uop.vector_subrange %s 1" (Z.to_string index)) pp_bitvector
            | _, _ -> foreign_call ()
          end
        | _, _ -> foreign_call ()
      end
    | "get_slice_int"    -> begin
        match arguments, pp_arguments with
        | [ width; _; start ], [ _; pp_integer; _ ] -> begin
            let* width = extract_compile_time_integer width
            and* start = extract_compile_time_integer start
            in
            match width, start with
            | Some width, Some start when Z.equal start Z.zero -> begin
                GC.return @@ pp_unop (PP.string @@ Printf.sprintf "uop.get_slice_int (n := %s)" (Z.to_string width)) pp_integer
              end
            | _, _ -> foreign_call ()
          end
        | _, _ -> foreign_call ()
      end
    | "sail_truncateLSB" -> begin
        match arguments, pp_arguments with
        | [ bitvector; kept ], [ pp_bitvector; _ ] -> begin
            let* kept = extract_compile_time_integer kept
            in
            match kept, constant_bitvector_width bitvector with
            | Some kept, Some width when Z.leq kept width -> begin
                GC.return @@ pp_unop (PP.string @@ Printf.sprintf "uop.vector_subrange %s %s" (Z.to_string (Z.sub width kept)) (Z.to_string kept)) pp_bitvector
              end
            | _, _ -> foreign_call ()
          end
        | _, _ -> foreign_call ()
      end
    | _ -> foreign_call ()
  end


let translate
    (function_identifier : Ast.Identifier.t     )
    (arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  let* pp_arguments =
    GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
  in
  GC.pp_annotate [%here] begin
    match Ast.Identifier.to_string function_identifier with
    | "eq_bit" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bit_equality)
          pp_arguments
      end
    | "eq_bits" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.equality)
          pp_arguments
      end
    | "add_bits" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.addition)
          ~name:(Some MuSail.Operator.Name.bitvector_addition)
          pp_arguments
      end
    | "sub_bits" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.Bitvector.subtraction)
          ~name:(Some MuSail.Operator.Name.bitvector_subtraction)
          pp_arguments
      end
    | "and_vec" -> begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_conjunction)
          pp_arguments
      end
    | "or_vec" -> begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_disjunction)
          pp_arguments
      end
    | "xor_vec" -> begin
        translate_binary_operator
          function_identifier
          ~name:(Some MuSail.Operator.Name.bitvector_xor)
          pp_arguments
      end
    | "eq_bool" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bool_equality)
          ~name:(Some MuSail.Operator.Name.bool_equality)
          pp_arguments
      end
    | "neq_bool" -> begin
        translate_binary_operator
          function_identifier
          ~infix:(Some MuSail.Operator.Infix.bool_equality)
          ~name:(Some MuSail.Operator.Name.bool_inequality)
          pp_arguments
      end
    | "not_bool"             -> translate_unary_operator function_identifier (PP.string "uop.not") pp_arguments
    | "not_vec"              -> translate_unary_operator function_identifier (PP.string "uop.bvnot") pp_arguments
    | "signed"               -> translate_unary_operator function_identifier (PP.string "uop.signed") pp_arguments
    | "unsigned"             -> translate_unary_operator function_identifier (PP.string "uop.unsigned") pp_arguments
    | "eq_unit"              -> translate_unit_equality ()
    | "make_the_value"       -> translate_make_the_value arguments
    | "size_itself_int"      -> translate_make_the_value arguments
    | "add_bits_int"         -> translate_add_bits_int arguments
    | "sail_zeros"           -> translate_sail_zeros arguments
    | "sail_ones"            -> translate_sail_ones arguments
    | "sail_shiftleft"       -> translate_shift_left function_identifier arguments
    | "sail_shiftright"      -> translate_shift_right function_identifier arguments
    | "sail_zero_extend"     -> translate_zero_extend arguments
    | "truncate"             -> translate_truncate arguments
    | "sail_sign_extend"     -> translate_sign_extend arguments
    | "sail_assert"          -> translate_assertion arguments
    | "bitvector_concat"     -> translate_bitvector_concatenation arguments
    | "subrange_bits"        -> translate_bitvector_slicing arguments
    | "update_subrange_bits" -> translate_bitvector_update_subrange arguments
    | _ -> begin
        let* foreign_function = ForeignFunctions.find function_identifier
        in
        match foreign_function with
        | Some foreign_function -> translate_extern_call foreign_function arguments pp_arguments
        | None                  -> begin
            GC.return begin
              pp_function_call_using_configured_syntax
                (Identifier.pp function_identifier)
                pp_arguments
            end
          end
      end
  end
