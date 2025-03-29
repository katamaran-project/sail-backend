open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_infix_binary_operation (binary_operator : Ast.BinaryOperator.t) : PP.t GC.t =
  GC.pp_annotate [%here] begin
    match binary_operator with
    | Plus                                                 -> GC.return Coq.Operator.addition
    | Minus                                                -> GC.return Coq.Operator.subtraction
    | Times                                                -> GC.return Coq.Operator.multiplication
    | And                                                  -> GC.return Coq.Operator.conjunction
    | Or                                                   -> GC.return Coq.Operator.disjunction
    | EqualTo                                              -> GC.return Coq.Operator.equality
    | NotEqualTo                                           -> GC.return Coq.Operator.inequality
    | StandardComparison LessThan                          -> GC.return Coq.Operator.less_than
    | StandardComparison GreaterThan                       -> GC.return Coq.Operator.greater_than
    | StandardComparison LessThanOrEqualTo                 -> GC.return Coq.Operator.less_than_or_equal_to
    | StandardComparison GreaterThanOrEqualTo              -> GC.return Coq.Operator.greater_than_or_equals_to
    | BitvectorComparison (Signed, LessThan)               -> GC.return MuSail.Operator.Infix.Bitvector.signed_less_than
    | BitvectorComparison (Signed, LessThanOrEqualTo)      -> GC.return MuSail.Operator.Infix.Bitvector.signed_less_than_or_equal_to
    | BitvectorComparison (Signed, GreaterThan)            -> GC.return MuSail.Operator.Infix.Bitvector.signed_greater_than
    | BitvectorComparison (Signed, GreaterThanOrEqualTo)   -> GC.return MuSail.Operator.Infix.Bitvector.signed_greater_than_or_equal_to
    | BitvectorComparison (Unsigned, LessThan)             -> GC.return MuSail.Operator.Infix.Bitvector.signed_less_than
    | BitvectorComparison (Unsigned, LessThanOrEqualTo)    -> GC.return MuSail.Operator.Infix.Bitvector.signed_less_than_or_equal_to
    | BitvectorComparison (Unsigned, GreaterThan)          -> GC.return MuSail.Operator.Infix.Bitvector.signed_greater_than
    | BitvectorComparison (Unsigned, GreaterThanOrEqualTo) -> GC.return MuSail.Operator.Infix.Bitvector.signed_greater_than_or_equal_to
    | Pair                                                 -> GC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
    | Cons                                                 -> GC.not_yet_implemented [%here] (* Should not occur *)
    | Append                                               -> GC.not_yet_implemented [%here] (* Should not occur *)
  end


let rec pp_expression (expression : Ast.Expression.t) : PP.t GC.t =
  let rec pp_value (value : Ast.Value.t) : PP.t GC.t =
    GC.pp_annotate [%here] begin
        match value with
        | Bool true        -> GC.return @@ MuSail.Expression.pp_true ()
        | Bool false       -> GC.return @@ MuSail.Expression.pp_false ()
        | Int n            -> GC.return @@ MuSail.Expression.pp_integer @@ MuSail.Value.pp_integer n
        | String s         -> GC.return @@ MuSail.Expression.pp_string @@ Coq.pp_string s
        | Unit             -> GC.return @@ MuSail.Expression.pp_unit ()
        | Prod (_, _) as v -> begin
            let* pp_tuple_type =
              Type.pp_nanotype (Ast.Value.type_of_value v)
            and* pp_value' =
              pp_value v
            in
            GC.return begin
              MuSail.Expression.pp_value ~typ:pp_tuple_type ~value:pp_value'
            end
          end
        | Bit b -> begin
            GC.return begin
                if b
                then MuSail.Expression.pp_true ()
                else MuSail.Expression.pp_false ()
              end
          end
        | Bitvector _ -> GC.not_yet_implemented [%here]
      end

  and pp_binary_operation
      (binary_operator : Ast.BinaryOperator.t)
      (left_operand    : Ast.Expression.t    )
      (right_operand   : Ast.Expression.t    ) : PP.t GC.t
    =
    let* pp_left_operand  =
      GC.lift ~f:PP.(surround parens) @@ pp_expression left_operand
    and* pp_right_operand =
      GC.lift ~f:PP.(surround parens) @@ pp_expression right_operand
    in
    let pp id =
      GC.return begin
        Coq.pp_application (PP.string "exp_binop") [
          PP.string id;
          pp_left_operand;
          pp_right_operand
        ]
      end
    in
    GC.pp_annotate [%here] begin
        match binary_operator with
        | Pair   -> pp "bop.pair"
        | Cons   -> pp "bop.cons"
        | Append -> pp "bop.append"
        | _      -> begin
            let* binop' =
              pp_infix_binary_operation binary_operator
            in
            GC.return begin
              PP.horizontal [
                PP.(surround parens) pp_left_operand;
                binop';
                PP.(surround parens) pp_right_operand;
              ]
            end
          end
      end

  and pp_variable (identifier : Ast.Identifier.t) : PP.t GC.t =
    GC.return begin
        PP.annotate [%here] begin
            MuSail.Expression.pp_variable @@ Identifier.pp identifier
          end
      end

  and pp_unary_operation
      (operator : Ast.UnaryOperator.t)
      (operand  : Ast.Expression.t   ) : PP.t GC.t
    =
    GC.pp_annotate [%here] begin
      match operator with
      | Neg -> pp_negation operand
      | Not -> pp_logical_negation operand
    end

  and pp_negation (operand : Ast.Expression.t) : PP.t GC.t =
    let* pp_operand =
      GC.lift ~f:PP.(surround parens) @@ pp_expression operand
    in
    GC.return begin
        PP.annotate [%here] begin
            PP.(separate_horizontally ~separator:space [minus; pp_operand])
          end
      end

  and pp_logical_negation (operand : Ast.Expression.t) : PP.t GC.t =
    let* pp_operand =
      GC.lift ~f:PP.(surround parens) @@ pp_expression operand
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application (PP.string "exp_not") [ pp_operand ]
      end
    end

  and pp_list (elements : Ast.Expression.t list) : PP.t GC.t =
    let* pp_elements =
      GC.map ~f:pp_expression elements
    in
    let pp_list =
      Coq.pp_list ~use_notation:Configuration.(get pretty_print_lists) pp_elements
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application (PP.string "exp_list") [ pp_list ]
      end
    end

  and pp_record
      (type_identifier : Ast.Identifier.t     )
      (fields          : Ast.Identifier.t list) : PP.t GC.t
    =
    let pp_record_type =
      Identifier.pp @@ Identifier.reified_record_name type_identifier;
    and pp_record_fields =
      Coq.pp_list_using_notation begin
        List.map
          fields
          ~f:(fun id -> begin
                PP.annotate [%here] begin
                  MuSail.Expression.pp_variable (Identifier.pp id)
                end
              end)
      end
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application (PP.string "exp_record") [
          pp_record_type;
          pp_record_fields;
        ]
      end
    end

  and pp_enum
        (type_identifier : Ast.Identifier.t)
        (constructor_identifier : Ast.Identifier.t) : PP.t GC.t
    =
    let enum_type =
      Coq.pp_application
        (PP.string "ty.enum")
        [ Identifier.pp @@ Identifier.reified_enum_name type_identifier ]
    and enum_constructor =
      Identifier.pp constructor_identifier
    in
    GC.return begin
        PP.annotate [%here] begin
          MuSail.Expression.pp_value
            ~typ:enum_type
            ~value:enum_constructor
          end
      end

  and pp_variant
        (type_identifier        : Ast.Identifier.t     )
        (constructor_identifier : Ast.Identifier.t     )
        (fields                 : Ast.Expression.t list) : PP.t GC.t
    =
    let reified_variant_identifier =
      Identifier.reified_variant_name type_identifier
    and reified_constructor_identifier =
      Identifier.reified_variant_constructor_name constructor_identifier
    in
    let pp_variant_identifier =
      Identifier.pp reified_variant_identifier
    and pp_constructor_identifier =
      Identifier.pp reified_constructor_identifier
    in
    let* pp_fields =
      (*
         The number of fields determines how the union value should be represented.

           ()          : tt
           (x)         : x
           (x, y)      : pair of x y
           (x, y, ...) : tuple of x y ...
      *)
      match fields with
      | []     -> GC.lift ~f:PP.(surround parens) @@ pp_expression @@ Ast.Expression.Value Ast.Value.Unit
      | [x]    -> GC.lift ~f:PP.(surround parens) @@ pp_expression x
      | [x; y] -> GC.lift ~f:PP.(surround parens) @@ pp_expression @@ Ast.Expression.BinaryOperation (Ast.BinaryOperator.Pair, x, y)
      | xs     -> GC.lift ~f:PP.(surround parens) @@ pp_expression @@ Ast.Expression.Tuple xs
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "exp_union")
          [
            pp_variant_identifier;
            pp_constructor_identifier;
            pp_fields
          ]
      end
    end

  and pp_tuple (elements : Ast.Expression.t list) : PP.t GC.t =
    let* pp_elements =
      GC.map ~f:pp_expression elements
    in
    GC.return @@ Coq.pp_application
      (PP.string "exp_tuple")
      [Coq.pp_list pp_elements]

  and pp_bitvector (elements : Ast.Expression.t list) : PP.t GC.t =
    (*
       We currently one support bitvector expressions whose elements are all known at compile-time.
    *)
    let compile_time_digits : bool list option =
      (* Returns the bit (as Some bit) if it's known at compile time, otherwise None *)
      let get_compile_time_digit (expression : Ast.Expression.t) : bool option =
        match expression with
        | Value (Ast.Value.Bit bit) -> Some bit
        | _                         -> None
      in
      Option.all @@ List.map ~f:get_compile_time_digit elements
    in
    match compile_time_digits with
    | Some bits -> begin
        (* Compute the value represented by the bits *)
        let size =
          List.length bits
        in
        let value =
          Util.convert_bits_to_z bits
        in
        GC.return begin
          MuSail.Expression.pp_bitvector
            ~size
            ~value
        end
      end
    | None -> GC.not_yet_implemented ~message:"bitvectors with compile-time unknown elements not supported yet" [%here]

  in
  GC.pp_annotate [%here] begin
    match expression with
    | Variable (identifier, _typ)                       -> pp_variable identifier
    | Value value                                       -> pp_value value
    | UnaryOperation (operator, operand)                -> pp_unary_operation operator operand
    | BinaryOperation (op, e1, e2)                      -> pp_binary_operation op e1 e2
    | List elements                                     -> pp_list elements
    | Record { type_identifier; fields }                -> pp_record type_identifier fields
    | Enum { type_identifier; constructor_identifier }  -> pp_enum type_identifier constructor_identifier
    | Variant { type_identifier;
                constructor_identifier;
                fields    }                             -> pp_variant type_identifier constructor_identifier fields
    | Tuple elements                                    -> pp_tuple elements
    | Bitvector elements                                -> pp_bitvector elements
  end
