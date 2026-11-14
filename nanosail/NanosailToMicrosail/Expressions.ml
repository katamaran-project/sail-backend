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
    | BitvectorComparison (Unsigned, LessThan)             -> GC.return MuSail.Operator.Infix.Bitvector.unsigned_less_than
    | BitvectorComparison (Unsigned, LessThanOrEqualTo)    -> GC.return MuSail.Operator.Infix.Bitvector.unsigned_less_than_or_equal_to
    | BitvectorComparison (Unsigned, GreaterThan)          -> GC.return MuSail.Operator.Infix.Bitvector.unsigned_greater_than
    | BitvectorComparison (Unsigned, GreaterThanOrEqualTo) -> GC.return MuSail.Operator.Infix.Bitvector.unsigned_greater_than_or_equal_to
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
              Type.pp_type (Ast.Value.type_of_value v)
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
        | Record _
        | Enum _ as v -> begin
            let* pp_value_type = Type.pp_type (Ast.Value.type_of_value v)
            and* pp_value'     = ValueDefinitions.pp_value v
            in
            GC.return @@ MuSail.Expression.pp_value ~typ:pp_value_type ~value:pp_value'
          end
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

  (*
     exp_val <type> <identifier>, where <identifier> is the Rocq definition of the top-level value
  *)
  and pp_top_level_value
      (identifier : Ast.Identifier.t)
      (typ        : Ast.Type.t      ) : PP.t GC.t
    =
    let* pp_value_type = Type.pp_type typ
    and* rocq_name     = ValueDefinitions.rocq_name_of_top_level_value identifier
    in
    GC.return begin
      PP.annotate [%here] begin
        MuSail.Expression.pp_value ~typ:pp_value_type ~value:(Identifier.pp rocq_name)
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

  (*
     Injection into a sum:

       exp_unop (uop.inl (σ2 := <right_type>)) (<value>)
       exp_unop (uop.inr (σ1 := <left_type>)) (<value>)

     This is exp_inl/exp_inr, except that we explicitly mention the type of the other side,
     because Coq cannot always infer it from the context.
  *)
  and pp_injection
      (side       : Ast.SumSide.t   )
      (left_type  : Ast.Type.t      )
      (right_type : Ast.Type.t      )
      (value      : Ast.Expression.t) : PP.t GC.t
    =
    let* pp_value =
      GC.lift ~f:PP.(surround parens) @@ pp_expression value
    in
    let* pp_operator =
      let pp_operator
          (operator        : string    )
          (other_parameter : string    )
          (other_type      : Ast.Type.t) : PP.t GC.t
        =
        let* pp_other_type = Type.pp_type other_type
        in
        GC.return begin
          PP.(surround parens) begin
            Coq.pp_application
              (PP.string operator)
              [
                PP.(surround parens) begin
                  PP.horizontal [ PP.string other_parameter; PP.string " := "; pp_other_type ]
                end
              ]
          end
        end
      in
      match side with
      | Left  -> pp_operator "uop.inl" "σ2" right_type
      | Right -> pp_operator "uop.inr" "σ1" left_type
    in
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "exp_unop")
          [
            pp_operator;
            pp_value;
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

  (*
     A bitvector built from single bits, most significant bit first (Sail's [e1, ..., en]).

     If all bits are known at compile time, it becomes a bitvector literal.
     Otherwise, each element that is not known is a bitvector of length 1 (in Sail, bit is bitvector(1)),
     and the whole is the concatenation of the elements, where runs of known bits form one literal:

       [b, 0b1, 0b0, c]   becomes   (b @ 0b10) @ c

     Concatenation is bop.bvapp, whose first operand holds the least significant bits:
     x @ y is generated as exp_binop (@bop.bvapp _ |y| |x|) y x (see FunctionCalls.translate_bitvector_concatenation).
  *)
  and pp_bitvector (elements : Ast.Expression.t list) : PP.t GC.t =
    (*
       Bit literals (bitone, 0b1) are themselves translated as bitvectors of one bit (see Function.expression_of_literal);
       their bits are spliced in.
    *)
    let elements =
      let rec flatten (expression : Ast.Expression.t) : Ast.Expression.t list =
        match expression with
        | Bitvector inner_elements -> List.concat_map ~f:flatten inner_elements
        | Variable _
        | TopLevelValue _
        | Value _
        | List _
        | UnaryOperation _
        | BinaryOperation _
        | Record _
        | Enum _
        | Variant _
        | Injection _
        | Tuple _                  -> [ expression ]
      in
      List.concat_map ~f:flatten elements
    in
    (* Returns the bit if it is known at compile time *)
    let get_compile_time_digit (expression : Ast.Expression.t) : bool option =
      match expression with
      | Value (Ast.Value.Bit bit)         -> Some bit
      | Value (Ast.Value.Bitvector [bit]) -> Some bit
      | Value (Ast.Value.Bitvector _)     -> None
      | Value (Ast.Value.Unit)            -> None
      | Value (Ast.Value.Bool _)          -> None
      | Value (Ast.Value.Int _)           -> None
      | Value (Ast.Value.String _)        -> None
      | Value (Ast.Value.Prod _)          -> None
      | Value (Ast.Value.Record _)        -> None
      | Value (Ast.Value.Enum _)          -> None
      | Variable _
      | TopLevelValue _
      | List _
      | UnaryOperation _
      | BinaryOperation _
      | Record _
      | Enum _
      | Variant _
      | Injection _
      | Tuple _
      | Bitvector _                       -> None
    in
    let compile_time_digits : bool list option =
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
    | None -> begin
        (*
           Chunks of the bitvector, most significant first, with their length:
           a run of known bits, or a single element of length 1.
        *)
        let* chunks : (PP.t * int) list =
          let pp_known_bits (bits : bool list) : PP.t * int =
            let size = List.length bits
            in
            (MuSail.Expression.pp_bitvector ~size ~value:(Util.convert_bits_to_z bits), size)
          in
          let rec chunk
              (known_bits : bool list            )  (* run of known bits seen so far, in reverse *)
              (elements   : Ast.Expression.t list) : (PP.t * int) list GC.t
            =
            let flush (chunks : (PP.t * int) list) : (PP.t * int) list =
              match known_bits with
              | []     -> chunks
              | _ :: _ -> pp_known_bits (List.rev known_bits) :: chunks
            in
            match elements with
            | [] -> GC.return @@ flush []
            | element :: rest -> begin
                match get_compile_time_digit element with
                | Some bit -> chunk (bit :: known_bits) rest
                | None     -> begin
                    let* pp_element = pp_expression element
                    and* rest       = chunk [] rest
                    in
                    GC.return @@ flush ((pp_element, 1) :: rest)
                  end
              end
          in
          chunk [] elements
        in
        let concatenate
            ((low,  low_size ) : PP.t * int)
            ((high, high_size) : PP.t * int) : PP.t * int
          =
          let operator =
            PP.(surround parens) begin
              Coq.pp_explicit_application
                (PP.string "bop.bvapp")
                [
                  PP.string "_";
                  PP.string @@ Int.to_string low_size;
                  PP.string @@ Int.to_string high_size;
                ]
            end
          in
          let concatenation =
            Coq.pp_application
              (PP.string "exp_binop")
              [
                operator;
                PP.(surround parens) low;
                PP.(surround parens) high;
              ]
          in
          (concatenation, low_size + high_size)
        in
        (* Fold from the least significant chunk: each more significant chunk is appended on the high side *)
        match List.rev chunks with
        | []                   -> GC.fail [%here] "empty bitvector"
        | least :: more        -> begin
            let (result, _size) =
              List.fold more ~init:least ~f:concatenate
            in
            GC.return @@ PP.annotate [%here] result
          end
      end

  in
  GC.pp_annotate [%here] begin
    match expression with
    | Variable (identifier, _typ)                       -> pp_variable identifier
    | TopLevelValue (identifier, typ)                   -> pp_top_level_value identifier typ
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
    | Injection { side;
                  left_type;
                  right_type;
                  value }                               -> pp_injection side left_type right_type value
  end
