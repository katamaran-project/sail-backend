open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_infix_binary_operation (binary_operator : Ast.BinaryOperator.t) : PP.document GC.t =
  match binary_operator with
  | Plus                 -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.addition
  | Minus                -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.subtraction
  | Times                -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.multiplication
  | And                  -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.conjunction
  | Or                   -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.disjunction
  | EqualTo              -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.equality
  | NotEqualTo           -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.inequality
  | LessThan             -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.less_than
  | GreaterThan          -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.greater_than
  | LessThanOrEqualTo    -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.less_than_or_equal_to
  | GreaterThanOrEqualTo -> GC.pp_annotate [%here] @@ GC.return Coq.Operator.greater_than_or_equals_to
  | Pair                 -> GC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
  | Cons                 -> GC.not_yet_implemented [%here] (* Should not occur *)
  | Append               -> GC.not_yet_implemented [%here] (* Should not occur *)


let rec pp_expression (expression : Ast.Expression.t) : PP.document GC.t =
  let rec pp_value (value : Ast.Value.t) : PP.document GC.t =
    GC.pp_annotate [%here] begin
        match value with
        | Bool true        -> GC.return @@ PP.string "exp_true"
        | Bool false       -> GC.return @@ PP.string "exp_false"
        | Int n            -> GC.return @@ Coq.pp_application (PP.string "exp_int"   ) [ Coq.pp_integer n                    ]
        | String s         -> GC.return @@ Coq.pp_application (PP.string "exp_string") [ PP.(surround dquotes @@ string s)   ]
        | Unit             -> GC.return @@ Coq.pp_application (PP.string "exp_val"   ) [ PP.string "ty.unit"; PP.string "tt" ]
        | Prod (_, _) as v -> begin
            let* pp_tuple_type =
              GC.pp_annotate [%here] @@ Nanotype.pp_nanotype (Ast.Value.type_of_value v)
            in
            let* pp_value' =
              GC.pp_annotate [%here] @@ pp_value v
            in
            GC.return begin
                PP.annotate [%here] begin
                    Coq.pp_application
                      (PP.string "exp_val")
                      [
                        pp_tuple_type;
                        pp_value'
                      ]
                  end
              end
          end
      end

  and pp_binary_operation
      (binary_operator : Ast.BinaryOperator.t)
      (left_operand    : Ast.Expression.t    )
      (right_operand   : Ast.Expression.t    ) : PP.document GC.t
    =
    let* pp_left_operand  =
      GC.pp_annotate [%here] begin
          GC.lift ~f:PP.(surround parens) @@ pp_expression left_operand
        end
    and* pp_right_operand =
      GC.pp_annotate [%here] begin
          GC.lift ~f:PP.(surround parens) @@ pp_expression right_operand
        end
    in
    let pp id =
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_application (PP.string "exp_binop") [
                  PP.string id;
                  pp_left_operand;
                  pp_right_operand
                ]
            end
        end
    in
    GC.pp_annotate [%here] begin
        match binary_operator with
        | Pair   -> pp "bop.pair"
        | Cons   -> pp "bop.cons"
        | Append -> pp "bop.append"
        | _      -> begin
            let* binop' =
              GC.pp_annotate [%here] @@ pp_infix_binary_operation binary_operator
            in
            GC.return begin
                PP.annotate [%here] begin
                    PP.horizontal [
                        PP.(surround parens) pp_left_operand;
                        binop';
                        PP.(surround parens) pp_right_operand;
                      ]
                  end
              end
          end
      end

  and pp_variable (identifier : Ast.Identifier.t) : PP.document GC.t =
    GC.return begin
        PP.annotate [%here] begin
            PPSail.pp_expression_of_identifier @@ Identifier.pp identifier
          end
      end

  and pp_unary_operation
      (operator : Ast.UnaryOperator.t)
      (operand  : Ast.Expression.t   ) : PP.document GC.t
    =
    match operator with
    | Ast.UnaryOperator.Neg -> GC.pp_annotate [%here] @@ pp_negation operand
    | Ast.UnaryOperator.Not -> GC.pp_annotate [%here] @@ pp_logical_negation operand

  and pp_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand =
      GC.pp_annotate [%here] begin
          GC.lift ~f:PP.(surround parens) @@ pp_expression operand
        end
    in
    GC.return begin
        PP.annotate [%here] begin
            PP.(separate_horizontally ~separator:space [minus; pp_operand])
          end
      end

  and pp_logical_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand =
      GC.pp_annotate [%here] begin
          GC.lift ~f:PP.(surround parens) @@ pp_expression operand
        end
    in
    GC.return begin
        PP.annotate [%here] begin
            Coq.pp_application (PP.string "exp_not") [ pp_operand ]
          end
      end

  and pp_list (elements : Ast.Expression.t list) : PP.document GC.t =
    let* pp_elements =
      GC.map ~f:pp_expression elements
    in
    let pp_list =
      PP.annotate [%here] begin
          Coq.pp_list ~use_notation:Configuration.(get use_list_notations) pp_elements
        end
    in
    GC.return begin
        PP.annotate [%here] begin
            Coq.pp_application (PP.string "exp_list") [ pp_list ]
          end
      end

  and pp_record
      (type_identifier      : Ast.Identifier.t     )
      (variable_identifiers : Ast.Identifier.t list) : PP.document GC.t
    =
    let pp_record_type =
      PP.annotate [%here] begin
          Identifier.pp @@ Configuration.reified_record_name type_identifier;
        end
      
    and pp_record_fields =
      PP.annotate [%here] begin
          Coq.pp_list_using_notation begin
              List.map
                variable_identifiers
                ~f:(fun id -> begin
                        PP.annotate [%here] begin
                            Coq.pp_application
                              (PP.string "exp_var")
                              [ PP.(surround dquotes) @@ Identifier.pp id ]
                          end
                      end)
            end
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
        (constructor_identifier : Ast.Identifier.t) : PP.document GC.t
    =
    let enum_type =
      PP.annotate [%here] begin
          Coq.pp_application
            (PP.string "ty.enum")
            [ Identifier.pp @@ Configuration.reified_enum_name type_identifier ]
        end

    and enum_constructor =
      PP.annotate [%here] @@ Identifier.pp constructor_identifier

    in
    GC.return begin
        PP.annotate [%here] begin
            PPSail.pp_expression_value enum_type enum_constructor
          end
      end

  and pp_variant 
        (type_identifier        : Ast.Identifier.t     )
        (constructor_identifier : Ast.Identifier.t     )
        (arguments              : Ast.Expression.t list) : PP.document GC.t
    =
    let reified_variant_identifier =
      Configuration.reified_variant_name type_identifier
    and reified_constructor_identifier =
      Configuration.reified_variant_constructor_name constructor_identifier
    in
    let pp_variant_identifier =
      Identifier.pp reified_variant_identifier
    and pp_constructor_identifier =
      Identifier.pp reified_constructor_identifier
    in
    let* pp_arguments =
      match arguments with
      | []     -> GC.pp_annotate [%here] @@ pp_expression @@ Ast.Expression.Val Ast.Value.Unit
      | [x]    -> GC.pp_annotate [%here] @@ GC.lift ~f:PP.(surround parens) @@ pp_expression x
      | [x; y] -> GC.pp_annotate [%here] @@ GC.lift ~f:PP.(surround parens) @@ pp_expression @@ Ast.Expression.BinaryOperation (Ast.BinaryOperator.Pair, x, y)
      | xs     -> GC.not_yet_implemented [%here]
        (* GC.pp_annotate [%here] @@ GC.lift ~f:PP.(surround parens) @@ pp_expression @@ Ast.Expression.Tuple xs *)
    in
    GC.return @@ Coq.pp_application
      (PP.string "exp_union")
      [
        pp_variant_identifier;
        pp_constructor_identifier;
        pp_arguments
      ]

  in
  match expression with
  | Variable identifier                               -> GC.pp_annotate [%here] @@ pp_variable identifier
  | Val value                                         -> GC.pp_annotate [%here] @@ pp_value value
  | UnaryOperation (operator, operand)                -> GC.pp_annotate [%here] @@ pp_unary_operation operator operand
  | BinaryOperation (op, e1, e2)                      -> GC.pp_annotate [%here] @@ pp_binary_operation op e1 e2
  | List elements                                     -> GC.pp_annotate [%here] @@ pp_list elements
  | Record { type_identifier; variable_identifiers }  -> GC.pp_annotate [%here] @@ pp_record type_identifier variable_identifiers
  | Enum { type_identifier; constructor_identifier }  -> GC.pp_annotate [%here] @@ pp_enum type_identifier constructor_identifier
  | Variant { type_identifier;
              constructor_identifier;
              arguments }                             -> GC.pp_annotate [%here] @@ pp_variant type_identifier constructor_identifier arguments
