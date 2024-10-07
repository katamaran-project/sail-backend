open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_infix_binary_operation (binary_operator : Ast.BinaryOperator.t) : PP.document GC.t =
  match binary_operator with
  | Plus                 -> GC.return Coq.Operator.addition
  | Minus                -> GC.return Coq.Operator.subtraction
  | Times                -> GC.return Coq.Operator.multiplication
  | And                  -> GC.return Coq.Operator.conjunction
  | Or                   -> GC.return Coq.Operator.disjunction
  | EqualTo              -> GC.return Coq.Operator.equality
  | NotEqualTo           -> GC.return Coq.Operator.inequality
  | LessThan             -> GC.return Coq.Operator.less_than
  | GreaterThan          -> GC.return Coq.Operator.greater_than
  | LessThanOrEqualTo    -> GC.return Coq.Operator.less_than_or_equal_to
  | GreaterThanOrEqualTo -> GC.return Coq.Operator.greater_than_or_equals_to
  | Pair                 -> GC.not_yet_implemented [%here] (* Should not occur *) (* use fail *)
  | Cons                 -> GC.not_yet_implemented [%here] (* Should not occur *)
  | Append               -> GC.not_yet_implemented [%here] (* Should not occur *)


let rec pp_expression (expression : Ast.Expression.t) : PP.document GC.t =
  let rec pp_value (value : Ast.Value.t) : PP.document GC.t =
    match value with
    | Bool true        -> GC.return @@ PP.string "exp_true"
    | Bool false       -> GC.return @@ PP.string "exp_false"
    | Int n            -> GC.return @@ Coq.pp_application (PP.string "exp_int") [ Coq.pp_integer n]
    | String s         -> GC.return @@ Coq.pp_application (PP.string "exp_string") [ PP.(surround dquotes @@ string s) ]
    | Unit             -> GC.return @@ Coq.pp_application (PP.string "exp_val") [ PP.string "ty.unit"; PP.string "tt"]
    | Prod (_, _) as v -> begin
        let* pp_tuple_type = Nanotype.pp_nanotype (Ast.Value.type_of_value v)
        in
        let* pp_value' = pp_value v
        in
        GC.return @@ Coq.pp_application
                       (PP.string "exp_val")
                       [
                         pp_tuple_type;
                         pp_value'
                       ]
      end

  and pp_binary_operation
      (binary_operator : Ast.BinaryOperator.t)
      (left_operand    : Ast.Expression.t    )
      (right_operand   : Ast.Expression.t    ) : PP.document GC.t
    =
    let* pp_left_operand  = GC.lift ~f:PP.(surround parens) @@ pp_expression left_operand
    and* pp_right_operand = GC.lift ~f:PP.(surround parens) @@ pp_expression right_operand
    in
    let pp id =
      GC.return @@ Coq.pp_application (PP.string "exp_binop") [
                       PP.string "exp_binop";
                       PP.string id;
                       pp_left_operand;
                       pp_right_operand
                     ]
    in
    match binary_operator with
    | Pair   -> pp "bop.pair"
    | Cons   -> pp "bop.cons"
    | Append -> pp "bop.append"
    | _      -> begin
        let* binop' = pp_infix_binary_operation binary_operator
        in
        GC.return begin
          PP.horizontal [
            PP.(surround parens) pp_left_operand;
            binop';
            PP.(surround parens) pp_right_operand;
          ]
        end
      end

  and pp_variable (identifier : Ast.Identifier.t) : PP.document GC.t =
    GC.return @@ PPSail.pp_expression_of_identifier @@ Identifier.pp identifier

  and pp_unary_operation
      (operator : Ast.UnaryOperator.t)
      (operand  : Ast.Expression.t   ) : PP.document GC.t
    =
    match operator with
    | Ast.UnaryOperator.Neg -> pp_negation operand
    | Ast.UnaryOperator.Not -> pp_logical_negation operand

  and pp_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand = GC.lift ~f:PP.(surround parens) @@ pp_expression operand
    in
    GC.return @@ PP.(separate_horizontally ~separator:space [minus; pp_operand])

  and pp_logical_negation (operand : Ast.Expression.t) : PP.document GC.t =
    let* pp_operand = GC.lift ~f:PP.(surround parens) @@ pp_expression operand
    in
    GC.return @@ Coq.pp_application (PP.string "exp_not") [ pp_operand ]

  and pp_list (elements : Ast.Expression.t list) : PP.document GC.t =
    let* pp_elements =
      GC.map ~f:pp_expression elements
    in
    let pp_list =
      Coq.pp_list ~use_notation:Configuration.(get use_list_notations) pp_elements
    in
    GC.return @@ Coq.pp_application (PP.string "exp_list") [ pp_list ]

  and pp_record
      (type_identifier      : Ast.Identifier.t     )
      (variable_identifiers : Ast.Identifier.t list) : PP.document GC.t
    =
    GC.return @@ Coq.pp_application (PP.string "exp_record") [
                     Identifier.pp @@ Configuration.reified_record_name type_identifier;
                     Coq.pp_list_using_notation begin
                         List.map
                           variable_identifiers
                           ~f:(fun id -> Coq.pp_application (PP.string "exp_var") [ PP.(surround dquotes) @@ Identifier.pp id ])
                       end
                   ]

  in
  match expression with
  | Variable identifier                              -> pp_variable identifier
  | Val value                                        -> pp_value value
  | UnaryOperation (operator, operand)               -> pp_unary_operation operator operand
  | BinaryOperation (op, e1, e2)                     -> pp_binary_operation op e1 e2
  | List elements                                    -> pp_list elements
  | Record { type_identifier; variable_identifiers } -> pp_record type_identifier variable_identifiers
  | Enum args -> begin
      let enum_type =
        Coq.pp_application
          (PP.string "ty.enum")
          [ Identifier.pp @@ Configuration.reified_enum_name args.type_identifier ]
      and enum_constructor =
        Identifier.pp args.constructor_identifier
      in
      GC.return @@ PPSail.pp_expression_value enum_type enum_constructor
    end
