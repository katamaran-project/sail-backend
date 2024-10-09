open Base


type t =
  | Variable        of Identifier.t
  | Val             of Value.t
  | List            of t list
  | UnaryOperation  of UnaryOperator.t * t
  | BinaryOperation of BinaryOperator.t * t * t
  | Record          of { type_identifier        : Identifier.t;
                         variable_identifiers   : Identifier.t list }
  | Enum            of { type_identifier        : Identifier.t;
                         constructor_identifier : Identifier.t }
  | Variant         of { type_identifier        : Identifier.t;
                         constructor_identifier : Identifier.t;
                         argument_identifiers   : Identifier.t list }


let rec to_fexpr (expression : t) : FExpr.t =
  let variable_to_fexpr (identifier : Identifier.t) : FExpr.t =
    FExpr.mk_application ~positional:[Identifier.to_fexpr identifier] "Var"

  and value_to_fexpr (value : Value.t) : FExpr.t =
    FExpr.mk_application ~positional:[Value.to_fexpr value] "Val"

  and list_to_fexpr (values : t list) : FExpr.t =
    FExpr.mk_application ~positional:(List.map ~f:to_fexpr values) "List"

  and unary_operation_to_fexpr
      (operator : UnaryOperator.t)
      (operand  : t              ) : FExpr.t
    =
    FExpr.mk_application ~positional:[UnaryOperator.to_fexpr operator; to_fexpr operand] "UnaryOp"

  and binary_operation_to_fexpr
      (operator      : BinaryOperator.t)
      (left_operand  : t               )
      (right_operand : t               ) : FExpr.t
    =
    FExpr.mk_application ~positional:[BinaryOperator.to_fexpr operator; to_fexpr left_operand; to_fexpr right_operand] "BinaryOp"

  and record_to_fexpr
      (type_identifier : Identifier.t     )
      (variables       : Identifier.t list) : FExpr.t
    =
    let keyword =
      [
        ("record_type", Identifier.to_fexpr type_identifier);
        ("variables", FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr variables)
      ]
    in
    FExpr.mk_application ~keyword "Record"

  and enum_to_fexpr
      (type_identifier        : Identifier.t)
      (constructor_identifier : Identifier.t) : FExpr.t
    =
    FExpr.mk_application ~positional:[Identifier.to_fexpr type_identifier; Identifier.to_fexpr constructor_identifier] "Enum"

  and variant_to_fexpr
      (type_identifier        : Identifier.t     )
      (constructor_identifier : Identifier.t     )
      (argument_identifiers   : Identifier.t list) : FExpr.t
    =
    let keyword =
      [
        ("type"       , Identifier.to_fexpr type_identifier                                  );
        ("constructor", Identifier.to_fexpr constructor_identifier                           );
        ("arguments"  , FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr argument_identifiers);
      ]
    in
    FExpr.mk_application ~keyword "Union"

  in
  match expression with
   | Variable identifier                                     -> variable_to_fexpr identifier
   | Val value                                               -> value_to_fexpr value
   | List items                                              -> list_to_fexpr items
   | UnaryOperation (operator, operand)                      -> unary_operation_to_fexpr operator operand
   | BinaryOperation (operator, left_operand, right_operand) -> binary_operation_to_fexpr operator left_operand right_operand
   | Record { type_identifier;
              variable_identifiers }                         -> record_to_fexpr type_identifier variable_identifiers
   | Enum { type_identifier;
            constructor_identifier }                         -> enum_to_fexpr type_identifier constructor_identifier
   | Variant { type_identifier;
               constructor_identifier;
               argument_identifiers }                        -> variant_to_fexpr type_identifier constructor_identifier argument_identifiers
