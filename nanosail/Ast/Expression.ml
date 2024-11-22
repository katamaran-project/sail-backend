open Base


type t =
  | Variable        of Identifier.t * Nanotype.t
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
                         fields                 : t list }
  | Tuple           of t list
  | Bitvector       of t list


exception MissingTypeInference


let infer_type (expression : t) : Nanotype.t =
  match expression with
   | Variable (_, typ)         -> typ
   | Val _                     -> raise MissingTypeInference
   | List _                    -> raise MissingTypeInference
   | UnaryOperation (_, _)     -> raise MissingTypeInference
   | BinaryOperation (_, _, _) -> raise MissingTypeInference
   | Record _                  -> raise MissingTypeInference
   | Enum _                    -> raise MissingTypeInference
   | Variant _                 -> raise MissingTypeInference
   | Tuple _                   -> raise MissingTypeInference
   | Bitvector bits            -> Ast.Type.Bitvector (Ast.Numeric.Expression.Constant (Z.of_int @@ List.length bits))
     

let rec to_fexpr (expression : t) : FExpr.t =
  let variable_to_fexpr
      (identifier : Identifier.t)
      (typ        : Nanotype.t  ) : FExpr.t
    =
    FExpr.mk_application ~positional:[Identifier.to_fexpr identifier; Nanotype.to_fexpr typ] "Var"

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
      (type_identifier        : Identifier.t)
      (constructor_identifier : Identifier.t)
      (arguments              : t list      ) : FExpr.t
    =
    let keyword =
      [
        ("type"       , Identifier.to_fexpr type_identifier            );
        ("constructor", Identifier.to_fexpr constructor_identifier     );
        ("arguments"  , FExpr.mk_list @@ List.map ~f:to_fexpr arguments);
      ]
    in
    FExpr.mk_application ~keyword "Union"

  and tuple_to_fexpr (elements : t list) : FExpr.t =
    let positional =
      List.map ~f:to_fexpr elements
    in
    FExpr.mk_application ~positional "Tuple"

  and bitvector_to_fexpr (elements : t list) : FExpr.t =
    let positional =
      List.map ~f:to_fexpr elements
    in
    FExpr.mk_application ~positional "Bitvector"

  in
  match expression with
   | Variable (identifier, typ)                              -> variable_to_fexpr identifier typ
   | Val value                                               -> value_to_fexpr value
   | List items                                              -> list_to_fexpr items
   | UnaryOperation (operator, operand)                      -> unary_operation_to_fexpr operator operand
   | BinaryOperation (operator, left_operand, right_operand) -> binary_operation_to_fexpr operator left_operand right_operand
   | Tuple elements                                          -> tuple_to_fexpr elements
   | Bitvector elements                                      -> bitvector_to_fexpr elements
   | Record { type_identifier;
              variable_identifiers }                         -> record_to_fexpr type_identifier variable_identifiers
   | Enum { type_identifier;
            constructor_identifier }                         -> enum_to_fexpr type_identifier constructor_identifier
   | Variant { type_identifier;
               constructor_identifier;
               fields    }                                   -> variant_to_fexpr type_identifier constructor_identifier fields
