open! ExtBase


type t =
  | Variable        of Identifier.t * Nanotype.t
  | Value           of Value.t
  | List            of t list
  | UnaryOperation  of UnaryOperator.t * t
  | BinaryOperation of BinaryOperator.t * t * t
  | Record          of { type_identifier        : Identifier.t;
                         fields                 : Identifier.t list }
  | Enum            of { type_identifier        : Identifier.t;
                         constructor_identifier : Identifier.t }
  | Variant         of { type_identifier        : Identifier.t;
                         constructor_identifier : Identifier.t;
                         fields                 : t list }
  | Tuple           of t list
  | Bitvector       of t list



exception UnimplementedExpressionEquality


(* Still incomplete, raises UnimplementedExpressionEquality in unimplemented cases, todo complete this *)
let rec equal
    (expression_1 : t)
    (expression_2 : t) : bool
  =
  match expression_1, expression_2 with
  | Variable (identifier_1, type_1), Variable (identifier_2, type_2) -> begin
      Identifier.equal
        identifier_1
        identifier_2
      &&
      Nanotype.equal
        type_1
        type_2
    end
  | List subexpressions_1, List subexpressions_2 -> begin
      List.equal equal
        subexpressions_1
        subexpressions_2
    end
  | Tuple subexpressions_1, Tuple subexpressions_2 -> begin
      List.equal equal
        subexpressions_1
        subexpressions_2
    end
  | Value value_1, Value value_2 -> begin
      Value.equal
        value_1
        value_2
    end
  | UnaryOperation (operator_1, operand_1), UnaryOperation (operator_2, operand_2) -> begin
      UnaryOperator.equal
        operator_1
        operator_2
      &&
      equal
        operand_1
        operand_2
    end
  | BinaryOperation (operator_1, left_operand_1, right_operand_1), BinaryOperation (operator_2, left_operand_2, right_operand_2) -> begin
      BinaryOperator.equal
        operator_1
        operator_2
      &&
      equal
        left_operand_1
        left_operand_2
      &&
      equal
        right_operand_1
        right_operand_2
    end
  | Record _, Record _                                   -> raise UnimplementedExpressionEquality
  | Enum _, Enum _                                       -> raise UnimplementedExpressionEquality
  | Variant _, Variant _                                 -> raise UnimplementedExpressionEquality
  | Bitvector _, Bitvector _                             -> raise UnimplementedExpressionEquality
  | Variable _, _                                        -> false
  | Value _, _                                           -> false
  | List _, _                                            -> false
  | UnaryOperation _, _                                  -> false
  | BinaryOperation (_, _, _), _                         -> false
  | Record _, _                                          -> false
  | Enum _, _                                            -> false
  | Variant _, _                                         -> false
  | Tuple _, _                                           -> false
  | Bitvector _, _                                       -> false


exception UnimplementedTypeInference

(* Still incomplete, raises UnimplementedTypeInference in unimplemented cases, todo complete this *)
let infer_type (expression : t) : Nanotype.t =
  match expression with
   | Variable (_, typ)         -> typ
   | Value _                   -> raise UnimplementedTypeInference
   | List _                    -> raise UnimplementedTypeInference
   | UnaryOperation (_, _)     -> raise UnimplementedTypeInference
   | BinaryOperation (_, _, _) -> raise UnimplementedTypeInference
   | Record _                  -> raise UnimplementedTypeInference
   | Enum _                    -> raise UnimplementedTypeInference
   | Variant _                 -> raise UnimplementedTypeInference
   | Tuple _                   -> raise UnimplementedTypeInference
   | Bitvector bits            -> Nanotype.Bitvector (Numeric.Expression.Constant (Z.of_int @@ List.length bits))


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
   | Value value                                             -> value_to_fexpr value
   | List items                                              -> list_to_fexpr items
   | UnaryOperation (operator, operand)                      -> unary_operation_to_fexpr operator operand
   | BinaryOperation (operator, left_operand, right_operand) -> binary_operation_to_fexpr operator left_operand right_operand
   | Tuple elements                                          -> tuple_to_fexpr elements
   | Bitvector elements                                      -> bitvector_to_fexpr elements
   | Record { type_identifier;
              fields }                                       -> record_to_fexpr type_identifier fields
   | Enum { type_identifier;
            constructor_identifier }                         -> enum_to_fexpr type_identifier constructor_identifier
   | Variant { type_identifier;
               constructor_identifier;
               fields    }                                   -> variant_to_fexpr type_identifier constructor_identifier fields


let rec free_variables (expression : t) : Identifier.Set.t =
  match expression with
   | Variable (identifier, _)                                            -> Identifier.Set.singleton identifier
   | Value _                                                             -> Identifier.Set.empty
   | List elements                                                       -> Identifier.Set.unions @@ List.map ~f:free_variables elements
   | UnaryOperation (_, operand)                                         -> free_variables operand
   | BinaryOperation (_, left_operand, right_operand)                    -> Identifier.Set.union (free_variables left_operand) (free_variables right_operand)
   | Record { type_identifier = _; fields }                              -> Identifier.Set.of_list fields
   | Enum _                                                              -> Identifier.Set.empty
   | Variant { type_identifier = _; constructor_identifier = _; fields } -> Identifier.Set.unions @@ List.map ~f:free_variables fields
   | Tuple elements                                                      -> Identifier.Set.unions @@ List.map ~f:free_variables elements
   | Bitvector elements                                                  -> Identifier.Set.unions @@ List.map ~f:free_variables elements


let substitute_numeric_expression_identifier
    (substitution : Identifier.t -> Numeric.Expression.t)
    (expression   : t                                   ) : t
  =
  let typsubst = Nanotype.substitute_numeric_expression_identifier substitution
  in
  let rec subst (expression : t) : t =
    match expression with
    | Variable (identifier, typ)                                  -> Variable (identifier, typsubst typ)
    | Value _                                                     -> expression
    | List elements                                               -> List (List.map ~f:subst elements)
    | UnaryOperation (operator, operand)                          -> UnaryOperation (operator, subst operand)
    | BinaryOperation (operator, left_operand, right_operand)     -> BinaryOperation (operator, left_operand, right_operand)
    | Record _                                                    -> expression
    | Enum _                                                      -> expression
    | Tuple elements                                              -> Tuple (List.map ~f:subst elements)
    | Bitvector elements                                          -> Bitvector (List.map ~f:subst elements)
    | Variant { type_identifier; constructor_identifier; fields } -> begin
        Variant {
          type_identifier;
          constructor_identifier;
          fields = List.map ~f:subst fields
        }
      end
  in
  subst expression
