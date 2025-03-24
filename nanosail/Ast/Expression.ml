(*

   Unlike muSail, Sail does not make the distinction
   between statemens and expressions.

   Nanosail follows the same design as muSail.

*)
open ExtBase


type t =
  | Variable        of Identifier.t * Type.t
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


let rec equal
    (expression_1 : t)
    (expression_2 : t) : bool
  =
  match expression_1, expression_2 with
  | Variable (identifier_1, type_1),
    Variable (identifier_2, type_2) -> begin
      Identifier.equal
        identifier_1
        identifier_2
      &&
      Type.equal
        type_1
        type_2
    end

  | List subexpressions_1,
    List subexpressions_2 -> begin
      List.equal equal
        subexpressions_1
        subexpressions_2
    end

  | Tuple subexpressions_1,
    Tuple subexpressions_2 -> begin
      List.equal equal
        subexpressions_1
        subexpressions_2
    end

  | Value value_1,
    Value value_2 -> begin
      Value.equal
        value_1
        value_2
    end

  | UnaryOperation (operator_1, operand_1),
    UnaryOperation (operator_2, operand_2) -> begin
      UnaryOperator.equal
        operator_1
        operator_2
      &&
      equal
        operand_1
        operand_2
    end

  | BinaryOperation (operator_1, left_operand_1, right_operand_1),
    BinaryOperation (operator_2, left_operand_2, right_operand_2) -> begin
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

  | Record { type_identifier = type_identifier_1; fields = fields_1 },
    Record { type_identifier = type_identifier_2; fields = fields_2 } -> begin
      Identifier.equal
        type_identifier_1
        type_identifier_2
      &&
      List.equal Identifier.equal
        fields_1
        fields_2
    end

  | Enum { type_identifier = type_identifier_1; constructor_identifier = constructor_identifier_1 },
    Enum { type_identifier = type_identifier_2; constructor_identifier = constructor_identifier_2 } -> begin
      Identifier.equal
        type_identifier_1
        type_identifier_2
      &&
      Identifier.equal
        constructor_identifier_1
        constructor_identifier_2
    end

  | Variant { type_identifier = type_identifier_1; constructor_identifier = constructor_identifier_1; fields = fields_1 },
    Variant { type_identifier = type_identifier_2; constructor_identifier = constructor_identifier_2; fields = fields_2 } -> begin
      Identifier.equal
        type_identifier_1
        type_identifier_2
      &&
      Identifier.equal
        constructor_identifier_1
        constructor_identifier_2
      &&
      List.equal equal
        fields_1
        fields_2
    end

  | Bitvector elements_1,
    Bitvector elements_2 -> begin
      List.equal equal
        elements_1
        elements_2
    end

  | Variable _, _                -> false
  | Value _, _                   -> false
  | List _, _                    -> false
  | UnaryOperation _, _          -> false
  | BinaryOperation (_, _, _), _ -> false
  | Record _, _                  -> false
  | Enum _, _                    -> false
  | Variant _, _                 -> false
  | Tuple _, _                   -> false
  | Bitvector _, _               -> false


exception UnimplementedTypeInference

(* Still incomplete, raises UnimplementedTypeInference in unimplemented cases, todo complete this *)
let infer_type (expression : t) : Type.t =
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
   | Bitvector bits            -> Type.Bitvector (Numeric.Expression.Constant (Z.of_int @@ List.length bits))


let rec to_fexpr (expression : t) : FExpr.t =
  let variable_to_fexpr
      (identifier : Identifier.t)
      (typ        : Type.t      ) : FExpr.t
    =
    FExpr.mk_application ~positional:[Identifier.to_fexpr identifier; Type.to_fexpr typ] "Var"

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
        ("record_type", Identifier.to_fexpr type_identifier                       );
        ("variables"  , FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr variables)
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


class virtual rewriter =
  object
      method virtual rewrite                 : t -> t
      method virtual rewrite_binary_operator : operator : BinaryOperator.t -> left_operand : t -> right_operand : t -> t
      method virtual rewrite_bitvector       : elements : t list -> t
      method virtual rewrite_enum            : type_identifier : Identifier.t -> constructor_identifier : Identifier.t -> t
      method virtual rewrite_list            : elements : t list -> t
      method virtual rewrite_record          : type_identifier : Identifier.t -> fields : Identifier.t list -> t
      method virtual rewrite_tuple           : elements : t list -> t
      method virtual rewrite_unary_operation : operator : UnaryOperator.t -> operand : t -> t
      method virtual rewrite_value           : value : Value.t -> t
      method virtual rewrite_variable        : identifier : Identifier.t -> typ : Type.t -> t
      method virtual rewrite_variant         : type_identifier : Identifier.t -> constructor_identifier : Identifier.t -> fields : t list -> t
  end


class identity_rewriter =
  object(self)
    inherit rewriter

    method rewrite (expression : t) : t =
      match expression with
      | Variable (identifier, typ)                                  -> self#rewrite_variable ~identifier ~typ
      | Value value                                                 -> self#rewrite_value ~value
      | List elements                                               -> self#rewrite_list ~elements
      | UnaryOperation (operator, operand)                          -> self#rewrite_unary_operation ~operator ~operand
      | BinaryOperation (operator, left_operand, right_operand)     -> self#rewrite_binary_operator ~operator ~left_operand ~right_operand
      | Record { type_identifier; fields }                          -> self#rewrite_record ~type_identifier ~fields
      | Enum { type_identifier; constructor_identifier }            -> self#rewrite_enum ~type_identifier ~constructor_identifier
      | Variant { type_identifier; constructor_identifier; fields } -> self#rewrite_variant ~type_identifier ~constructor_identifier ~fields
      | Tuple elements                                              -> self#rewrite_tuple ~elements
      | Bitvector elements                                          -> self#rewrite_bitvector ~elements

    method rewrite_variable
        ~(identifier : Identifier.t)
        ~(typ        : Type.t      ) : t
      =
      Variable (identifier, typ)

    method rewrite_value ~(value : Value.t) : t =
      Value value

    method rewrite_list ~(elements : t list) : t =
      List (List.map ~f:self#rewrite elements)

    method rewrite_unary_operation
        ~(operator : UnaryOperator.t)
        ~(operand  : t              ) : t
      =
      UnaryOperation (operator, self#rewrite operand)

    method rewrite_binary_operator
        ~(operator      : BinaryOperator.t)
        ~(left_operand  : t               )
        ~(right_operand : t               ) : t
      =
      BinaryOperation (operator, self#rewrite left_operand, self#rewrite right_operand)

    method rewrite_record
        ~(type_identifier : Identifier.t     )
        ~(fields          : Identifier.t list) : t
      =
      Record { type_identifier; fields }

    method rewrite_enum
        ~(type_identifier        : Identifier.t)
        ~(constructor_identifier : Identifier.t) : t
      =
      Enum { type_identifier; constructor_identifier }

    method rewrite_variant
        ~(type_identifier        : Identifier.t)
        ~(constructor_identifier : Identifier.t)
        ~(fields                 : t list      ) : t
      =
      Variant { type_identifier; constructor_identifier; fields = List.map ~f:self#rewrite fields }

    method rewrite_tuple ~(elements : t list) : t =
      Tuple (List.map ~f:self#rewrite elements)

    method rewrite_bitvector ~(elements : t list) : t =
      Bitvector (List.map ~f:self#rewrite elements)
  end


let substitute_numeric_expression_identifier
    (substitution : Identifier.t -> Numeric.Expression.t)
    (expression   : t                                   ) : t
  =
  let substitute_in_type =
    Type.substitute_numeric_expression_identifier substitution
  in
  let rewriter =
    object
      inherit identity_rewriter

      method! rewrite_variable ~identifier ~typ =
        Variable (identifier, substitute_in_type typ)
    end
  in
  rewriter#rewrite expression


let simplify (expression : t) : t =
  let rewriter =
    object
      inherit identity_rewriter

      method! rewrite_variable ~identifier ~typ =
        Variable (identifier, Type.simplify typ)
    end
  in
  rewriter#rewrite expression
