open ExtBase


module Substitute = struct
  let in_numeric_expression
      (substitute         : Ast.Identifier.t -> Ast.Numeric.Expression.t)
      (numeric_expression : Ast.Numeric.Expression.t                    ) : Ast.Numeric.Expression.t
    =
    let rec subst (numeric_expression : Ast.Numeric.Expression.t) : Ast.Numeric.Expression.t =
      match (numeric_expression : Ast.Numeric.Expression.t) with
      | Constant _ -> numeric_expression
      | BinaryOperation (operator, left_operand, right_operand) -> begin
          BinaryOperation (
            operator,
            subst left_operand,
            subst right_operand
          )
        end
      | Neg numeric_expression -> Neg (subst numeric_expression)
      | PowerOf2 numeric_expression -> PowerOf2 (subst numeric_expression)
      | Id identifier -> substitute identifier
      | Var identifier -> substitute identifier
    in
    subst numeric_expression
end
