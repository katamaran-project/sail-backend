(*
   Numeric expressions are used in types to
   specify constraints on the values a type-level variable can take on.

   For example,

     val concatenate : forall 'n 'm, (bitvector('n), bitvector('m)) -> bitvector('n + 'm)

   Here, 'n + 'm is a numeric expression.
*)
module Expression = Recursive.NumericExpression

(*
   Numeric constraints are used in types to
   specify contraints on the values a type-level variable can take on.

   For example,

     val indexate : forall 'n 'k, 0 <= 'k & 'k < 'n, (bitvector('n), int('k)) -> bit

   Here, 0 <= 'k & 'k < 'n is a numeric constraint.
*)
module Constraint = Recursive.NumericConstraint


(*
   A substitution for substitute_numeric_expression_identifier (of types, expressions, statements)
   that replaces each size variable in values by its value. The substitution sees both Var and Id;
   every other identifier is kept: as Var if it is a size variable (kid__n), as Id otherwise.
*)
let size_variable_substitution (values : (Identifier.t * Z.t) list) (identifier : Identifier.t) : Expression.t =
  match Base.List.Assoc.find values ~equal:Identifier.equal identifier with
  | Some value -> Expression.Constant value
  | None       -> begin
      if Base.String.is_prefix (Identifier.to_string identifier) ~prefix:"kid__"
      then Expression.Var identifier
      else Expression.Id identifier
    end
