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
