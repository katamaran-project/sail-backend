type t =
  | Var    of Identifier.t
  | Val    of Value.t
  | Neg    of t
  | Not    of t
  | List   of t list
  | Binop  of BinaryOperator.t * t * t
  | Record of { type_identifier : Identifier.t; variable_identifiers : Identifier.t list }
  | Enum   of Identifier.t
