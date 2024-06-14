type t =
  | Exp_var    of Identifier.t
  | Exp_val    of Value.t
  | Exp_neg    of t
  | Exp_not    of t
  | Exp_list   of t list
  | Exp_binop  of BinaryOperator.t * t * t
  | Exp_record of { type_identifier : Identifier.t; variable_identifiers : Identifier.t list }
  | Exp_enum   of Identifier.t
