val rename_in_statement  : (Identifier.t -> Identifier.t) -> Statement.t -> Statement.t
val rename_in_expression : (Identifier.t -> Identifier.t) -> Expression.t -> Expression.t
val create_renamer       : Identifier.t -> Identifier.t -> Identifier.t -> Identifier.t
