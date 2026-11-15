val generate : Ast.Definition.Function.t list -> PP.t GenerationContext.t
val signature : Ast.Definition.Function.t -> TypeLevelParameters.signature
val type_level_parameters : Ast.Definition.Function.t -> (Ast.Identifier.t * Ast.Kind.t) list
