val pp_function_definition_kit : (Sail.sail_definition * Ast.Definition.Function.t) list -> (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list -> PP.t GenerationContext.t

(* The bodies (Definition fun_f), one document per given function (its monomorphs for a polymorphic function) *)
val pp_function_definitions : (Sail.sail_definition * Ast.Definition.Function.t) list -> (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list -> PP.t list GenerationContext.t

(* FunDef; qualifier gives the Rocq library that defines a body, if it is not the current one *)
val pp_fundef_inductive_type : ?qualifier:(Ast.Identifier.t -> string option) -> Ast.Definition.Function.t list -> PP.t GenerationContext.t
