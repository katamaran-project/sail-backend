val pp_regdeclkit                     : (Sail.sail_definition * Ast.Definition.Register.t) list -> PP.t GenerationContext.t
val pp_regname_inductive_type         : (Sail.sail_definition * Ast.Definition.Register.t) list -> PP.t GenerationContext.t
val translate_regname                 : Ast.Identifier.t -> Ast.Identifier.t
val regname_tag                       : Ast.Identifier.t
val regname_inductive_type_identifier : Ast.Identifier.t
val pp_register_finiteness            : (Sail.sail_definition * Ast.Definition.Register.t) list -> PP.t GenerationContext.t
val extra_eqdec_identifiers           :  unit -> Ast.Identifier.t list
val extra_no_confusion_identifiers    :  unit -> Ast.Identifier.t list
