val generate_regdeclkit    : (Sail.sail_definition * Ast.Definition.register_definition) list -> PPrint.document
val regname_inductive_type : (Sail.sail_definition * Ast.Definition.register_definition) list -> PPrint.document option
val translate_regname      : Ast.Identifier.t -> Ast.Identifier.t
val generate_noconfusions  : (Sail.sail_definition * Ast.Definition.register_definition) list -> PPrint.document option
val required_eqdecs        : (Sail.sail_definition * Ast.Definition.register_definition) list -> Ast.Identifier.t list
