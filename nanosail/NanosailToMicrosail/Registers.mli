val generate               : (Sail.sail_definition * Ast.register_definition) list -> PPrint.document
val regname_inductive_type : (Sail.sail_definition * Ast.register_definition) list -> PPrint.document option
val translate_regname      : Ast.Identifier.t -> Ast.Identifier.t
val generate_noconfusions  : (Sail.sail_definition * Ast.register_definition) list -> PPrint.document option
