val generate          : (Sail.sail_definition * Ast.register_definition) list -> PPrint.document
val regnames          : (Sail.sail_definition * Ast.register_definition) list -> PPrint.document option
val translate_regname : Ast.Identifier.t -> Ast.Identifier.t
