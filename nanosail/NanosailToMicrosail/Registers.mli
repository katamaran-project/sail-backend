val generate          : (Ast.sail_definition * Ast.register_definition) list -> PPrint.document
val regnames          : (Ast.sail_definition * Ast.register_definition) list -> PPrint.document option
val translate_regname : Ast.identifier -> Ast.identifier
