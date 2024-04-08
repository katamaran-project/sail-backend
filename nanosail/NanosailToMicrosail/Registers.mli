val generate : (Ast.sail_definition * Ast.register_definition) list -> PPrint.document
val regnames : ?prefix : string -> (Ast.sail_definition * Ast.register_definition) list -> PPrint.document option
