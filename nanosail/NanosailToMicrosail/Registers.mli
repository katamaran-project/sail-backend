val generate_regdeclkit               : (Sail.sail_definition * Ast.Definition.register_definition) list -> PPrint.document
val regname_inductive_type            : (Sail.sail_definition * Ast.Definition.register_definition) list -> PPrint.document
val translate_regname                 : Ast.Identifier.t -> Ast.Identifier.t
val required_no_confusions            : (Sail.sail_definition * Ast.Definition.register_definition) list -> Ast.Identifier.t list
val required_eqdecs                   : (Sail.sail_definition * Ast.Definition.register_definition) list -> Ast.Identifier.t list
val regname_tag                       : Ast.Identifier.t
val regname_inductive_type_identifier : Ast.Identifier.t
val generate_register_finiteness      : (Sail.sail_definition * Ast.Definition.register_definition) list -> PP.document
