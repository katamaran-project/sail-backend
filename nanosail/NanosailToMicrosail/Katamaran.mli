val pretty_print               : Ast.program -> PP.document GenerationContext.t
val output_document_to_channel : PP.requirement -> out_channel -> PP.document GenerationContext.t -> unit
val full_translation           : Ast.program -> PP.document
