val opt_list_notations : bool ref

val fromIR_pp : ?show_untranslated:bool -> Ast.ir_t -> PPrint.document

val pretty_print : PPrint.requirement -> out_channel -> PPrint.document -> unit
