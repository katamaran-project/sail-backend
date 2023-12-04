val opt_list_notations : bool ref

val opt_include_untranslated : bool ref

val fromIR_pp : Ast.ir_t -> PPrint.document

val pretty_print : PPrint.requirement -> out_channel -> PPrint.document -> unit
