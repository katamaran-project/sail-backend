
val opt_list_notations : bool ref

(******************************************************************************)

val fromIR_pp : Ast.ir_t -> PPrint.document

val pretty_print : int -> out_channel -> PPrint.document -> unit
