(** Pretty printing from a NanoSail ast to a µSail Coq file for Katamaran *)

(** Set to true for better list notations *)
val opt_list_notations : bool ref

(******************************************************************************)

(** Takes a nanosail AST and returns the corresponding µSail PPrint.document. *)
val fromIR_pp : Ast.ir_t -> PPrint.document

(** Takes an width, an output channel, and a PPrint.document and prints the
    document to the output channel with the given width. *)
val pretty_print : int -> out_channel -> PPrint.document -> unit
