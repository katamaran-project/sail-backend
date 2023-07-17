(** Sail to NanoSail translation. *)

(** Takes a sail typed ast and a name and returns the corresponding nanosail
    ast. *)
val sail_to_nanosail :
  Libsail.Type_check.tannot Libsail.Ast_defs.ast ->
  string -> Ast.ir_t
