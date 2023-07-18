(** Takes a example test name and returns the corresponding nanosail ast *)
val find_ir : string -> Nanosail.Ast.ir_t

(** Takes a example test name and returns the corresponding nanosail ast as an
    option *)
val find_ir_opt : string -> Nanosail.Ast.ir_t option

(** List of example test names *)
val ir_names : string list
