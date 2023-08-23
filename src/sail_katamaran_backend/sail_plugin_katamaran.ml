open Libsail
open Nanosail.Sail_to_nanosail
open Nanosail.Pretty_printing_katamaran

(** Width of the output (dfault is 80) *)
let opt_width = ref 80

(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  ("-ok",
    Arg.Unit (fun () -> print_endline("ok.")) ,
    " (debug) print \"ok.\"");
  ("-list_notations",
    Arg.Set opt_list_notations,
    " use list notations");
  ("-w",
    Arg.Set_int opt_width,
    " set a custom width for the output")
]

(** List of rewrites applied to the sail ast after type checking and before
    any translation to intermediate representation language. *)
let katamaran_rewrites = [
  ("guarded_pats", []);
  ("make_cases_exhaustive", []);
  ("merge_function_clauses", []);
  ("recheck_defs", []);
]

(** Katamaran target action. *)
let katamaran_target _ out_file ast _ _ =
  let close, output_chan, prog_name = match out_file with
    | Some f -> (true, open_out (f ^ ".v"), String.capitalize_ascii f)
    | None   -> (false, stdout, "NoName")
  in let ir = sail_to_nanosail ast prog_name
  in pretty_print !opt_width output_chan
    (fromIR_pp ir);
  if close then close_out output_chan

(** Registering of the katamaran target. *)
let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target