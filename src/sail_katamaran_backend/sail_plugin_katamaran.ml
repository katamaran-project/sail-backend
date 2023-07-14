open Libsail
open Nanosail.Sail_to_nanosail
open Nanosail.Pretty_printing_katamaran

let katamaran_options = [
  ("-ok",
    Arg.Unit (fun () -> print_endline("ok.")) ,
    " print \"ok.\"");
  ("-list_notations",
    Arg.Set opt_list_notations,
    " use list notations");
]

let katamaran_rewrites = 
  [
    ("guarded_pats", []);
    ("make_cases_exhaustive", []);
    ("merge_function_clauses", []);
    ("recheck_defs", []);
  ]

let katamaran_target _ out_file ast _ _ =
  let close, output_chan, prog_name = match out_file with
    | Some f -> (true, open_out (f ^ ".v"), String.capitalize_ascii f)
    | None   -> (false, stdout, "NoName")
  in let ir = ast_to_ir ast prog_name
  in pretty_print 80 output_chan
    (fromIR_pp ir);
  if close then close_out output_chan

let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target