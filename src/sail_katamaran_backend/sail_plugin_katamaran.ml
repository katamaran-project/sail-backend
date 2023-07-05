open Libsail
open Nanosail

let katamaran_options = [
  ( "-katamaran_ok", Arg.Unit(fun () -> print_endline("ok.")) , " print \"ok.\"")
]

let katamaran_rewrites = 
  [
    ("remove_blocks", []);
    ("remove_superfluous_letbinds", []);
    ("remove_superfluous_returns", [])
  ]

let katamaran_target _ _ out_file ast _ _ =
  let close, output_chan = match out_file with
    | Some f -> (true, open_out (f ^ ".v"))
    | None   -> (false, stdout)
  in Pretty_printing_katamaran.pretty_print 80 output_chan
      (Pretty_printing_katamaran.fromIR_pp (Sail_to_nanosail.ast_to_ir ast));
  if close then close_out output_chan

let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target