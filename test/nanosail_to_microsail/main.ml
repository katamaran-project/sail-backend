open Nanosail.Pretty_printing_katamaran
open Examples


let usage_msg = "main.exe <options> <input>"
let width = ref 80
let input_name = ref ""


let rec options = [
  ("-w", Arg.Set_int width,
    " Set the width of the generated lines (80 by default)");
  ("-list_notations", Arg.Set opt_list_notations, " Use list notations");
  ("-inputs", Arg.Unit inputs, " Print the list of available inputs");
  ("-help", Arg.Unit help,
    " Display this list of options. Also available as -h or --help");
  ("-h", Arg.Unit help, "");
  ("--help", Arg.Unit help, "");
]

and inputs () = raise (Arg.Help (
  String.concat "\n" ir_names ^ "\n"
))

and help () = raise (Arg.Help (
  Arg.usage_string (Arg.align options) usage_msg
))

let anon_fun input = input_name := input


(******************************************************************************)
(* Main *)

let () =
  Arg.parse (Arg.align options) anon_fun usage_msg;
  let ir = match find_ir_opt !input_name with
    | Some ir -> ir
    | _       -> raise (Arg.Bad "Unknown input")
  in pretty_print !width Out_channel.stdout (fromIR_pp ir)