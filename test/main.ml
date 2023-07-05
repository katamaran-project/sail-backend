open Nanosail.Pretty_printing_katamaran
open Examples

let usage_msg = "main.exe <options> <input>"
let width = ref 80
let input_name = ref "all"

let help options = raise (Arg.Help
  (Arg.usage_string (Arg.align options) usage_msg))

let rec options = [
  ("-w", Arg.Set_int width, " Set the width of the generated lines");
  ("--list-notations", Arg.Set list_notations, " Use list notations");
  ("-help", Arg.Unit (fun () -> help options), "");
  ("-h", Arg.Unit (fun () -> help options), "");
  ("--help", Arg.Unit (fun () -> help options),
    " Display this list of options. Also available as -h or --help");
]

let anon_fun input = input_name := input


(******************************************************************************)
(* Main *)

let () =
  Arg.parse (Arg.align options) anon_fun usage_msg;
  let ir = match !input_name with 
      | "lists" -> ListsIR.ir
      | "long"  -> LongIR.ir
      | "prod"  -> ProdIR.ir
      | "expr"  -> ExprIR.ir
      | "all"   -> AllIR.ir
      | _       -> failwith "Unknown input"
  in pretty_print !width Out_channel.stdout (fromIR_pp ir)