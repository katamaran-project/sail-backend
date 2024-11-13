open Base


let print_check_message () =
  Stdio.print_endline("The Katamaran plugin is functioning correctly")


let rewrite_count () =
  match Sys.getenv "REWRITES" with
  | None     -> List.length Rewrites.katamaran_rewrites
  | Some str -> Int.of_string str


let rewrites () =
  List.take Rewrites.katamaran_rewrites (rewrite_count ())


module CLI = struct
  module Arg = struct
    let mk_option s =
      let words = String.split s ~on:' '
      in
      String.concat ~sep:"_" ("-katamaran" :: words)

    let check                = mk_option "check"
    let list_notations       = mk_option "list notations"
    let width                = mk_option "width"
    let include_original     = mk_option "add original"
    let print_rewrites       = mk_option "print rewrites"
    let config_file          = mk_option "config"
  end
end

(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  (CLI.Arg.check,
    Stdlib.Arg.Unit print_check_message,
    "(debug) check if Katamaran plugin is correctly installed");
  (CLI.Arg.list_notations,
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set use_list_notations true)),
    "use list notations");
  (CLI.Arg.width,
    Stdlib.Arg.Set_int Options.width,
    "set a custom width for the output");
  (CLI.Arg.include_original,
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_original_code true)),
   "show original Sail code in output");
  (CLI.Arg.config_file,
   Stdlib.Arg.String (fun s -> Nanosail.Configuration.load_configuration s),
   "Specify configuration file");
]


(* Entry point for Katamaran target *)
let katamaran_target
      (_     : string option                   )
      (state : Libsail.Interactive.State.istate)
  =
  let ast = state.ast
  in
  let translation = Nanosail.SailToNanosail.translate ast
  in
  Nanosail.Templates.process translation


(* Tell Sail about new Katamaran target *)
let _ =
  Libsail.Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:(rewrites ())
    katamaran_target
