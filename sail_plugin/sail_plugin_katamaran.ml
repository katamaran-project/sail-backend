open Base


let print_check_message () =
  Stdio.print_endline("The Katamaran plugin is functioning correctly")

(*
   By default, use all rewrites listed in Rewrites.katamaran_rewrites.
   Setting the environment variable REWRITES allow to only enable
   a prefix of these rewrites.
*)
let rewrites () =
  let rewrite_count =
    match Sys.getenv "REWRITES" with
    | None     -> List.length Rewrites.katamaran_rewrites
    | Some str -> Int.of_string str
  in
  List.take Rewrites.katamaran_rewrites rewrite_count


module CLI = struct
  module Arg = struct
    let mk_option s =
      let words = String.split s ~on:' '
      in
      String.concat ~sep:"-" ("-katamaran" :: words)

    let check       = mk_option "check"
    let config_file = mk_option "config"
  end
end


(*
   Command line options added to sail when the sail_katamaran_backend is loaded or installed.
*)
let katamaran_options = [
  (CLI.Arg.check,
    Stdlib.Arg.Unit print_check_message,
    "(debug) check if Katamaran plugin is correctly installed");
  (CLI.Arg.config_file,
   Stdlib.Arg.String (fun s -> Nanosail.Configuration.load_configuration s),
   "Specify configuration file");
]


(*
   Entry point for Katamaran target
*)
let katamaran_target
      (_     : string option                   )
      (state : Libsail.Interactive.State.istate)
  =
  Stdio.print_endline "Starting translation from Sail to muSail";
  let ast = state.ast
  in
  let translation = Nanosail.SailToNanosail.translate ast
  in
  Nanosail.Templates.process translation;
  Stdio.print_endline "Done with translation"


(*
   Tell Sail about new Katamaran target
*)
let _ =
  Libsail.Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:(rewrites ())
    katamaran_target
