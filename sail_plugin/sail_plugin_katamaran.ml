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

    let check       = mk_option "check"
    let config_file = mk_option "config"
    let verbose     = mk_option "verbose"
  end
end


let set_verbosity_level (level : int) =
  Nanosail.Configuration.(set Nanosail.Logging.verbosity_level @@ Nanosail.Logging.VerbosityLevel.from_int level)


(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  (CLI.Arg.check,
    Stdlib.Arg.Unit print_check_message,
    "(debug) check if Katamaran plugin is correctly installed");
  (CLI.Arg.config_file,
   Stdlib.Arg.String (fun s -> Nanosail.Configuration.load_configuration s),
   "Specify configuration file");
  (CLI.Arg.verbose,
   Stdlib.Arg.Int set_verbosity_level,
   "Verbose");
]


let configure_verbosity () =
  let verbosity_from_environment_variable () =
    match Sys.getenv "VERBOSE" with
    | Some level -> set_verbosity_level @@ Int.of_string level
    | None       -> ()
  in
  verbosity_from_environment_variable ()


(* Entry point for Katamaran target *)
let katamaran_target
      (_     : string option                   )
      (state : Libsail.Interactive.State.istate)
  =
  Stdio.print_endline "Starting translation from Sail to muSail";
  configure_verbosity ();
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
