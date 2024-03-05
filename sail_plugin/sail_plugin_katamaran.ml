open Base


(** Width of the output (default is 100) *)
let print_check_message () =
  Stdio.print_endline("Katamaran plugin is functioning correctly")


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
    let program_name         = mk_option "program name"
    let include_original     = mk_option "add original"
    let include_untranslated = mk_option "include untranslated"
    let include_ignored      = mk_option "include ignored"
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
  (CLI.Arg.program_name,
   Stdlib.Arg.Set_string Options.program_name,
    "set a custom width for the output");
  (CLI.Arg.include_original,
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_original_code true)),
   "show original Sail code in output");
  (CLI.Arg.include_untranslated,
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_untranslated_definitions true)),
   "include information about untranslated Sail code");
  (CLI.Arg.include_ignored,
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_ignored_definitions true)),
   "include ignored Sail definitions");
  (CLI.Arg.print_rewrites,
   Stdlib.Arg.Set Options.print_rewrites,
   "Prints the list of rewrites");
  (CLI.Arg.config_file,
   Stdlib.Arg.String (fun s -> Nanosail.Configuration.load_configuration_file s),
   "Specify configuration file");
]


(* let with_open_file filename func = *)
(*   Auxlib.using *)
(*     ~resource:(Stdio.Out_channel.create filename) *)
(*     ~close:Stdio.Out_channel.close *)
(*     ~body:func *)


(* let with_stdout func = *)
(*   func Stdio.stdout *)

let print_rewrites () =
  List.iteri ~f:(fun index (rewrite_name, _) ->
      Stdio.printf "[%02d] %s\n" (index + 1) rewrite_name
    )
    (rewrites ())


(* Entry point for Katamaran target *)
let katamaran_target
      (_                : Yojson.Basic.t option                         )
      (_                : string                                        )
      (_output_filename : string option                                 )
      (ast              : Libsail.Type_check.tannot Libsail.Ast_defs.ast)
      (_                : Libsail.Effects.side_effect_info              )
      (_                : Libsail.Type_check.env                        )
  =
  if !Options.print_rewrites then print_rewrites ();
  let translation = Nanosail.SailToNanosail.translate ast !Options.program_name
  in
  Nanosail.Templates.process translation


(* Tell Sail about new Katamaran target *)
let _ =
  Libsail.Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:(rewrites ())
    katamaran_target
