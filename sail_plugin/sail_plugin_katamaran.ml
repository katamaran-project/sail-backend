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


(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  ("-katamaran_check",
    Stdlib.Arg.Unit print_check_message,
    "(debug) check if Katamaran plugin is correctly installed");
  ("-katamaran_list_notations",
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set use_list_notations true)),
    "use list notations");
  ("-katamaran_width",
    Stdlib.Arg.Set_int Options.width,
    "set a custom width for the output");
  ("-katamaran_add_original",
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_original_code true)),
   "show original Sail code in output");
  ("-katamaran_include_untranslated",
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_untranslated_definitions true)),
   "include information about untranslated Sail code");
  ("-katamaran_include_ignored",
   Stdlib.Arg.Unit (fun () -> Nanosail.Configuration.(set include_ignored_definitions true)),
   "include ignored Sail definitions");
  ("-katamaran_print_rewrites",
   Stdlib.Arg.Set Options.print_rewrites,
   "Prints the list of rewrites");
  ("-katamaran_config",
   Stdlib.Arg.String (fun s -> Nanosail.Configuration.load_configuration_file s),
   "Specify configuration file");
]


let with_open_file filename func =
  Auxlib.using
    ~resource:(Stdio.Out_channel.create filename)
    ~close:Stdio.Out_channel.close
    ~body:func


let with_stdout func =
  func Stdio.stdout


(** Katamaran target action. *)
let katamaran_target
      (_        : Yojson.Basic.t option                         )
      (_        : string                                        )
      (filename : string option                                 )
      (ast      : Libsail.Type_check.tannot Libsail.Ast_defs.ast)
      (_        : Libsail.Effects.side_effect_info              )
      (_        : Libsail.Type_check.env                        )
  =
  if !Options.print_rewrites
  then begin
      List.iteri ~f:(fun index (rewrite_name, _) ->
          Stdio.printf "[%02d] %s\n" (index + 1) rewrite_name
        )
        (rewrites ())
    end;
  let add_extension filename =
    if Stdlib.Filename.check_suffix filename ".v"
    then filename
    else filename ^ ".v"
  in
  let program_name_from_filename filename =
    String.capitalize (Stdlib.Filename.chop_extension filename)
  in
  let context, program_name =
    match filename with
    | Some filename -> (with_open_file (add_extension filename), program_name_from_filename filename)
    | None          -> (with_stdout, "NoName")
  in
  let nanosail_representation = Nanosail.SailToNanosail.translate ast program_name
  in
  let sanitized_nanosail_representation = Nanosail.SailToNanosail.coqify_identifiers nanosail_representation
  in
  let document = Nanosail.NanosailToMicrosail.Katamaran.fromIR_pp sanitized_nanosail_representation
  in
  context (fun output_channel -> Nanosail.NanosailToMicrosail.Katamaran.pretty_print !Options.width output_channel document)


(** Registering of the katamaran target. *)
let _ =
  Libsail.Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:(rewrites ())
    katamaran_target
