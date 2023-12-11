open Libsail


(** Width of the output (default is 100) *)
let opt_width = ref 100

let print_check_message () =
  print_endline("Katamaran plugin is functioning correctly")

(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  ("-katamaran_check",
    Arg.Unit print_check_message,
    "(debug) check if Katamaran plugin is correctly installed");
  ("-katamaran_list_notations",
   Arg.Unit (fun () -> Nanosail.Settings.set_list_notations true),
    "use list notations");
  ("-katamaran_width",
    Arg.Set_int opt_width,
    "set a custom width for the output");
  ("-katamaran_add_original",
   Arg.Unit (fun () -> Nanosail.Settings.set_include_original_code true),
   "show original Sail code in output");
  ("-katamaran_include_untranslated",
   Arg.Unit (fun () -> Nanosail.Settings.set_include_untranslated_definitions true),
   "include information about untranslated Sail code")
]

(** List of rewrites applied to the sail ast after type checking and before
    any translation to intermediate representation language. *)
let katamaran_rewrites = [
    ("prover_regstate", [Rewrites.Bool_arg true]);              (* From Coq backend *)
    ("instantiate_outcomes", [Rewrites.String_arg "coq"]);      (* From Coq backend *)
    ("realize_mappings", []);                                   (* From Coq backend *)
    ("remove_vector_subrange_pats", []);                        (* From Coq backend *)
    ("remove_duplicate_valspecs", []);                          (* From Coq backend *)
    ("toplevel_string_append", []);                             (* From Coq backend *)
    ("pat_string_append", []);                                  (* From Coq backend *)
    ("mapping_patterns", []);                                   (* From Coq backend *)
    ("truncate_hex_literals", []);                              (* From C backend *)
    ("mono_rewrites", [Rewrites.Bool_arg true]);                (* From C backend *)
    ("add_unspecified_rec", []);                                (* From Coq backend *)
    ("undefined", [Rewrites.Bool_arg true]);                    (* From Coq backend *)
    ("vector_string_pats_to_bit_list", []);                     (* From Coq backend *)
    ("remove_not_pats", []);                                    (* From Coq backend *)
    ("remove_impossible_int_cases", []);                        (* From Coq backend *)
    ("tuple_assignments", []);                                  (* From Coq backend *)
    ("vector_concat_assignments", []);                          (* From Coq backend *)
    ("simple_assignments", []);                                 (* From Coq backend *)
    ("remove_vector_concat", []);                               (* From Coq backend *)
    ("remove_bitvector_pats", []);                              (* From Coq backend *)
    ("remove_numeral_pats", []);                                (* From Coq backend *)
    ("pattern_literals", [Rewrites.Literal_arg "lem"]);         (* From Coq backend *)
    ("guarded_pats", []);                                       (* From Coq backend *)
    (* ("register_ref_writes", rewrite_register_ref_writes); *) (* From Coq backend *)
    (* ("nexp_ids", []); *)                                     (* From Coq backend *)
    ("split", [Rewrites.String_arg "execute"]);                 (* From Coq backend *)
    ("minimise_recursive_functions", []);                       (* From Coq backend *)
    ("recheck_defs", []);                                       (* From Coq backend *)
    (* ("remove_assert", rewrite_ast_remove_assert); *)         (* From Coq backend *)
    ("move_termination_measures", []);                          (* From Coq backend *)
    ("top_sort_defs", []);                                      (* From Coq backend *)
    ("const_prop_mutrec", [Rewrites.String_arg "coq"]);         (* From Coq backend *)
    ("exp_lift_assign", []);                                    (* From Coq backend *)
    ("early_return", []);                                       (* From Coq backend *)
    (* We need to do the exhaustiveness check before merging, because it may
       introduce new wildcard clauses *)
    ("recheck_defs", []);                                       (* From Coq backend *)
    ("make_cases_exhaustive", []);                              (* From Coq backend *)
    (* merge funcls before adding the measure argument so that it doesn't
       disappear into an internal pattern match *)
    ("merge_function_clauses", []);                             (* From Coq backend *)
    ("recheck_defs", []);                                       (* From Coq backend *)
    ("rewrite_explicit_measure", []);                           (* From Coq backend *)
    ("rewrite_loops_with_escape_effect", []);                   (* From Coq backend *)
    ("recheck_defs", []);                                       (* From Coq backend *)
    ("remove_blocks", []);                                      (* From Coq backend *)
    ("attach_effects", []);                                     (* From Coq backend *)
    ("letbind_effects", []);                                    (* From Coq backend *)
    ("remove_e_assign", []);                                    (* From Coq backend *)
    ("attach_effects", []);                                     (* From Coq backend *)
    ("internal_lets", []);                                      (* From Coq backend *)
    ("remove_superfluous_letbinds", []);                        (* From Coq backend *)
    ("remove_superfluous_returns", []);                         (* From Coq backend *)
    ("bit_lists_to_lits", []);                                  (* From Coq backend *)
    ("toplevel_let_patterns", []);                              (* From Coq backend *)
    ("recheck_defs", []);                                       (* From Coq backend *)
    ("attach_effects", []);                                     (* From Coq backend *)
  ]


let with_open_file filename func =
  let output_channel = open_out filename in
  try
    func output_channel
  with e -> close_out output_channel; raise e


let with_stdout func =
  func stdout


(** Katamaran target action. *)
let katamaran_target _ _ filename ast _ _ =
  let add_extension filename =
    if Filename.check_suffix filename ".v"
    then filename
    else filename ^ ".v"
  in
  let program_name_from_filename filename =
    String.capitalize_ascii (Filename.chop_extension filename)
  in
  let context, program_name =
    match filename with
    | Some filename -> (with_open_file (add_extension filename), program_name_from_filename filename)
    | None          -> (with_stdout, "NoName")
  in
  let nanosail_representation = Nanosail.sail_to_nanosail ast program_name
  in
  let document = Nanosail.Gen.Katamaran.fromIR_pp nanosail_representation
  in
  context (fun output_channel -> Nanosail.Gen.Katamaran.pretty_print !opt_width output_channel document)


(** Registering of the katamaran target. *)
let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target
