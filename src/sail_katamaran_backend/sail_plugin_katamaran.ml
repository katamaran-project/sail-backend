open Libsail
open Nanosail.Sail_to_nanosail
open Nanosail.Pretty_printing_katamaran

(** Width of the output (default is 200) *)
let opt_width = ref 100

let opt_include_untranslated = ref false

(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  ("-katamaran_check",
    Arg.Unit (fun () -> print_endline("Katamaran plugin is functioning correctly")) ,
    "(debug) check if Katamaran plugin is correctly installed");
  ("-katamaran_list_notations",
    Arg.Set opt_list_notations,
    "use list notations");
  ("-katamaran_width",
    Arg.Set_int opt_width,
    "set a custom width for the output");
  ("-katamaran_add_original",
   Arg.Set include_original_sail_code,
   "show original Sail code in output");
  ("-katamaran_include_untranslated",
   Arg.Set opt_include_untranslated,
   "include information about untranslated Sail code")
]

(** List of rewrites applied to the sail ast after type checking and before
    any translation to intermediate representation language. *)
let katamaran_rewrites = [
    ("prover_regstate", [Rewrites.Bool_arg true]);
    ("instantiate_outcomes", [Rewrites.String_arg "coq"]);
    ("realize_mappings", []);
    ("remove_vector_subrange_pats", []);
    ("remove_duplicate_valspecs", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("add_unspecified_rec", []);
    ("undefined", [Rewrites.Bool_arg true]);
    ("vector_string_pats_to_bit_list", []);
    ("remove_not_pats", []);
    ("remove_impossible_int_cases", []);
    ("tuple_assignments", []);
    ("vector_concat_assignments", []);
    ("simple_assignments", []);
    ("remove_vector_concat", []);
    ("remove_bitvector_pats", []);
    ("remove_numeral_pats", []);
    ("pattern_literals", [Rewrites.Literal_arg "lem"]);
    ("guarded_pats", []);
    (* ("register_ref_writes", rewrite_register_ref_writes); *)
    (* ("nexp_ids", []); *)
    ("split", [Rewrites.String_arg "execute"]);
    ("minimise_recursive_functions", []);
    ("recheck_defs", []);
    (* ("remove_assert", rewrite_ast_remove_assert); *)
    ("move_termination_measures", []);
    ("top_sort_defs", []);
    ("const_prop_mutrec", [Rewrites.String_arg "coq"]);
    ("exp_lift_assign", []);
    ("early_return", []);
    (* We need to do the exhaustiveness check before merging, because it may
       introduce new wildcard clauses *)
    ("recheck_defs", []);
    ("make_cases_exhaustive", []);
    (* merge funcls before adding the measure argument so that it doesn't
       disappear into an internal pattern match *)
    ("merge_function_clauses", []);
    ("recheck_defs", []);
    ("rewrite_explicit_measure", []);
    ("rewrite_loops_with_escape_effect", []);
    ("recheck_defs", []);
    ("remove_blocks", []);
    ("attach_effects", []);
    ("letbind_effects", []);
    ("remove_e_assign", []);
    ("attach_effects", []);
    ("internal_lets", []);
    ("remove_superfluous_letbinds", []);
    ("remove_superfluous_returns", []);
    ("bit_lists_to_lits", []);
    ("toplevel_let_patterns", []);
    ("recheck_defs", []);
    ("attach_effects", []);
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
  let nanosail_representation = sail_to_nanosail ast program_name
  in
  let document = fromIR_pp ~show_untranslated:!opt_include_untranslated nanosail_representation
  in
  context (fun output_channel -> pretty_print !opt_width output_channel document)


(** Registering of the katamaran target. *)
let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target
