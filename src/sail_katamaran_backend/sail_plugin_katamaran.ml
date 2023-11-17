open Libsail
open Nanosail.Sail_to_nanosail
open Nanosail.Pretty_printing_katamaran

(** Width of the output (dfault is 80) *)
let opt_width = ref 80

(** Command line options added to sail when the sail_katamaran_backend is loaded
    or installed. *)
let katamaran_options = [
  ("-ok",
    Arg.Unit (fun () -> print_endline("ok.")) ,
    " (debug) print \"ok.\"");
  ("-list_notations",
    Arg.Set opt_list_notations,
    " use list notations");
  ("-w",
    Arg.Set_int opt_width,
    " set a custom width for the output")
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
    ("nexp_ids", []);
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

(** Katamaran target action. *)
let katamaran_target _ _ out_file ast _ _ =
  let close, output_chan, prog_name = match out_file with
    | Some f -> (true, open_out (f ^ ".v"), String.capitalize_ascii f)
    | None   -> (false, stdout, "NoName")
  in let ir = sail_to_nanosail ast prog_name
  in pretty_print !opt_width output_chan
    (fromIR_pp ir);
  if close then close_out output_chan

(** Registering of the katamaran target. *)
let _ = Target.register
    ~name:"katamaran"
    ~options:katamaran_options
    ~rewrites:katamaran_rewrites
    katamaran_target
