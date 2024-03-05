module R = Libsail.Rewrites

(**
   List of rewrites applied to the sail ast after type checking and before
   any translation to intermediate representation language.
 *)
let katamaran_rewrites =
  [
    ("prover_regstate", [R.Bool_arg true]);                            (* From Coq backend *)
    ("instantiate_outcomes", [R.String_arg "coq"]);                    (* From Coq backend *)
    ("realize_mappings", []);                                          (* From Coq backend *)
    ("remove_vector_subrange_pats", []);                               (* From Coq backend *)
    ("remove_duplicate_valspecs", []);                                 (* From Coq backend *)
    ("toplevel_string_append", []);                                    (* From Coq backend *)
    ("pat_string_append", []);                                         (* From Coq backend *)
    ("mapping_patterns", []);                                          (* From Coq backend *)
    ("truncate_hex_literals", []);                                     (* From C backend   *)
    ("mono_rewrites", [If_flag R.opt_mono_rewrites]);                  (* From C backend   *)
    ("recheck_defs", [If_flag R.opt_mono_rewrites]);                   (* From C backend   *)
    ("toplevel_nexps", [If_mono_arg]);                                 (* From C backend   *)
    ("monomorphise", [String_arg "c"; If_mono_arg]);                   (* From C backend   *)
    ("atoms_to_singletons", [String_arg "c"; If_mono_arg]);            (* From C backend   *)
    ("add_unspecified_rec", []);                                       (* From Coq backend *)
    ("undefined", [R.Bool_arg true]);                                  (* From Coq backend *)
    ("vector_string_pats_to_bit_list", []);                            (* From Coq backend *)
    ("remove_not_pats", []);                                           (* From Coq backend *)
    ("remove_impossible_int_cases", []);                               (* From Coq backend *)
    ("tuple_assignments", []);                                         (* From Coq backend *)
    ("vector_concat_assignments", []);                                 (* From Coq backend *)
    ("simple_struct_assignments", []);                                 (* From C backend   *)
    ("simple_assignments", []);                                        (* From Coq backend *)
    ("remove_vector_concat", []);                                      (* From Coq backend *)
    ("remove_bitvector_pats", []);                                     (* From Coq backend *)
    ("remove_numeral_pats", []);                                       (* From Coq backend *)
    (* ("pattern_literals", [Rewrites.Literal_arg "lem"]);                (\* From Coq backend *\) *)
    ("pattern_literals", [Literal_arg "all"]);                         (* From C   backend *)
    ("guarded_pats", []);                                              (* From Coq backend *)
    ("split", [R.String_arg "execute"]);                               (* From Coq backend *)
    ("minimise_recursive_functions", []);                              (* From Coq backend *)
    ("recheck_defs", []);                                              (* From Coq backend *)
    ("move_termination_measures", []);                                 (* From Coq backend *)
    ("top_sort_defs", []);                                             (* From Coq backend *)
    ("const_prop_mutrec", [R.String_arg "coq"]);                       (* From Coq backend *)
    ("exp_lift_assign", []);                                           (* From Coq backend *)
    (* ("early_return", []);                                              (\* From Coq backend *\) *)
    ("recheck_defs", []);                                              (* From Coq backend *)
    ("make_cases_exhaustive", []);                                     (* From Coq backend *)
    ("merge_function_clauses", []);                                    (* From Coq backend *)
    ("constant_fold", [String_arg "c"]);                               (* From C backend   *)
    ("recheck_defs", []);                                              (* From Coq backend *)
    ("rewrite_explicit_measure", []);                                  (* From Coq backend *)
    ("rewrite_loops_with_escape_effect", []);                          (* From Coq backend *)
    ("recheck_defs", []);                                              (* From Coq backend *)
    ("remove_blocks", []);                                             (* From Coq backend *)
    ("attach_effects", []);                                            (* From Coq backend *)
    (* ("letbind_effects", []);                                           (\* From Coq backend *\) *)
    (* ("remove_e_assign", []);                                           (\* From Coq backend *\) *)
    (* ("attach_effects", []);                                            (\* From Coq backend *\) *)
    (* ("internal_lets", []);                                             (\* From Coq backend *\) *)
    (* ("remove_superfluous_letbinds", []);                               (\* From Coq backend *\) *)
    (* ("remove_superfluous_returns", []);                                (\* From Coq backend *\) *)
    (* ("bit_lists_to_lits", []);                                         (\* From Coq backend *\) *)
    (* ("toplevel_let_patterns", []);                                     (\* From Coq backend *\) *)
    (* ("recheck_defs", []);                                              (\* From Coq backend *\) *)
    (* ("attach_effects", []);                                            (\* From Coq backend *\) *)
  ]


(* Can be ignored *)
let _c_rewrites =
  [
    ("instantiate_outcomes", [R.String_arg "c"]);
    ("realize_mappings", []);
    ("remove_vector_subrange_pats", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("truncate_hex_literals", []);
    ("mono_rewrites", [If_flag R.opt_mono_rewrites]);
    ("recheck_defs", [If_flag R.opt_mono_rewrites]);
    ("toplevel_nexps", [If_mono_arg]);
    ("monomorphise", [String_arg "c"; If_mono_arg]);
    ("atoms_to_singletons", [String_arg "c"; If_mono_arg]);
    ("recheck_defs", [If_mono_arg]);
    ("undefined", [Bool_arg false]);
    ("vector_string_pats_to_bit_list", []);
    ("remove_not_pats", []);
    ("remove_vector_concat", []);
    ("remove_bitvector_pats", []);
    ("pattern_literals", [Literal_arg "all"]);
    ("tuple_assignments", []);
    ("vector_concat_assignments", []);
    ("simple_struct_assignments", []);
    ("exp_lift_assign", []);
    ("merge_function_clauses", []);
    ("recheck_defs", []);
    ("constant_fold", [String_arg "c"]);
  ]

