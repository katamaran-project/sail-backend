module R = Libsail.Rewrites


(*
  List of all rewrites, copied from sail source code.
  Note: some of these rewrites still need arguments; they can't be used as-is
*)
let all_rewrites =
  [
    ("recheck_defs", []);
    ("realize_mappings", []);
    ("remove_duplicate_valspecs", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("truncate_hex_literals", []);
    ("mono_rewrites", []);
    ("complete_record_params", []);
    ("toplevel_nexps", []);
    ("toplevel_consts", []);
    ("monomorphise", []);
    ("atoms_to_singletons", []);
    ("add_bitvector_casts", []);
    ("remove_impossible_int_cases", []);
    ("const_prop_mutrec", []);
    ("make_cases_exhaustive", []);
    ("undefined", []);
    ("vector_string_pats_to_bit_list", []);
    ("remove_not_pats", []);
    ("pattern_literals", []);
    ("vector_concat_assignments", []);
    ("tuple_assignments", []);
    ("simple_assignments", []);
    ("simple_struct_assignments", []);
    ("remove_vector_concat", []);
    ("remove_vector_subrange_pats", []);
    ("remove_bitvector_pats", []);
    ("remove_numeral_pats", []);
    ("guarded_pats", []);
    ("bit_lists_to_lits", []);
    ("exp_lift_assign", []);
    ("early_return", []);
    ("nexp_ids", []);
    ("remove_blocks", []);
    ("letbind_effects", []);
    ("remove_e_assign", []);
    ("internal_lets", []);
    ("remove_superfluous_letbinds", []);
    ("remove_superfluous_returns", []);
    ("merge_function_clauses", []);
    ("minimise_recursive_functions", []);
    ("move_termination_measures", []);
    ("rewrite_explicit_measure", []);
    ("rewrite_loops_with_escape_effect", []);
    ("simple_types", []);
    ("instantiate_outcomes", []);
    ("top_sort_defs", []);
    ("constant_fold", []);
    ("split", []);
    ("properties", []);
    ("infer_effects", []);
    ("attach_effects", []);
    ("prover_regstate", []);
    ("add_unspecified_rec", []);
    ("toplevel_let_patterns", []);
    ("remove_bitfield_records", []);
  ]


(**
   List of rewrites applied to the sail ast after type checking and before
   any translation to intermediate representation language.
 *)
let katamaran_rewrites =
  [
    ("instantiate_outcomes", [R.String_arg "coq"]);

    (*
      Rewrites mappings as two functions.
    *)
    ("realize_mappings", []);
    
    ("remove_vector_subrange_pats", []);
    ("remove_duplicate_valspecs", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("truncate_hex_literals", []);
    ("mono_rewrites", []);
    ("recheck_defs", []);

    (*
      Rewrites type signatures of polymorphic functions so that no numeric expressions
      appear in the parameter/return types.

      Example:

        val append_64 = pure {_: "append_64"}: forall 'n. (bitvector('n), bitvector(64)) -> bitvector('n + 64)

      gets rewritten to
     
        val append_64 = pure {_: "append_64"}: forall 'n 'n_plus_p64, 'n_plus_p64 == 'n + 64.
          (bitvector('n), bitvector(64)) -> bitvector('n_plus_p64)
      
    *)
    ("toplevel_nexps", []);
    ("monomorphise", [String_arg "c"]);

    (*
      This rewrite needs to be followed by recheck_defs, otherwise a crash ensues.
      It is unclear what recheck_defs does, as it seems to have no impact on the Sail code,
      at least in cases where it makes a difference in whether atoms_to_singletons leads
      to a crash or not.
      
      This can mean that
      * it affects some internal state in Sail
      * whatever changes it does make are not visible in the code
      
      The second option seems more likely.
    *)
    ("atoms_to_singletons", [String_arg "c"]);
    ("recheck_defs", []);
    
    ("add_unspecified_rec", []);

    (*
      Replaces undefined by calls to the appropriate undefined_xxx() function
      depending on the type, e.g., undefined_int, undefined_bool, etc.
    *)
    ("undefined", [R.Bool_arg true]);

    (*
       Translates 0b000 into [bitzero, bitzero, bitzero]
       Required
    *)
    ("vector_string_pats_to_bit_list", []);
    
    ("remove_not_pats", []);
    ("remove_impossible_int_cases", []);
    ("tuple_assignments", []);
    ("vector_concat_assignments", []);
    ("simple_struct_assignments", []);
    ("simple_assignments", []);
    ("remove_vector_concat", []);
    ("remove_bitvector_pats", []);
    ("remove_numeral_pats", []);
    (* ("pattern_literals", [Literal_arg "lem"]);                (\* From Coq backend *\) *)
    ("pattern_literals", [Literal_arg "all"]);
    ("guarded_pats", []);
    ("split", [R.String_arg "execute"]);
    ("minimise_recursive_functions", []);
    ("recheck_defs", []);
    ("move_termination_measures", []);
    ("const_prop_mutrec", [R.String_arg "coq"]);
    ("exp_lift_assign", []);
    (* ("early_return", []);                                              (\* From Coq backend *\) *)
    ("recheck_defs", []);
    ("make_cases_exhaustive", []);
    ("merge_function_clauses", []);
    ("constant_fold", [String_arg "c"]);
    ("recheck_defs", []);
    ("rewrite_explicit_measure", []);
    ("rewrite_loops_with_escape_effect", []);
    ("recheck_defs", []);
    (* ("remove_blocks", []);                                             (\* From Coq backend *\) *)
    (* ("attach_effects", []);                                            (\* From Coq backend *\) *)
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
