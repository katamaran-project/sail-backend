module R = Libsail.Rewrites


(*
  List of all rewrites, copied from sail source code.

    recheck_defs
    realize_mappings
    remove_duplicate_valspecs
    toplevel_string_append
    pat_string_append
    mapping_patterns
    truncate_hex_literals
    mono_rewrites
    complete_record_params
    toplevel_nexps
    toplevel_consts
    monomorphise
    atoms_to_singletons
    add_bitvector_casts
    remove_impossible_int_cases
    const_prop_mutrec
    pattern_exhaustivity_redundancy
    undefined
    remove_not_pats
    pattern_literals
    vector_concat_assignments
    tuple_assignments
    simple_assignments
    simple_struct_assignments
    remove_vector_concat
    remove_vector_subrange_pats
    remove_bitvector_pats
    remove_numeral_pats
    guarded_pats
    bit_lists_to_lits
    exp_lift_assign
    early_return
    nexp_ids
    remove_blocks
    letbind_effects
    remove_e_assign
    internal_lets
    remove_superfluous_letbinds
    remove_superfluous_returns
    merge_function_clauses
    minimise_recursive_functions
    move_termination_measures
    rewrite_explicit_measure
    rewrite_loops_with_escape_effect
    simple_types
    instantiate_outcomes
    top_sort_defs
    constant_fold
    split
    properties
    infer_effects
    attach_effects
    prover_regstate
    add_unspecified_rec
    toplevel_let_patterns
    remove_bitfield_records
*)


(*
   List of rewrites applied to the sail AST after type checking and before
   any translation to intermediate representation language.
*)
let katamaran_rewrites =
  [
    (*
      Moves termination measures into the functions they belong to, and loop
      measures (termination_measure f repeat/while/until N) into the loops.
      Must come first, like in Sail's own Coq and Lem backends: monomorphise
      rejects top-level loop measures as unreachable.
    *)
    ("move_termination_measures", []);

    (*
      Since Sail 0.20, this rewrite takes an extra argument keep_original_defs.
      Previous versions behaved as if it were false.
    *)
    ("instantiate_outcomes", [R.String_arg "coq"; R.Bool_arg false]);

    (*
      Rewrites mappings as two functions.
    *)
    ("realize_mappings", []);

    ("remove_vector_subrange_pats", []);
    ("remove_duplicate_valspecs", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("recheck_defs", []);
    ("simple_assignments", []);
    ("remove_bitvector_pats", []);
    ("remove_bitfield_records", []);
    ("recheck_defs", []);
    ("truncate_hex_literals", []);
    ("recheck_defs", []);
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
    (*
      Monomorphisation: copies of functions for the values of their size arguments.
      Only with --auto-mono or --mono-split, as in Sail's own backends. Without the
      analysis these options enable, it still splits a union constructor whose payload
      width ranges over a finite set into one constructor per width, while uses whose
      width is not a constant keep naming the original constructor, which no longer exists.
    *)
    ("monomorphise", [String_arg "c"; If_mono_arg]);

    (*
      Turns int('n) parameters that determine a bitvector width into itself('n)
      parameters, passed as make_the_value(n) and read back with size_itself_int(n).
      Part of monomorphisation, so only with --auto-mono or --mono-split, as in Sail's
      own backends. The translator does not need it: it translates make_the_value and
      size_itself_int as the identity and itself('n) as an int.
    *)
    ("atoms_to_singletons", [String_arg "c"; If_mono_arg]);

    (*
      Type checks the definitions again, so that their type annotations agree with the
      val specs that toplevel_nexps changed (bits(8 * 'n) becomes bits('p8_times_n)), and
      that atoms_to_singletons changed if it ran. Later rewrites (e.g., remove_vector_concat)
      fail on the outdated annotations otherwise.
    *)
    ("recheck_defs", []);

    ("add_unspecified_rec", []);

    (*
      Replaces undefined by calls to the appropriate undefined_xxx() function
      depending on the type, e.g., undefined_int, undefined_bool, etc.
    *)
    ("undefined", [R.Bool_arg true]);

    (*
       vector_string_pats_to_bit_list (translates 0b010 into [bitzero, bitone, bitzero])
       was removed in Sail 0.20; the Coq backend dropped it without replacement.
    *)

    ("remove_not_pats", []);

    (*
      Simplifies code where sufficient information is available at compile time.Abi_aarch64_le

        function let_test () =
          let a = true in
          let b = false in
          let c = if b then a else b in
          c

      is rewritten to

        function let_test () =
          let a = true in
          let b = false
          in let c = b in
          c

    *)
    ("remove_impossible_int_cases", []);
    ("tuple_assignments", []);
    ("vector_concat_assignments", []);
    ("simple_struct_assignments", []);
    ("simple_assignments", []);
    ("remove_vector_concat", []);
    ("remove_bitvector_pats", []);

    (*

      Numerical patterns are replaced by pattern guards.

        function iszero x = {
          match x {
            0 => true,
            _ => false
          }
        }

      gets rewritten to

        zero x = {
          match x {
            l__0 if eq_int(l__0, 0) => true,
              _ => false
          }
        }

    *)
    ("remove_numeral_pats", []);
    (* ("pattern_literals", [Literal_arg "lem"]);                (\* From Coq backend *\) *)
    ("pattern_literals", [Literal_arg "all"]);

    (*
      Makes cases exhaustive.

        enum Foo = { x, y, z }

        function foo e = $[incomplete] match e {x => 1}

        function foo e = match e {
          x => 1,
          _ => {
              assert(false, "Pattern match failure at model.sail:10.18-12.1");
              exit(())
          }
        }

    *)
    ("guarded_pats", []);
    ("split", [R.String_arg "execute"]);
    ("minimise_recursive_functions", []);
    ("recheck_defs", []);
    ("const_prop_mutrec", [R.String_arg "coq"]);
    ("exp_lift_assign", []);
    (*
      Pulls early returns out of the positions after which nothing else runs
      (the last expression of a block, both branches of an if, all arms of a match,
      the body of a let or var), so that the function body computes the result directly.
      An if with a return in one branch takes the rest of the block as its other branch.

        function f(x) = {
          if x < 0 then return 0;
          let y = x + 1;
          return y
        }

      becomes

        function f(x) = if x < 0 then 0 else { let y = x + 1; y }

      A return that cannot be pulled out becomes a call to the builtin early_return,
      which the translator does not support (see Function.statement_of_application).
      Functions without return are left alone.
    *)
    ("early_return", []);                                              (* From Coq backend *)
    ("recheck_defs", []);

    (*
      One would expect this rewrite to make cases exhaustive, but it doesn't seem to.
      Instead, the guarded_pats rewrite adds code dealing with missing cases.

      One might think make_cases_exhaustive would do the same were it not
      that guarded_pats already took care of it. However, this is not the case:
      deactivating guarded_pats simply leads to Sail crashing.

      This rewrite does seem to remove useless cases though.
      It rewrote

        function my_enum_to_int_forwards_matches arg# = $[complete] match arg# {
          Foo => true,
          Bar => true,
          Baz => true,
          _ => false
        }

      to

        $[complete]
        function my_enum_to_int_forwards_matches arg# = $[complete] match arg# {
          Foo => true,
          Bar => true,
          Baz => true
        }

    *)
    (*
      make_cases_exhaustive was replaced by pattern_exhaustivity_redundancy in Sail 0.20.
    *)
    ("pattern_exhaustivity_redundancy", []);

    (*
      Joins function clauses into one function.

        function execute Increment = {
            eax = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eax, 1);
            eip = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eip, 1)
        }
        and execute Decrement = {
            eax = $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(eax, 1);
            eip = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eip, 1)

      is rewritten to

        function execute merge#var = match merge#var {
          Increment => {
              eax = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eax, 1);
              eip = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eip, 1)
          },
          Decrement => {
              eax = $[overloaded { "name" = "-", "is_infix" = true }] sub_atom(eax, 1);
              eip = $[overloaded { "name" = "+", "is_infix" = true }] add_atom(eip, 1)
          }

    *)
    ("merge_function_clauses", []);
    ("constant_fold", [String_arg "c"]);
    ("recheck_defs", []);
    (* ("rewrite_explicit_measure", []); *)
    ("rewrite_loops_with_escape_effect", []);
    ("recheck_defs", []);

    (*
        function neg x = {
            neq_bool(x, true)
        }

      becomes

        function neg x = neq_bool(x, true)

      Not used: it turns "{ assert(c); e }" into "let _ : unit = assert(c) in e",
      and Sail's type checker only adds c to the constraints for e when the assert
      is a statement of a block. When e type-checks only thanks to c, the recheck_defs
      below crashes Sail (see tests/coq-tests/assert-flow-typing-fixed-width).
      The translator does not need this rewrite: it handles blocks itself (AE_block).
    *)
    (* ("remove_blocks", []);                                          (\* From Coq backend *\) *)

    ("attach_effects", []);                                            (* From Coq backend *)
    (* ("letbind_effects", []);                                           (\* From Coq backend *\) *)
    (* ("remove_e_assign", []);                                           (\* From Coq backend *\) *)
    (* ("attach_effects", []);                                            (\* From Coq backend *\) *)
    (* ("internal_lets", []);                                             (\* From Coq backend *\) *)

    (*
       function foo _ = let x = true in x

       becomes

       function foo _ = true

    *)
    (* ("remove_superfluous_letbinds", []);                               (\* From Coq backend *\) *)

    (* ("remove_superfluous_returns", []);                                (\* From Coq backend *\) *)

    (*
      Rewrites bit lists to binary literals.

        [bitzero, bitone]

      gets rewritten to

        0b01
    *)
    (* ("bit_lists_to_lits", []); *)

    ("toplevel_let_patterns", []);                                     (* From Coq backend *)
    ("recheck_defs", []);                                              (* From Coq backend *)
    ("attach_effects", []);                                            (* From Coq backend *)

    (*
      Simplifies types (turns them unrefined), but causes trouble in most cases
      If actived, should probably be placed at the very end of the rewrite list
    *)
    (* ("simple_types", []); *)
  ]


(*
  Rewrites of the plugin's own, run by the plugin after katamaran_rewrites.
  Sail knows only its own rewrites by name (Libsail.Rewrites.all_rewriters) and runs
  the rewrites a target registers before it calls the target, so the plugin runs these
  itself, with Libsail.Rewrites.rewrite; --ddump-rewrite-ast dumps them too.
*)
let plugin_rewrites : R.rewrite_sequence =
  ("fold_constant_type_conditionals", ConstantTypeConditionals.rewrite)
  :: R.instantiate_rewrites [
         ("recheck_defs", []);
         ("attach_effects", []);
       ]
