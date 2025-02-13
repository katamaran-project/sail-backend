open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests
module Pattern        = SailToNanosail.Translate.Match.Pattern
module TM             = SailToNanosail.Translate.Match

open Shared
open Monads.Notations.Star(TC)


let test_adorn_bool_true_false =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let true_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and false_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Bool ]
        in
        let* tree = adorn
          tree
          [ Pattern.BoolCase true ]
          true_statement
        in
        let* tree = adorn
          tree
          [ Pattern.BoolCase false ]
          false_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Bool {
          when_true  = TM.PatternTree.Terminal (Some true_statement);
          when_false = TM.PatternTree.Terminal (Some false_statement)
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
    match b {
      true  => r1,
      false => r2
    }
  |} >:: test


let test_adorn_bool_false_true =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let true_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      and false_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Bool ]
        in
        let* tree = adorn
          tree
          [ Pattern.BoolCase false ]
          false_statement
        in
        let* tree = adorn
          tree
          [ Pattern.BoolCase true ]
          true_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Bool {
            when_true  = TM.PatternTree.Terminal (Some true_statement);
            when_false = TM.PatternTree.Terminal (Some false_statement)
          }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
    match b {
      false => r1,
      true  => r2
    }
  |} >:: test


let test_adorn_bool_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Bool ]
        in
        let* tree = adorn
          tree
          [ Pattern.Binder gen#wildcard ]
          statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Binder {
          matched_type = Ast.Type.Bool;
          binder       = gen#wildcard;
          subtree      = TM.PatternTree.Terminal (Some statement)
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc

  in
  {|
    match b {
      _ => r1
    }
  |} >:: test


let test_suite =
  "bool" >::: [
    test_adorn_bool_true_false;
    test_adorn_bool_false_true;
    test_adorn_bool_wildcard;
  ]
