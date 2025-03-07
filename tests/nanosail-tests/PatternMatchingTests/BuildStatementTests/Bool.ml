open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open PatternMatchingShared


let test_build_match_for_bool_true_false =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let true_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and false_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* tree =
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
      let* actual_match_statement =
        build_match [mkid "b"] tree Ast.Type.Unit
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchBool {
            condition  = mkid "b";
            when_true  = true_statement;
            when_false = false_statement;
          }
        end
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
    match b {
      true  => r1,
      false => r2
    }
  |} >:: test


let test_build_match_for_bool_false_true =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let false_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and true_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* tree =
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
      let* actual_match_statement =
        build_match [mkid "b"] tree Ast.Type.Unit
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchBool {
            condition  = mkid "b";
            when_true  = true_statement;
            when_false = false_statement;
          }
        end
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
    match b {
      false => r1,
      true  => r2
    }
  |} >:: test


let test_build_match_for_bool_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Bool ]
        in
        let* tree = adorn
          tree
          [ Pattern.Binder gen#wildcard ]
          statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "b"] tree Ast.Type.Unit
      in
      let expected_match_statement =
        statement
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      match b {
        _ => r1
      }

    should become

      r1
  |} >:: test


let test_build_match_for_bool_binder =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Bool ]
        in
        let* tree = adorn
          tree
          [ Pattern.Binder (mkbinder "x") ]
          statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "b"] tree Ast.Type.Unit
      in
      let expected_match_statement =
        Ast.Statement.Let {
          binder                 = mkid "x";
          binding_statement_type = Ast.Type.Bool;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "b", Ast.Type.Bool));
          body_statement         = statement;
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      match b {
        x => r1
      }

    should become

      let x = b in r1
  |} >:: test



let test_suite =
  "bool" >::: [
    test_build_match_for_bool_true_false;
    test_build_match_for_bool_false_true;
    test_build_match_for_bool_wildcard;
    test_build_match_for_bool_binder;
  ]
