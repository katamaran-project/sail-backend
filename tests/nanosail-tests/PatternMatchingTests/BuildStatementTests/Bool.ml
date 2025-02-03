open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open Shared


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
        build_match [mkid "b"] tree
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
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        (Normalize.normalize_statement expected_match_statement)
        (Normalize.normalize_statement actual_match_statement);
      TC.return ()
    in
    ignore @@ run_tc tc
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
        build_match [mkid "b"] tree
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
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        (Normalize.normalize_statement expected_match_statement)
        (Normalize.normalize_statement actual_match_statement);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
    match b {
      false => r1,
      true  => r2
    }
  |} >:: test


let test_suite =
  "bool" >::: [
    test_build_match_for_bool_true_false;
    test_build_match_for_bool_false_true;
  ]
