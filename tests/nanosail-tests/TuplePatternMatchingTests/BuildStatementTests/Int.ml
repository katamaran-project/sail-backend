open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_match_for_int_1 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true }
            ]
            statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier    = mkid "n";
          binding_statement_type = Ast.Type.Int;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement         = statement;
        }
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
      match intval {
        _ => read_register r1,
      }
  |} >:: test


let test_build_match_for_int_2 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = false }
            ]
            statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier    = mkid "n";
          binding_statement_type = Ast.Type.Int;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement         = statement;
        }
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
      match intval {
        n => read_register r1,
      }
  |} >:: test


let test_build_match_for_int_int_1 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; Ast.Type.Int ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true };
              Pattern.Binder { identifier = mkid "k"; wildcard = true };
            ]
            statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier        = mkid "n";
          binding_statement_type     = Ast.Type.Int;
          binding_statement          = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement             = Ast.Statement.Let {
              variable_identifier    = mkid "k";
              binding_statement_type = Ast.Type.Int;
              binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value2", Ast.Type.Int));
              body_statement         = statement;
            };
        }
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
      match (intval, intval) {
        (_, _) => read_register r1,
      }
  |} >:: test


let test_build_match_for_int_int_2 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; Ast.Type.Int ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = false };
              Pattern.Binder { identifier = mkid "k"; wildcard = false };
            ]
            statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier        = mkid "n";
          binding_statement_type     = Ast.Type.Int;
          binding_statement          = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement             = Ast.Statement.Let {
              variable_identifier    = mkid "k";
              binding_statement_type = Ast.Type.Int;
              binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value2", Ast.Type.Int));
              body_statement         = statement;
            };
        }
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
      match (intval, intval) {
        (n, k) => read_register r1,
      }
  |} >:: test



let test_suite =
  "match generation" >::: [
    test_build_match_for_int_1;
    test_build_match_for_int_2;
    test_build_match_for_int_int_1;
    test_build_match_for_int_int_2;
  ]
