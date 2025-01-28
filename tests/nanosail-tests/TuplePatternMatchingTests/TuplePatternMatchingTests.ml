open Base
open OUnit2
open Nanosail

module BuildChainTests = BuildChainTests


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
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true }
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
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
        expected_match_statement
        actual_match_statement;
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = false }
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
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
        expected_match_statement
        actual_match_statement;
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int; Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true };
              Pattern.Binder { identifier = mkid "k"; wildcard = true };
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int; Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = false };
              Pattern.Binder { identifier = mkid "k"; wildcard = false };
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      match (intval, intval) {
        (n, k) => read_register r1,
      }
  |} >:: test


let test_build_match_for_enum_int =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let statement =
        mkstm 1
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.Binder { identifier = mkid "k"; wildcard = true };
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "enum_value"; mkid "int_value"] chain
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "enum_value";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Let {
                    variable_identifier    = mkid "k";
                    binding_statement_type = Ast.Type.Int;
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "int_value", Ast.Type.Int));
                    body_statement         = statement
                  }
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A = { A1 }
    
      match (enumval, intval) {
        (A1, k) => read_register r1,
      }
  |} >:: test



let test_generate_match_suite =
  "match generation" >::: [
    test_build_match_for_int_1;
    test_build_match_for_int_2;
    test_build_match_for_int_int_1;
    test_build_match_for_int_int_2;

    test_build_match_for_enum_int;
  ]


let test_suite =
  "tuple pattern matching" >::: [
    BuildChainTests.test_suite;
    CategorizeTests.test_suite;
    BuildStatementTests.test_suite;
    test_generate_match_suite;
  ]
