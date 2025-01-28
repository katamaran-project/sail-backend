open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_match_for_variant_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Unit))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [],
                    a1_statement
                  )
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
      union A = {
        A1 : unit
      }

      match value1 {
        A1() => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_2 =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Binder { identifier = mkid "x"; wildcard = false } ))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x" ],
                    a1_statement
                  )
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
      union A = {
        A1 : int
      }

      match value1 {
        A1(x) => read_register r1,
      }
  |} >:: test


let test_suite =
  "match generation" >::: [
    test_build_match_for_variant_1;
    test_build_match_for_variant_2;
  ]
