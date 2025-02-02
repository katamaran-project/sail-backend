open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match
module PN      = TM.PatternTree

open Shared


let test_adorn_variant_single_unary_constructor =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ enum_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder (mkbinder "x") ))
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let expected_pattern_tree =
        PN.Variant {
          variant_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, PN.UnaryConstructor (mkbinder "x"), PN.Terminal (Some a1_statement))
              );
            ]
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string PN.to_fexpr)
        ~cmp:TM.PatternTree.equal
        (Normalize.normalize_pattern_tree expected_pattern_tree)
        (Normalize.normalize_pattern_tree actual_pattern_tree);
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


let test_adorn_variant_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ enum_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder gen#wildcard;
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let expected_pattern_tree =
        PN.Variant {
          variant_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, PN.UnaryConstructor gen#wildcard, PN.Terminal (Some a1_statement))
              );
            ]
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string PN.to_fexpr)
        ~cmp:TM.PatternTree.equal
        (Normalize.normalize_pattern_tree expected_pattern_tree)
        (Normalize.normalize_pattern_tree actual_pattern_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : int
      }

      match value1 {
        _ => read_register r1,
      }
  |} >:: test



let test_adorn_variant_binder =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ enum_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder (mkbinder "x");
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let expected_pattern_tree =
        PN.Variant {
          variant_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (mkbinder "x", PN.UnaryConstructor (gen#wildcard), PN.Terminal (Some a1_statement))
              );
            ]
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string PN.to_fexpr)
        ~cmp:TM.PatternTree.equal
        (Normalize.normalize_pattern_tree expected_pattern_tree)
        (Normalize.normalize_pattern_tree actual_pattern_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : int
      }

      match value1 {
        x => read_register r1,
      }
  |} >:: test


let test_failure_due_to_clashing_field_binders =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [
          ("A1", [Ast.Type.Int]);
          ("A2", [Ast.Type.Int]);
        ]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree = build_empty_pattern_tree [ enum_type; enum_type ]
      in
      let* pattern_tree = adorn
          pattern_tree
          [
            Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x1"; wildcard = false } ));
            Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "y"; wildcard = false } ))
          ]
          a1_statement
      in
      let* _ = adorn
          pattern_tree
          [
            Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x2"; wildcard = false } ));
            Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "y"; wildcard = false } ))
          ]
          a1_statement
      in
        TC.return ()
    in
    run_failing_tc tc
  in
  {|
      union A = {
        A1 : int,
        A2 : int
      }

      match (value1, value2) {
        (A1(x1), A1(y)) => read_register r1,
        (A1(x2), A2(y)) => read_register r1,
      }
  |} >:: test


let test_suite =
  "enum" >::: [
    test_adorn_variant_single_unary_constructor;
    test_adorn_variant_wildcard;
    test_adorn_variant_binder;

    test_failure_due_to_clashing_field_binders;
  ]
