open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching
module PN      = TM.PatternNode

open Shared


let test_categorize_variant_single_unary_constructor =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* actual_pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x"; wildcard = false } ))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let expected_pattern_tree =
        PN.Variant {
          variant_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                PN.UnaryConstructor (Some (mkid "x"), PN.Terminal (Some a1_statement))
              );
            ]
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string PN.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_pattern_tree
        actual_pattern_tree;
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
  "enum" >::: [
    test_categorize_variant_single_unary_constructor;
  ]
