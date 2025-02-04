open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module TM = SailToNanosail.Translate.Match

open Shared


let test_build_pattern_tree_variant_single_unary_constructor =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let* actual_pattern_tree : TM.PatternTree.t =
        build_empty_pattern_tree [ variant_type ]
      in
      let expected_pattern_tree : TM.PatternTree.t =
        TM.PatternTree.Binder {
          matched_type = variant_type;
          binder       = gen#wildcard;
          subtree      = TM.PatternTree.Terminal None
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternTree.to_fexpr)
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
  |} >:: test


let test_build_pattern_tree_variant_single_unary_constructor_pair =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let* actual_pattern_tree : TM.PatternTree.t =
        build_empty_pattern_tree [ variant_type; variant_type ]
      in
      let expected_pattern_tree : TM.PatternTree.t =
        TM.PatternTree.Binder {
          matched_type = variant_type;
          binder       = gen#wildcard;
          subtree      = TM.PatternTree.Binder {
              matched_type = variant_type;
              binder       = gen#wildcard;
              subtree      = TM.PatternTree.Terminal None
            }
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternTree.to_fexpr)
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
  |} >:: test


let test_suite = "variant" >::: [
    test_build_pattern_tree_variant_single_unary_constructor;
    test_build_pattern_tree_variant_single_unary_constructor_pair;
]
