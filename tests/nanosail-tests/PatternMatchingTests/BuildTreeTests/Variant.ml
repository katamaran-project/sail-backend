open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module TM = SailToNanosail.Translate.Match

open PatternMatchingShared


let test_build_pattern_tree_variant_single_unary_constructor =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [Int None])]
      in
      let* actual_tree : TM.PatternTree.t =
        build_empty_pattern_tree [ variant_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = variant_type;
          binder       = gen#wildcard;
          subtree      = Leaf None
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
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
        TC.define_variant "A" [("A1", [Int None])]
      in
      let* actual_tree : TM.PatternTree.t =
        build_empty_pattern_tree [ variant_type; variant_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = variant_type;
          binder       = gen#wildcard;
          subtree      = Binder {
              matched_type = variant_type;
              binder       = gen#wildcard;
              subtree      = Leaf None
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : int
      }

      tuple (A, A)
  |} >:: test


let test_suite = "variant" >::: [
    test_build_pattern_tree_variant_single_unary_constructor;
    test_build_pattern_tree_variant_single_unary_constructor_pair;
]
