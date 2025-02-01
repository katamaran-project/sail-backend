open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module TM = SailToNanosail.Translate.Match
module PN = TM.PatternNode

open Shared


let test_build_pattern_tree_variant_single_unary_constructor =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let* actual_pattern_tree : PN.t =
        build_empty_pattern_tree [ enum_type ]
      in
      let expected_pattern_tree : PN.t =
        PN.Variant {
          variant_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                PN.UnaryConstructor (gen#wildcard, PN.Terminal None)
              );
            ]
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string PN.to_fexpr)
        ~cmp:TM.PatternNode.equal
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
]
