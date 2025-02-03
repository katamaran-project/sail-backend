open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open Shared


let test_adorn_bool_1 =
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
      let expected_tree =
        TM.PatternTree.Bool begin
          TM.PatternTree.SeparateBoolCases {
            when_true  = TM.PatternTree.Terminal (Some true_statement);
            when_false = TM.PatternTree.Terminal (Some false_statement)
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternTree.to_fexpr)
        ~cmp:TM.PatternTree.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
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


let test_suite =
  "bool" >::: [
    test_adorn_bool_1;
  ]
