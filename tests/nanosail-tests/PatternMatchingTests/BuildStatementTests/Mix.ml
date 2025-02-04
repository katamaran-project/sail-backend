open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open Shared


let test_build_match_for_enum_int =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let statement =
        mkstm 1
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; Ast.Type.Int ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.Binder { identifier = mkid "k"; wildcard = false };
            ]
            statement
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "enum_value"; mkid "int_value"] tree
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
        (Normalize.normalize_statement expected_match_statement)
        (Normalize.normalize_statement actual_match_statement);
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



let test_suite =
  "mix" >::: [
    test_build_match_for_enum_int;
  ]
