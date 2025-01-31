open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_pattern_tree_enum_1 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_tree =
        build_tuple_pattern_tree [ enum_type ]
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternNode.Terminal None)
              );
              (
                mkid "A2",
                (gen#wildcard, TM.PatternNode.Terminal None)
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree actual_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
    enum A = { A1, A2 }

    tuple (A)
  |} >:: test


let test_build_pattern_tree_enum_2 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"; "A3"]
      in
      let* actual_tree =
        build_tuple_pattern_tree [ enum_type ]
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternNode.Terminal None)
              );
              (
                mkid "A2",
                (gen#wildcard, TM.PatternNode.Terminal None)
              );
              (
                mkid "A3",
                (gen#wildcard, TM.PatternNode.Terminal None)
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree actual_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
    enum A = { A1, A2, A3 }

    tuple (A)
  |} >:: test


let test_build_pattern_tree_enum_3 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let* actual_tree =
        build_tuple_pattern_tree [ enum_type; enum_type ]
      in
      let expected_tree : TM.PatternNode.t =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,
                  TM.PatternNode.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (gen#wildcard, TM.PatternNode.Terminal None)
                        );
                      ];
                  }
                )
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree actual_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A = { A1, A2 }

      tuple (A, A)
  |} >:: test


let test_build_pattern_tree_enum_4 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_tree =
        build_tuple_pattern_tree [ enum_type; enum_type ]
      in
      let expected_tree : TM.PatternNode.t =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,
                  TM.PatternNode.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (gen#wildcard, TM.PatternNode.Terminal None)
                        );
                        (
                          mkid "A2",
                          (gen#wildcard, TM.PatternNode.Terminal None)
                        );
                      ];
                  }
                )
              );
              (
                mkid "A2",
                (
                  gen#wildcard,
                  TM.PatternNode.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (gen#wildcard, TM.PatternNode.Terminal None)
                        );
                        (
                          mkid "A2",
                          (gen#wildcard, TM.PatternNode.Terminal None)
                        );
                      ];
                  }
                )
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree actual_tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A = { A1, A2 }

      tuple (A, A)
  |} >:: test


let test_suite = "enum" >::: [
    test_build_pattern_tree_enum_1;
    test_build_pattern_tree_enum_2;
    test_build_pattern_tree_enum_3;
    test_build_pattern_tree_enum_4;
]
