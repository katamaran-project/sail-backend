open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_chain_enum_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_chain =
        build_tuple_pattern_chain [ enum_type ]
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (None, TM.PatternNode.Terminal None)
              );
              (
                mkid "A2",
                (None, TM.PatternNode.Terminal None)
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain
        actual_chain;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
    enum A { A1, A2 }

    tuple (A)
  |} >:: test


let test_build_chain_enum_2 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"; "A3"]
      in
      let* actual_chain =
        build_tuple_pattern_chain [ enum_type ]
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (None, TM.PatternNode.Terminal None)
              );
              (
                mkid "A2",
                (None, TM.PatternNode.Terminal None)
              );
              (
                mkid "A3",
                (None, TM.PatternNode.Terminal None)
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain
        actual_chain;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
    enum A { A1, A2, A3 }

    tuple (A)
  |} >:: test


let test_build_chain_enum_3 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_chain =
        build_tuple_pattern_chain [ enum_type; enum_type ]
      in
      let expected_chain : TM.PatternNode.t =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  None,
                  TM.PatternNode.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (None, TM.PatternNode.Terminal None)
                        );
                        (
                          mkid "A2",
                          (None, TM.PatternNode.Terminal None)
                        );
                      ];
                  }
                )
              );
              (
                mkid "A2",
                (
                  None,
                  TM.PatternNode.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (None, TM.PatternNode.Terminal None)
                        );
                        (
                          mkid "A2",
                          (None, TM.PatternNode.Terminal None)
                        );
                      ];
                  }
                )
              );
            ];
        }
      in
      assert_equal ~cmp:TM.PatternNode.equal expected_chain actual_chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A { A1, A2 }

      tuple (A, A)
  |} >:: test


let test_suite = "enum" >::: [
    test_build_chain_enum_1;
    test_build_chain_enum_2;
    test_build_chain_enum_3;    
]
