open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_categorize_enum_1 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
          tree
          [ Pattern.EnumCase (mkid "A1") ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternNode.Terminal (Some a1_statement));
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
    enum A = { A1 }

    match a {
      A1 => read_register r1
    }
  |} >:: test


let test_categorize_enum_2 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = true }
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (mkwild "x", TM.PatternNode.Terminal (Some a1_statement))
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
    enum A = { A1 }

    match a {
      _ => read_register r1
    }
  |} >:: test


let test_categorize_enum_3 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = false }
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (mkbinder "x", TM.PatternNode.Terminal (Some a1_statement))
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A = { A1 }

      match a {
        x => read_register r1
      }

    should become

      match a {
        A1 => let x = a in read_register r1
      }
  |} >:: test


let test_categorize_enum_4 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let a2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternNode.Terminal (Some a1_statement))
              );
              (
                mkid "A2",
                (gen#wildcard, TM.PatternNode.Terminal (Some a2_statement))
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_categorize_enum_5 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let a2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* actual_tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,  (* no binder since pattern mentions A1 explicitly *)
                  TM.PatternNode.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  mkwild "x",  (* no binder since pattern is wildcard *)
                  TM.PatternNode.Terminal (Some a2_statement)
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

      match a {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_categorize_enum_6 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let a2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false }
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,  (* no binder since pattern mentions "A1" explicitly *)
                  TM.PatternNode.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  mkbinder "x",  (* binder necessary because pattern requires the value to be bound to x *)
                  TM.PatternNode.Terminal (Some a2_statement)
                )
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        x  => read_register r2,
      }
  |} >:: test


let test_failure_due_to_clashing_binders =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let a2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* tree = build_tuple_pattern_tree [ enum_type; enum_type ]
      in
      let* tree = categorize
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = false };
            Pattern.EnumCase (mkid "A1")
          ]
          a1_statement
      in
      let* _ = categorize
          tree
          [
            Pattern.Binder { identifier = mkid "y"; wildcard = false };
            Pattern.EnumCase (mkid "A2")
          ]
          a2_statement
      in
      TC.return ()
    in
    run_failing_tc tc
  in
  {|
      enum A = { A1, A2 }

      match (value1, value2) {
        (x, A1) => read_register r1,
        (y, A2) => read_register r2,
      }

    should fail
  |} >:: test


let test_suite =
  "enum" >::: [
    test_categorize_enum_1;
    test_categorize_enum_2;
    test_categorize_enum_3;
    test_categorize_enum_4;
    test_categorize_enum_5;
    test_categorize_enum_6;

    test_failure_due_to_clashing_binders;
  ]
