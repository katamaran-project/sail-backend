open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open Shared


let test_adorn_enum_single_case =
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
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [ Pattern.EnumCase (mkid "A1") ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternTree.Terminal (Some a1_statement));
              );
            ];
        }
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
    enum A = { A1 }

    match a {
      A1 => read_register r1
    }
  |} >:: test


let test_adorn_enum_single_case_wildcard =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = true }
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (mkwild "x", TM.PatternTree.Terminal (Some a1_statement))
              );
            ];
        }
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
    enum A = { A1 }

    match a {
      _ => read_register r1
    }
  |} >:: test


let test_adorn_enum_single_case_binder =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = false }
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (mkbinder "x", TM.PatternTree.Terminal (Some a1_statement))
              );
            ];
        }
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
      enum A = { A1 }

      match a {
        x => read_register r1
      }

    should become

      match a {
        A1 => let x = a in read_register r1
      }
  |} >:: test


let test_adorn_enum_4 =
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
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (gen#wildcard, TM.PatternTree.Terminal (Some a1_statement))
              );
              (
                mkid "A2",
                (gen#wildcard, TM.PatternTree.Terminal (Some a2_statement))
              );
            ];
        }
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
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_adorn_enum_5 =
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
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,  (* no binder since pattern mentions A1 explicitly *)
                  TM.PatternTree.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  mkwild "x",  (* no binder since pattern is wildcard *)
                  TM.PatternTree.Terminal (Some a2_statement)
                )
              );
            ];
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternTree.to_fexpr)
        ~cmp:TM.PatternTree.equal
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


let test_adorn_enum_6 =
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
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false }
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,  (* no binder since pattern mentions "A1" explicitly *)
                  TM.PatternTree.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  mkbinder "x",  (* binder necessary because pattern requires the value to be bound to x *)
                  TM.PatternTree.Terminal (Some a2_statement)
                )
              );
            ];
        }
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
      let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
      in
      let* tree = adorn
          tree
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = false };
            Pattern.EnumCase (mkid "A1")
          ]
          a1_statement
      in
      let* _ = adorn
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
    test_adorn_enum_single_case;
    test_adorn_enum_single_case_wildcard;
    test_adorn_enum_single_case_binder;
    test_adorn_enum_4;
    test_adorn_enum_5;
    test_adorn_enum_6;

    test_failure_due_to_clashing_binders;
  ]
