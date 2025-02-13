open Base
open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open Shared
open Monads.Notations.Star(TC)


let test_adorn_enum_single_case =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [ EnumCase (mkid "A1") ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Enum {
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
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
    enum A = { A1 }

    match a {
      A1 => read_register r1
    }
  |} >:: test


let test_adorn_enum_single_case_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [
            Pattern.Binder gen#wildcard
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Terminal (Some a1_statement)
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
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
        TC.define_enum_str "A" ["A1"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
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
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = mkbinder "x";
          subtree      = Terminal (Some a1_statement)
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
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


let test_adorn_enum_two_cases =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let a2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
            tree
            [
              EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              EnumCase (mkid "A2")
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
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
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_adorn_enum_two_cases_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let a2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
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
              Pattern.Binder gen#wildcard
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,
                  TM.PatternTree.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  gen#wildcard,
                  TM.PatternTree.Terminal (Some a2_statement)
                )
              );
            ];
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree actual_tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_adorn_enum_two_cases_binder =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let a2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
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
      let expected_tree : TM.PatternTree.t =
        Enum {
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
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match a {
        A1 => read_register r1,
        x  => read_register r2,
      }
  |} >:: test


let test_adorn_enum_two_cases_pair_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let a2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder gen#wildcard;
              Pattern.EnumCase (mkid "A1");
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder gen#wildcard;
              Pattern.EnumCase (mkid "A2");
            ]
            a2_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Enum {
              enum_identifier = mkid "A";
              table = Ast.Identifier.Map.of_alist_exn [
                  (
                    mkid "A1",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some a1_statement)
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some a2_statement)
                    )
                  );
                ];
            }
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match (a1, a2) {
        x, A1 => read_register r1,
        x, A2 => read_register r2,
      }
  |} >:: test


let test_clashing_binders =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let genid =
        gen#id
      in
      let a1_statement id : Ast.Statement.t =
        Expression (Variable (id, Int))
      in
      let a2_statement id : Ast.Statement.t =
        Expression (Variable (id, Int))
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "y"; wildcard = false };
              Pattern.EnumCase (mkid "A2")
            ]
            (a2_statement (mkid "y"))
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = { identifier = genid; wildcard = false };
          subtree      = Enum {
              enum_identifier = mkid "A";
              table = Ast.Identifier.Map.of_alist_exn [
                  (
                    mkid "A1",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some (a2_statement genid))
                    )
                  );
                ];
            }
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match (value1, value2) {
        (x, A1) => x,
        (y, A2) => y,
      }

    should become

      match (value1, value2) {
        (genid, A1) => genid,
        (genid, A2) => genid,
      }
  |} >:: test


let test_clashing_binders_2 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let genid =
        gen#id
      in
      let a1_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Val (Ast.Value.mk_int 1)
          )
        end
      in
      let a2_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Val (Ast.Value.mk_int 2)
          )
        end
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder (mkbinder "x");
              Pattern.EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder (mkbinder "y");
              Pattern.EnumCase (mkid "A2")
            ]
            (a2_statement (mkid "y"))
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = Ast.Type.Int;
          binder       = { identifier = genid; wildcard = false };
          subtree      = Enum {
              enum_identifier = mkid "A";
              table = Ast.Identifier.Map.of_alist_exn [
                  (
                    mkid "A1",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some (a2_statement genid))
                    )
                  );
                ];
            }
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternTree.to_fexpr)
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match (value1, value2) {
        (x, A1) => x + 1,
        (y, A2) => y + 2,
      }

    should become

      match (value1, value2) {
        (genid, A1) => genid + 1,
        (genid, A2) => genid + 2,
      }
  |} >:: test


let test_clashing_binders_3 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"; "A3"]
      in
      let genid =
        gen#id
      in
      let a1_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Val (Ast.Value.mk_int 1)
          )
        end
      in
      let a2_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Val (Ast.Value.mk_int 2)
          )
        end
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder (mkbinder "x");
              Pattern.EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder (mkbinder "y");
              Pattern.Binder gen#wildcard;
            ]
            (a2_statement (mkid "y"))
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        let wildcard_a2_a3 = gen#wildcard
        in
        Binder {
          matched_type = Ast.Type.Int;
          binder       = { identifier = genid; wildcard = false };
          subtree      = Enum {
              enum_identifier = mkid "A";
              table = Ast.Identifier.Map.of_alist_exn [
                  (
                    mkid "A1",
                    (
                      gen#wildcard,
                      TM.PatternTree.Terminal (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      wildcard_a2_a3,
                      TM.PatternTree.Terminal (Some (a2_statement genid))
                    )
                  );
                  (
                    mkid "A3",
                    (
                      wildcard_a2_a3, (* must be same wildcard as in A2 case, since when a pattern tree node is expanded, subtrees are copied *)
                      TM.PatternTree.Terminal (Some (a2_statement genid))
                    )
                  );
                ];
            }
        }
      in
      assert_equal
        ~cmp:TM.PatternTree.equal
        ~pp_diff:(pp_diff TM.PatternTree.to_fexpr)
        (Normalize.normalize_pattern_tree expected_tree)
        (Normalize.normalize_pattern_tree tree);
      TC.return ()
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2, A3 }

      match (value1, value2) {
        (x, A1) => x + 1,
        (y, _ ) => y + 2
      }

    should become

      match (value1, value2) {
        (genid, A1) => genid + 1,
        (genid, A2) => genid + 2,
        (genid, A3) => genid + 2,
      }
  |} >:: test


let test_suite =
  "enum" >::: [
    test_adorn_enum_single_case;
    test_adorn_enum_single_case_wildcard;
    test_adorn_enum_single_case_binder;
    test_adorn_enum_two_cases;
    test_adorn_enum_two_cases_wildcard;
    test_adorn_enum_two_cases_binder;
    test_adorn_enum_two_cases_pair_wildcard;

    test_clashing_binders;
    test_clashing_binders_2;
    test_clashing_binders_3;
  ]
