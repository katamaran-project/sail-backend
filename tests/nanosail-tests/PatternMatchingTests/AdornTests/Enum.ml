open OUnit2
open Nanosail

module BuildTreeTests = BuildTreeTests


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open PatternMatchingShared
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
      let* actual_tree =
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
                (gen#wildcard, TM.PatternTree.Leaf (Some a1_statement));
              );
            ];
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [
            Binder gen#wildcard
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Leaf (Some a1_statement)
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ enum_type ]
        in
        let* tree = adorn
          tree
          [
            Binder { identifier = mkid "x"; wildcard = false }
          ]
          a1_statement
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = mkbinder "x";
          subtree      = Leaf (Some a1_statement)
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
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
                (gen#wildcard, TM.PatternTree.Leaf (Some a1_statement))
              );
              (
                mkid "A2",
                (gen#wildcard, TM.PatternTree.Leaf (Some a2_statement))
              );
            ];
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
              EnumCase (mkid "A1")
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Binder gen#wildcard
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
                  TM.PatternTree.Leaf (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  gen#wildcard,
                  TM.PatternTree.Leaf (Some a2_statement)
                )
              );
            ];
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
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
              Binder { identifier = mkid "x"; wildcard = false }
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
                  TM.PatternTree.Leaf (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  mkbinder "x",  (* binder necessary because pattern requires the value to be bound to x *)
                  TM.PatternTree.Leaf (Some a2_statement)
                )
              );
            ];
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Binder gen#wildcard;
              EnumCase (mkid "A1");
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Binder gen#wildcard;
              EnumCase (mkid "A2");
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
                      TM.PatternTree.Leaf (Some a1_statement)
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Leaf (Some a2_statement)
                    )
                  );
                ];
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Binder { identifier = mkid "x"; wildcard = false };
              EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Binder { identifier = mkid "y"; wildcard = false };
              EnumCase (mkid "A2")
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
                      TM.PatternTree.Leaf (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Leaf (Some (a2_statement genid))
                    )
                  );
                ];
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
            Value (Ast.Value.mk_int 1)
          )
        end
      in
      let a2_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Value (Ast.Value.mk_int 2)
          )
        end
      in
      let* actual_tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "x");
              EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "y");
              EnumCase (mkid "A2")
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
                      TM.PatternTree.Leaf (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      gen#wildcard,
                      TM.PatternTree.Leaf (Some (a2_statement genid))
                    )
                  );
                ];
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
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
            Value (Ast.Value.mk_int 1)
          )
        end
      in
      let a2_statement id : Ast.Statement.t =
        Expression begin
          BinaryOperation (
            Plus,
            Variable (id, Ast.Type.Int),
            Value (Ast.Value.mk_int 2)
          )
        end
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ Ast.Type.Int; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "x");
              EnumCase (mkid "A1")
            ]
            (a1_statement (mkid "x"))
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "y");
              Binder gen#wildcard;
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
                      TM.PatternTree.Leaf (Some (a1_statement genid))
                    )
                  );
                  (
                    mkid "A2",
                    (
                      wildcard_a2_a3,
                      TM.PatternTree.Leaf (Some (a2_statement genid))
                    )
                  );
                  (
                    mkid "A3",
                    (
                      wildcard_a2_a3, (* must be same wildcard as in A2 case, since when a pattern tree node is expanded, subtrees are copied *)
                      TM.PatternTree.Leaf (Some (a2_statement genid))
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


let test_clashing_binders_4 =
  let test _ =
    skip_if true "clashing binder failure";
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let statement n : Ast.Statement.t =
        Expression (Value (Ast.Value.mk_int n))
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              EnumCase (mkid "A1");
              EnumCase (mkid "A1");
            ]
            (statement 1)
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "x");
              EnumCase (mkid "A1");
            ]
            (statement 2)
        in
        let* tree = adorn
            tree
            [
              Binder (mkbinder "y");
              Binder (mkwild "z");
            ]
            (statement 3)
        in
        TC.return tree
      in
      let expected_tree : TM.PatternTree.t =
        TM.PatternTree.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                (
                  gen#wildcard,
                  TM.PatternTree.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (gen#wildcard, TM.PatternTree.Leaf (Some (statement 1)))
                        );
                        (
                          mkid "A2",
                          (gen#wildcard, TM.PatternTree.Leaf (Some (statement 3)))
                        );
                      ]
                  }
                )
              );
              (
                mkid "A2",
                (
                  gen#wildcard,
                  TM.PatternTree.Enum {
                    enum_identifier = mkid "A";
                    table = Ast.Identifier.Map.of_alist_exn [
                        (
                          mkid "A1",
                          (gen#wildcard, TM.PatternTree.Leaf (Some (statement 2)))
                        );
                        (
                          mkid "A2",
                          (gen#wildcard, TM.PatternTree.Leaf (Some (statement 3)))
                        );
                      ]
                  }
                )
              )
            ]
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

      match pair {
        (A1, A1) => 1,
        (x , A1) => 2,
        (y , _ ) => 3
      }

    should become

      match pair {
        (A1, A1) => 1,
        (A2, A1) => 2,
        (A1, A2) => 3,
        (A2, A2) => 3
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
    test_clashing_binders_4;
  ]
