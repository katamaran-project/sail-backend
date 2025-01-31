open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_match_for_enum_1 =
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
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  a1_statement
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

      match value1 {
        A1 => read_register r1,
      }
  |} >:: test


let test_build_match_for_enum_2 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and a2_statement =
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
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  a1_statement
                );
                (
                  mkid "A2",
                  a2_statement
                );
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
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_3 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and a2_statement =
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
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  a1_statement
                );
                (
                  mkid "A2",
                  a2_statement
                );
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
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_4 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and a2_statement =
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
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false }
            ]
            a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  a1_statement
                );
                (
                  mkid "A2",
                  Ast.Statement.Let {
                    variable_identifier    = mkid "x";
                    binding_statement_type = Ast.Type.Enum (mkid "A");
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Enum (mkid "A")));
                    body_statement         = a2_statement
                  }
                );
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
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        x  => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_5 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement =
        mkstm 1
      and a1_a2_statement =
        mkstm 2
      and a2_a1_statement =
        mkstm 3
      and a2_a2_statement =
        mkstm 4
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a1_a1_statement
                          );
                          (
                            mkid "A2",
                            a1_a2_statement
                          );
                        ]
                    }
                  end
                );
                (
                  mkid "A2",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a2_a1_statement
                          );
                          (
                            mkid "A2",
                            a2_a2_statement
                          );
                        ]
                    }
                  end
                );
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
      enum A = { A1, A2 }

      match value1, value2 {
        A1, A1 => r1,
        A1, A2 => r2,
        A2, A1 => r3,
        A2, A2 => r4,
      }
  |} >:: test


let test_build_match_for_enum_6 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement =
        mkstm 1
      and a1_a2_statement =
        mkstm 2
      and a2_a1_statement =
        mkstm 3
      and a2_a2_statement =
        mkstm 4
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a1_a1_statement
                          );
                          (
                            mkid "A2",
                            a1_a2_statement
                          );
                        ]
                    }
                  end
                );
                (
                  mkid "A2",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a2_a1_statement
                          );
                          (
                            mkid "A2",
                            a2_a2_statement
                          );
                        ]
                    }
                  end
                );
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
      enum A = { A1, A2 }

      match value1, value2 {
        A1, A1 => r1,
        A1, A2 => r2,
        _ , A1 => r3,
        _ , A2 => r4,
      }
  |} >:: test


let test_build_match_for_enum_7 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement =
        mkstm 1
      and a1_a2_statement =
        mkstm 2
      and a2_a1_statement =
        mkstm 1
      and a2_a2_statement =
        mkstm 2
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a1_a1_statement
                          );
                          (
                            mkid "A2",
                            a1_a2_statement
                          );
                        ]
                    }
                  end
                );
                (
                  mkid "A2",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "value2";
                      matched_type = mkid "A";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "A1",
                            a2_a1_statement
                          );
                          (
                            mkid "A2",
                            a2_a2_statement
                          );
                        ]
                    }
                  end
                );
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
      enum A = { A1, A2 }

      match value1, value2 {
        _, A1 => r1,
        _, A2 => r2,
      }
  |} >:: test



(*
   is technically wrong, adds a superfluous let
   todo fix test and algorithm
*)
let test_build_match_for_enum_8 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement =
        mkstm 1
      and a1_a2_statement =
        mkstm 2
      and a2_a1_statement =
        mkstm 1
      and a2_a2_statement =
        mkstm 2
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Let {
                      variable_identifier = mkid "x";
                      binding_statement_type = Ast.Type.Enum (mkid "A");
                      binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Enum (mkid "A")));
                      body_statement         = Ast.Statement.Match begin
                          Ast.Statement.MatchEnum {
                            matched      = mkid "value2";
                            matched_type = mkid "A";
                            cases        = Ast.Identifier.Map.of_alist_exn [
                                (
                                  mkid "A1",
                                  a1_a1_statement
                                );
                                (
                                  mkid "A2",
                                  a1_a2_statement
                                );
                              ]
                          }
                        end
                    }
                );
                (
                  mkid "A2",
                  Ast.Statement.Let {
                      variable_identifier    = mkid "x";
                      binding_statement_type = Ast.Type.Enum (mkid "A");
                      binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Enum (mkid "A")));
                      body_statement         = Ast.Statement.Match begin
                          Ast.Statement.MatchEnum {
                            matched = mkid "value2";
                            matched_type = mkid "A";
                            cases = Ast.Identifier.Map.of_alist_exn [
                                (
                                  mkid "A1",
                                  a2_a1_statement
                                );
                                (
                                  mkid "A2",
                                  a2_a2_statement
                                );
                              ]
                          }
                        end
                    }
                );
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
      enum A = { A1, A2 }

      match value1, value2 {
        x, A1 => r1,
        _, A2 => r2,
      }
  |} >:: test


let test_build_match_for_enum_9 =
  let test _ =
    let tc =
      let* enum_type_a =
        define_enum_str "A" ["A1"; "A2"]
      and* enum_type_b =
        define_enum_str "B" ["B1"; "B2"]
      in
      let a1_b1_statement =
        mkstm 1
      and a1_b2_statement =
        mkstm 2
      and a2_b1_statement =
        mkstm 3
      and a2_b2_statement =
        mkstm 4
      in
      let* tree =
        let* tree = build_tuple_pattern_tree [ enum_type_a; enum_type_b ]
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "B1");
            ]
            a1_b1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "B2");
            ]
            a1_b2_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "B1");
            ]
            a2_b1_statement
            false
        in
        let* tree = categorize
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "B2");
            ]
            a2_b2_statement
            false
        in
        TC.return tree
      in
      let* actual_match_statement =
        build_match [mkid "a"; mkid "b"] tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "a";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "b";
                      matched_type = mkid "B";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "B1",
                            a1_b1_statement
                          );
                          (
                            mkid "B2",
                            a1_b2_statement
                          );
                        ]
                    }
                  end
                );
                (
                  mkid "A2",
                  Ast.Statement.Match begin
                    Ast.Statement.MatchEnum {
                      matched = mkid "b";
                      matched_type = mkid "B";
                      cases = Ast.Identifier.Map.of_alist_exn [
                          (
                            mkid "B1",
                            a2_b1_statement
                          );
                          (
                            mkid "B2",
                            a2_b2_statement
                          );
                        ]
                    }
                  end
                );
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
      enum A = { A1, A2 }
      enum B = { B1, B2 }

      match a, b {
        A1, B1 => r1,
        A1, B2 => r2,
        A2, B1 => r3,
        A2, B2 => r4
      }
  |} >:: test


let test_suite =
  "match generation" >::: [
    test_build_match_for_enum_1;
    test_build_match_for_enum_2;
    test_build_match_for_enum_3;
    test_build_match_for_enum_4;
    test_build_match_for_enum_5;
    test_build_match_for_enum_6;
    test_build_match_for_enum_7;
    test_build_match_for_enum_8;
    test_build_match_for_enum_9;
  ]
