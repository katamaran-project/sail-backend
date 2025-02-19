open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open PatternMatchingShared


let test_build_match_for_enum_with_single_case =
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
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1 }

      match value1 {
        A1 => read_register r1,
      }
  |} >:: test


let test_build_match_for_enum_with_two_cases =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and a2_statement : Ast.Statement.t =
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
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_with_two_cases_use_wildcard =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and a2_statement : Ast.Statement.t =
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
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_with_two_cases_use_binder =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and a2_statement : Ast.Statement.t =
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
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
                    binder                 = mkid "x";
                    binding_statement_type = Ast.Type.Enum (mkid "A");
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Enum (mkid "A")));
                    body_statement         = a2_statement
                  }
                );
              ]
          }
        end
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      match value1 {
        A1 => read_register r1,
        x  => read_register r2,
      }
  |} >:: test


let test_build_match_for_pair_of_enums =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement : Ast.Statement.t =
        mkstm 1
      and a1_a2_statement : Ast.Statement.t =
        mkstm 2
      and a2_a1_statement : Ast.Statement.t =
        mkstm 3
      and a2_a2_statement : Ast.Statement.t =
        mkstm 4
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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


let test_build_match_for_pair_of_enums_with_wildcards_for_first_value =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_a1_statement : Ast.Statement.t =
        mkstm 1
      and a1_a2_statement : Ast.Statement.t =
        mkstm 2
      and a2_a1_statement : Ast.Statement.t =
        mkstm 3
      and a2_a2_statement : Ast.Statement.t =
        mkstm 4
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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


let test_build_match_for_pair_of_enums_with_wildcards_for_first_value_2 =
  let test _ =
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        mkstm 1
      and a2_statement : Ast.Statement.t =
        mkstm 2
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
            matched      = mkid "value2";
            matched_type = mkid "A";
            cases        = Ast.Identifier.Map.of_alist_exn [
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let a1_statement : Ast.Statement.t =
        mkstm 1
      and a2_statement : Ast.Statement.t =
        mkstm 2
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type; enum_type ]
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder                 = mkid "x";
          binding_statement_type = enum_type;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", enum_type));
          body_statement         = Ast.Statement.Match begin
              Ast.Statement.MatchEnum {
                matched      = mkid "value2";
                matched_type = mkid "A";
                cases        = Ast.Identifier.Map.of_alist_exn [
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
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
        TC.define_enum_str "A" ["A1"; "A2"]
      and* enum_type_b =
        TC.define_enum_str "B" ["B1"; "B2"]
      in
      let a1_b1_statement : Ast.Statement.t =
        mkstm 1
      and a1_b2_statement : Ast.Statement.t =
        mkstm 2
      and a2_b1_statement : Ast.Statement.t =
        mkstm 3
      and a2_b2_statement : Ast.Statement.t =
        mkstm 4
      in
      let* tree =
        let* tree = build_empty_pattern_tree [ enum_type_a; enum_type_b ]
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "B1");
            ]
            a1_b1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "B2");
            ]
            a1_b2_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "B1");
            ]
            a2_b1_statement
        in
        let* tree = adorn
            tree
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "B2");
            ]
            a2_b2_statement
        in
        TC.return tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "a"; mkid "b"] tree
      in
      let expected_match_statement : Ast.Statement.t =
        Match begin
          MatchEnum {
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
  "enum" >::: [
    test_build_match_for_enum_with_single_case;
    test_build_match_for_enum_with_two_cases;
    test_build_match_for_enum_with_two_cases_use_wildcard;
    test_build_match_for_enum_with_two_cases_use_binder;
    test_build_match_for_pair_of_enums;
    test_build_match_for_pair_of_enums_with_wildcards_for_first_value;
    test_build_match_for_pair_of_enums_with_wildcards_for_first_value_2;
    test_build_match_for_enum_8;
    test_build_match_for_enum_9;
  ]
