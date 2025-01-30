open Base
open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching

open Shared


let test_build_match_for_variant_single_nullary_constructor =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Unit))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value1 {
        A1() => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_field_wildcard =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Binder { identifier = mkid "x"; wildcard = true }))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value1 {
        A1(_) => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_wildcard =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value {
        x => read_register r1,
      }

    should become

      match value {
        A1 () => read_register r1
      }
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_field_binder =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Binder { identifier = mkid "x"; wildcard = false }))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [],
                    Ast.Statement.Let {
                      variable_identifier    = mkid "x";
                      binding_statement_type = Ast.Type.Unit;
                      binding_statement      = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
                      body_statement         = a1_statement;
                    }
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value1 {
        A1(x) => let x = () in read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_unary_constructor =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x"; wildcard = false } ))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x" ],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : int
      }

      match value1 {
        A1(x) => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_unary_constructor_field_wildcard =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x"; wildcard = true } ))
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x" ],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : int
      }

      match value1 {
        A1(_) => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_binary_constructor =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "y"; wildcard = false };
                  ]
                )
              )
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x"; mkid "y" ],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int)
      }

      match value1 {
        A1(x, y) => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_two_constructors =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [
          ("A1", [Ast.Type.Int; Ast.Type.Int]);
          ("A2", [Ast.Type.Int]);
        ]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and a2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "y"; wildcard = false };
                  ]
                )
              )
            ]
            a1_statement
            false
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A2",
                  Binder { identifier = mkid "x"; wildcard = false };
                )
              )
            ]
            a2_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x"; mkid "y" ],
                    a1_statement
                  )
                );
                (
                  mkid "A2",
                  (
                    [ mkid "x" ],
                    a2_statement
                  )
                );
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int),
        A2 : int
      }

      match value1 {
        A1(x, y) => read_register r1,
        A2(x)    => read_register r2
      }
  |} >:: test


let test_build_match_for_variant_nary_constructor_field_wildcards =
  let test _ =
    let tc =
      let* enum_type =
        define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "y"; wildcard = true  };
                  ]
                )
              )
            ]
            a1_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x"; mkid "y" ],
                    a1_statement
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int)
      }

      match value1 {
        A1(x, _) => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_nary_constructor_field_wildcards_unification =
  let test _ =
    let tc =
      let* enum_type_a =
        define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      and* enum_type_b =
        define_variant "B" [("B1", []); ("B2", [])]
      in
      let b1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and b2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type_a; enum_type_b ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "xxx"; wildcard = true };
                    Binder { identifier = mkid "y"; wildcard = false  };
                  ]
                )
              );
              Pattern.(
                VariantCase (
                  mkid "B1",
                  Unit
                )
              )
            ]
            b1_statement
            false
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "yyy"; wildcard = true  };
                  ]
                );
              );
              Pattern.(
                VariantCase (
                  mkid "B2",
                  Unit
                )
              )
            ]
            b2_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x"; mkid "y" ],
                    Ast.Statement.Match begin
                      Ast.Statement.MatchVariant {
                        matched = mkid "value2";
                        matched_type = mkid "B";
                        cases = Ast.Identifier.Map.of_alist_exn [
                            (
                              mkid "B1",
                              (
                                [],
                                b1_statement
                              )
                            );
                            (
                              mkid "B2",
                              (
                                [],
                                b2_statement
                              )
                            )
                          ]
                      }
                    end
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      union B = {
        B1 : unit,
        B2 : unit
      }

      match (value1, value2) {
        (A1(_, y), B1) => read_register r1,
        (A1(x, _), B2) => read_register r2
      }

    should become

      match value_1 {
        A1(x, y) => match value_2 {
                      B1 => read_register r1,
                      B2 => read_register r2
                    }
      }
  |} >:: test


let test_build_match_for_tuple_of_variants =
  let test _ =
    let tc =
      let* enum_type_a =
        define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      and* enum_type_b =
        define_variant "B" [("B1", []); ("B2", [])]
      in
      let b1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      and b2_statement =
        Ast.Statement.ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type_a; enum_type_b ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "y"; wildcard = false };
                  ]
                )
              );
              Pattern.(
                VariantCase (
                  mkid "B1",
                  Unit
                )
              )
            ]
            b1_statement
            false
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.(
                VariantCase (
                  mkid "A1",
                  Tuple [
                    Binder { identifier = mkid "x"; wildcard = false };
                    Binder { identifier = mkid "y"; wildcard = false };
                  ]
                )
              );
              Pattern.(
                VariantCase (
                  mkid "B2",
                  Unit
                )
              )
            ]
            b2_statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ mkid "x"; mkid "y" ],
                    Ast.Statement.Match begin
                      Ast.Statement.MatchVariant {
                        matched = mkid "value2";
                        matched_type = mkid "B";
                        cases = Ast.Identifier.Map.of_alist_exn [
                            (
                              mkid "B1",
                              (
                                [],
                                b1_statement
                              )
                            );
                            (
                              mkid "B2",
                              (
                                [],
                                b2_statement
                              )
                            )
                          ]
                      }
                    end
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      union B = {
        B1 : unit,
        B2 : unit
      }

      match (value1, value2) {
        (A1(x, y), B1 ()) => read_register r1,
        (A1(x, y), B2 ()) => read_register r2
      }

    should become

      match value_1 {
        A1(x, y) => match value_2 {
                      B1 () => read_register r1,
                      B2 () => read_register r2
                    }
      }
  |} >:: test


let test_build_match_for_tuple_of_variants_wildcards =
  let test _ =
    let genid = create_identifier_generator ()
    in
    let x1 = genid ()
    and x2 = genid ()
    and x3 = genid ()
    and x4 = genid ()
    and x5 = genid ()
    and x6 = genid ()
    in
    let tc =
      let* enum_type_a =
        define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_tuple_pattern_tree [ enum_type_a; enum_type_a; enum_type_a ]
        in
        let* pattern_tree = categorize
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.Binder { identifier = mkid "y"; wildcard = true };
              Pattern.Binder { identifier = mkid "z"; wildcard = true };
            ]
            statement
            false
        in
        TC.return pattern_tree
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ x1; x2 ],
                    Ast.Statement.Match begin
                      Ast.Statement.MatchVariant {
                        matched = mkid "value2";
                        matched_type = mkid "A";
                        cases = Ast.Identifier.Map.of_alist_exn [
                            (
                              mkid "A1",
                              (
                                [ x3; x4 ],
                                Ast.Statement.Match begin
                                  Ast.Statement.MatchVariant {
                                    matched = mkid "value3";
                                    matched_type = mkid "A";
                                    cases = Ast.Identifier.Map.of_alist_exn [
                                        (
                                          mkid "A1",
                                          (
                                            [ x5; x6 ],
                                            statement
                                          )
                                        )
                                      ]
                                  }
                                end                                
                              )
                            );
                          ]
                      }
                    end
                  )
                )
              ]
          }
        end
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
        ~cmp:Ast.Statement.equal
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      match (value1, value2, value3) {
        (_, _, _) => read_register r1,
      }

    should become

      match value1 {
        A1(x1, x2) => match value2 {
                        A1(x3, x4) => match value3 {
                                        A1(x5, x6) => read_register r1
                                      }
                      }
     }
  |} >:: test


let test_suite =
  "match generation" >::: [
    (* test_build_match_for_variant_single_nullary_constructor; *)
    (* test_build_match_for_variant_single_nullary_constructor_field_wildcard; *)
    (* test_build_match_for_variant_single_nullary_constructor_wildcard; *)
    (* test_build_match_for_variant_single_nullary_constructor_field_binder; *)
    (* test_build_match_for_variant_single_unary_constructor; *)
    (* test_build_match_for_variant_single_unary_constructor_field_wildcard; *)
    (* test_build_match_for_variant_single_binary_constructor; *)
    (* test_build_match_for_variant_two_constructors; *)
    (* test_build_match_for_variant_nary_constructor_field_wildcards; *)

    test_build_match_for_variant_nary_constructor_field_wildcards_unification;
    (* test_build_match_for_tuple_of_variants; *)
    (* test_build_match_for_tuple_of_variants_wildcards; *)
  ]
