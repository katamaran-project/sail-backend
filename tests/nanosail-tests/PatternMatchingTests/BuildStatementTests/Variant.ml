open OUnit2
open Nanosail


module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open PatternMatchingShared


let test_build_match_for_variant_single_nullary_constructor =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Unit))
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [ gen#id ],
                    a1_statement
                  )
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
      union A = {
        A1 : unit
      }

      match value1 {
        A1() => read_register r1,
      }
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_field_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.VariantCase (mkid "A1", Pattern.Binder gen#wildcard)
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [gen#id],
                    a1_statement
                  )
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
      union A = {
        A1 : unit
      }

      match value1 {
        A1(_) => read_register r1,
      }

    should become

      match value {
        A1(_) => read_register r1
      }
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_wildcard =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder gen#wildcard
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        a1_statement
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value {
        _ => read_register r1,
      }

    should become

      read_register r1
  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_binder =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder (mkbinder "x")
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match 
        [mkid "value"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder = mkid "x";
          binding_statement_type = variant_type;
          binding_statement      = Expression (Variable (mkid "value", variant_type));
          body_statement         = a1_statement;
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : unit
      }

      match value {
        x => read_register r1,
      }

    should become

        let x = value in read_register r1

  |} >:: test


let test_build_match_for_variant_single_nullary_constructor_field_binder =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Pattern.Binder { identifier = mkid "x"; wildcard = false }))
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Ast.Statement.Match begin
          Ast.Statement.MatchVariant {
            matched = mkid "value1";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  (
                    [gen#id],
                    Ast.Statement.Let {
                      binder = mkid "x";
                      binding_statement_type = Ast.Type.Unit;
                      binding_statement      = Expression (Ast.Expression.Value Ast.Value.Unit);
                      body_statement         = a1_statement;
                    }
                  )
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
      let* variant_type =
        TC.define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x"; wildcard = false } ))
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
      let* variant_type =
        TC.define_variant "A" [("A1", [Ast.Type.Int])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.(VariantCase (mkid "A1", Binder { identifier = mkid "x"; wildcard = true } ))
            ]
            a1_statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
      let* variant_type =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
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
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
      let* variant_type =
        TC.define_variant "A" [
          ("A1", [Ast.Type.Int; Ast.Type.Int]);
          ("A2", [Ast.Type.Int]);
        ]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and a2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
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
        in
        let* pattern_tree = adorn
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
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
      let* variant_type =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let a1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type ]
        in
        let* pattern_tree = adorn
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
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
    let gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      and* variant_type_b =
        TC.define_variant "B" [("B1", []); ("B2", [])]
      in
      let b1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and b2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_b ]
        in
        let* pattern_tree = adorn
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
        in
        let* pattern_tree = adorn
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
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
                                [gen#id],
                                b1_statement
                              )
                            );
                            (
                              mkid "B2",
                              (
                                [gen#id],
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
    let gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      and* variant_type_b =
        TC.define_variant "B" [("B1", []); ("B2", [])]
      in
      let b1_statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      and b2_statement : Ast.Statement.t =
        ReadRegister (mkid "r2")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_b ]
        in
        let* pattern_tree = adorn
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
        in
        let* pattern_tree = adorn
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
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
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
                                [gen#id],
                                b1_statement
                              )
                            );
                            (
                              mkid "B2",
                              (
                                [gen#id],
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
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
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
    let _gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_a; variant_type_a ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.Binder { identifier = mkid "y"; wildcard = true };
              Pattern.Binder { identifier = mkid "z"; wildcard = true };
            ]
            statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        statement
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      match (value1, value2, value3) {
        (_, _, _) => read_register r1,
      }

    should become

      read_register r1
  |} >:: test


let test_build_match_for_tuple_of_variants_binders =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let* variant_type =
        TC.define_variant "A" [("A1", [Ast.Type.Int; Ast.Type.Int])]
      in
      let statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type; variant_type; variant_type ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.Binder { identifier = mkid "y"; wildcard = false };
              Pattern.Binder { identifier = mkid "z"; wildcard = false };
            ]
            statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder = mkid "x";
          binding_statement_type = variant_type;
          binding_statement      = Expression (Variable (mkid "value1", variant_type));
          body_statement         = Let {
              binder = mkid "y";
              binding_statement_type = variant_type;
              binding_statement      = Expression (Variable (mkid "value2", variant_type));
              body_statement         = Let {
                  binder = mkid "z";
                  binding_statement_type = variant_type;
                  binding_statement      = Expression (Variable (mkid "value3", variant_type));
                  body_statement         = statement
                }
            }
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      match (value1, value2, value3) {
        (x, y, z) => read_register r1,
      }

    should become

      let x = value1 in let y = value2 in let z = value3 in read_register r1
  |} >:: test


let test_build_match_for_tuple_of_variants_binders_2 =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [
          ("A1", [Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_b =
        TC.define_variant "B" [
          ("B1", []);
          ("B2", [Ast.Type.Int; Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_c =
        TC.define_variant "C" [
          ("C1", [Ast.Type.Int]);
          ("C2", [Ast.Type.Int]);
          ("C3", [Ast.Type.Int]);
        ]
      in
      let statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_b; variant_type_c ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.Binder { identifier = mkid "y"; wildcard = false };
              Pattern.Binder { identifier = mkid "z"; wildcard = false };
            ]
            statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder = mkid "x";
          binding_statement_type = variant_type_a;
          binding_statement      = Expression (Variable (mkid "value1", variant_type_a));
          body_statement         = Let {
              binder = mkid "y";
              binding_statement_type = variant_type_b;
              binding_statement      = Expression (Variable (mkid "value2", variant_type_b));
              body_statement         = Let {
                  binder = mkid "z";
                  binding_statement_type = variant_type_c;
                  binding_statement      = Expression (Variable (mkid "value3", variant_type_c));
                  body_statement         = statement
                }
            }
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      union B = {
        B1 : unit,
        B2 : (int, int, int)
      }

      union C = {
        C1 : int,
        C2 : int,
        C3 : int
      }

      match (A_value, B_value, C_value) {
        (x, y, z) => read_register r1,
      }

    should become

      let x = A_value in let y = B_value in let z = C_value in read_register r1
  |} >:: test


let test_build_match_for_tuple_of_variants_binders_3 =
  let test _ =
    let _gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [
          ("A1", [Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_b =
        TC.define_variant "B" [
          ("B1", []);
          ("B2", [Ast.Type.Int; Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_c =
        TC.define_variant "C" [
          ("C1", [Ast.Type.Int]);
          ("C2", [Ast.Type.Int]);
          ("C3", [Ast.Type.Int]);
        ]
      in
      let statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_b; variant_type_c ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.Binder { identifier = mkid "y"; wildcard = true };
              Pattern.Binder { identifier = mkid "z"; wildcard = false };
            ]
            statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder = mkid "x";
          binding_statement_type = variant_type_a;
          binding_statement      = Expression (Variable (mkid "value1", variant_type_a));
          body_statement         = Let {
              binder = mkid "z";
              binding_statement_type = variant_type_c;
              binding_statement      = Expression (Variable (mkid "value3", variant_type_c));
              body_statement         = statement
            }
        }
      in
      TC.assert_equal_statements expected_match_statement actual_match_statement
    in
    TC.run_expecting_success tc
  in
  {|
      union A = {
        A1 : (int, int),
      }

      union B = {
        B1 : unit,
        B2 : (int, int, int)
      }

      union C = {
        C1 : int,
        C2 : int,
        C3 : int
      }

      match (A_value, B_value, C_value) {
        (x, _, z) => read_register r1,
      }

    should become

      let x = A_value in let z = C_value in read_register r1
  |} >:: test


let test_build_match_for_tuple_of_variants_binders_4 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* variant_type_a =
        TC.define_variant "A" [
          ("A1", [Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_b =
        TC.define_variant "B" [
          ("B1", []);
          ("B2", [Ast.Type.Int; Ast.Type.Int; Ast.Type.Int])
        ]
      and* variant_type_c =
        TC.define_variant "C" [
          ("C1", [Ast.Type.Int]);
          ("C2", [Ast.Type.Int]);
          ("C3", [Ast.Type.Int]);
        ]
      in
      let statement : Ast.Statement.t =
        ReadRegister (mkid "r1")
      in
      let* pattern_tree =
        let* pattern_tree = build_empty_pattern_tree [ variant_type_a; variant_type_b; variant_type_c ]
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.VariantCase ( mkid "B1", Pattern.Unit );
              Pattern.Binder { identifier = mkid "z"; wildcard = false };
            ]
            statement
        in
        let* pattern_tree = adorn
            pattern_tree
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.Binder { identifier = mkid "y"; wildcard = true };
              Pattern.Binder { identifier = mkid "z"; wildcard = false };
            ]
            statement
        in
        TC.return pattern_tree
      in
      let* actual_match_statement : Ast.Statement.t =
        build_match [mkid "value1"; mkid "value2"; mkid "value3"] pattern_tree
      in
      let expected_match_statement : Ast.Statement.t =
        Let {
          binder = mkid "x";
          binding_statement_type = variant_type_a;
          binding_statement      = Expression (Variable (mkid "value1", variant_type_a));
          body_statement         = Ast.Statement.Match begin
              Ast.Statement.MatchVariant {
                matched = mkid "value2";
                matched_type = mkid "B";
                cases = Ast.Identifier.Map.of_alist_exn [
                    (
                      mkid "B1",
                      (
                        [gen#id],
                        Ast.Statement.Let {
                          binder = mkid "z";
                          binding_statement_type = variant_type_c;
                          binding_statement      = Expression (Variable (mkid "value3", variant_type_c));
                          body_statement         = statement
                        }
                      )
                    );
                    (
                      mkid "B2",
                      (
                        [gen#id; gen#id; gen#id],
                        Ast.Statement.Let {
                          binder = mkid "z";
                          binding_statement_type = variant_type_c;
                          binding_statement      = Expression (Variable (mkid "value3", variant_type_c));
                          body_statement         = statement
                        }
                      )
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
      union A = {
        A1 : (int, int),
      }

      union B = {
        B1 : unit,
        B2 : (int, int, int)
      }

      union C = {
        C1 : int,
        C2 : int,
        C3 : int
      }

      match (A_value, B_value, C_value) {
        (x, B1 (), z) => read_register r1,
        (x, _, z)     => read_register r2,
      }

    should become

      let x = A_value in
        match B_value {
          B1 ()      => let z = C_value in read_register r1
          B2 (_;_;_) => let z = C_value in read_register r2
        }
  |} >:: test


let test_suite =
  "variant" >::: [
    test_build_match_for_variant_single_nullary_constructor;
    test_build_match_for_variant_single_nullary_constructor_field_wildcard;
    test_build_match_for_variant_single_nullary_constructor_wildcard;
    test_build_match_for_variant_single_nullary_constructor_binder;
    test_build_match_for_variant_single_nullary_constructor_field_binder;
    test_build_match_for_variant_single_unary_constructor;
    test_build_match_for_variant_single_unary_constructor_field_wildcard;
    test_build_match_for_variant_single_binary_constructor;
    test_build_match_for_variant_two_constructors;
    test_build_match_for_variant_nary_constructor_field_wildcards;
    test_build_match_for_variant_nary_constructor_field_wildcards_unification;
    test_build_match_for_tuple_of_variants;
    test_build_match_for_tuple_of_variants_wildcards;
    test_build_match_for_tuple_of_variants_binders;
    test_build_match_for_tuple_of_variants_binders_2;
    test_build_match_for_tuple_of_variants_binders_3;
    test_build_match_for_tuple_of_variants_binders_4;
  ]
