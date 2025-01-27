open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching


let dummy_location : Libsail.Ast.l =
  Libsail.Parse_ast.Unknown

let mkid = Ast.Identifier.mk


let define_enum
    (identifier : Ast.Identifier.t     )
    (cases      : Ast.Identifier.t list) : Ast.Type.t TC.t
  =
  let enum_definition : Ast.Definition.Type.Enum.t =
    {
      identifier;
      cases
    }
  in
  let definition =
    Ast.Definition.TypeDefinition (Ast.Definition.Type.Enum enum_definition)
  in
  let* () = TC.store_definition definition
  in
  TC.return @@ Ast.Type.Enum identifier


let define_enum_str
    (identifier : string     )
    (cases      : string list) : Ast.Type.t TC.t
  =
  let identifier = Ast.Identifier.mk identifier
  and cases      = List.map ~f:Ast.Identifier.mk cases
  in
  define_enum identifier cases


let run_tc (tc : 'a TC.t) : 'a =
  let result, _ = TC.run tc
  in
  match result with
  | TC.Success result -> result
  | TC.Failure error  -> begin
      let error_message =
        Printf.sprintf "execution of TC resulted in failure: %s" @@ TC.Error.to_string error
      in
      assert_failure error_message
    end


let build_tuple_pattern_chain = TM.build_tuple_pattern_chain dummy_location
let categorize                = TM.categorize_case dummy_location
let build_match               = TM.build_leveled_match_statements

let test_build_chain_enum_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* chain =
        build_tuple_pattern_chain [ enum_type ]
      in
      match chain with
      | Enum { enum_identifier; table; binder_identifier } -> begin
          assert_equal ~cmp:Ast.Identifier.equal (mkid "A") enum_identifier;
          let expected_table : TM.PatternNode.t Ast.Identifier.Map.t =
            Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal None
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal None
              );
            ]
          in
          assert_equal ~cmp:(Ast.Identifier.Map.equal TM.PatternNode.equal) expected_table table;
          assert_equal ~cmp:(Option.equal Ast.Identifier.equal) None binder_identifier;
          TC.return ()
        end
       | _ -> assert_failure "expected enum node"
    in
    ignore @@ run_tc tc

  in
  "building chain for (enum[A1,A2])" >:: test


let test_build_chain_enum_2 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"; "A3"]
      in
      let* chain =
        build_tuple_pattern_chain [ enum_type ]
      in
      match chain with
      | Enum { enum_identifier; table; binder_identifier } -> begin
          assert_equal ~cmp:Ast.Identifier.equal (mkid "A") enum_identifier;
          let expected_table : TM.PatternNode.t Ast.Identifier.Map.t =
            Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal None
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal None
              );
              (
                mkid "A3",
                TM.PatternNode.Terminal None
              );
            ]
          in
          assert_equal ~cmp:(Ast.Identifier.Map.equal TM.PatternNode.equal) expected_table table;
          assert_equal ~cmp:(Option.equal Ast.Identifier.equal) None binder_identifier;
          TC.return ()
        end
       | _ -> assert_failure "expected enum node"
    in
    ignore @@ run_tc tc

  in
  "building chain for (enum[A1,A2,A3])" >:: test


let test_build_chain_enum_3 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* chain =
        build_tuple_pattern_chain [ enum_type; enum_type ]
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Enum {
                  enum_identifier = mkid "A";
                  table = Ast.Identifier.Map.of_alist_exn [
                      (
                        mkid "A1",
                        TM.PatternNode.Terminal None
                      );
                      (
                        mkid "A2",
                        TM.PatternNode.Terminal None
                      );
                    ];
                  binder_identifier = None;
                }
              );
              (
                mkid "A2",
                TM.PatternNode.Enum {
                  enum_identifier = mkid "A";
                  table = Ast.Identifier.Map.of_alist_exn [
                      (
                        mkid "A1",
                        TM.PatternNode.Terminal None
                      );
                      (
                        mkid "A2",
                        TM.PatternNode.Terminal None
                      );
                    ];
                  binder_identifier = None;
                }
              );
            ];
          binder_identifier = None;
        }
      in
      assert_equal ~cmp:TM.PatternNode.equal expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  "building chain for (enum[A1,A2], enum[A1, A2])" >:: test


let test_categorize_enum_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
          chain
          [ Pattern.EnumCase (mkid "A1") ]
          a1_statement
          false
        in
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement);
              );
            ];
          binder_identifier = None;
        }
      in
      assert_equal ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr) ~cmp:TM.PatternNode.equal expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
    enum A { A1 }

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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
          chain
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = true }
          ]
          a1_statement
          false
        in
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement)
              );
            ];
          binder_identifier = None;
        }
      in
      assert_equal ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr) ~cmp:TM.PatternNode.equal expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
    enum A { A1 }

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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
          chain
          [
            Pattern.Binder { identifier = mkid "x"; wildcard = false }
          ]
          a1_statement
          false
        in
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement)
              );
            ];
          binder_identifier = Some (mkid "x");
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A { A1 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
            false
        in            
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement)
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal (Some a2_statement)
              );
            ];
          binder_identifier = None;
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A { A1, A2 }
  
      match a {
        A1 => read_register r1,
        A2 => read_register r2,
      }
  |} >:: test


let test_categorize_enum_5 =
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
            false
        in            
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement)
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal (Some a2_statement)
              );
            ];
          binder_identifier = None;
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc

  in
  {|
      enum A { A1, A2 }
  
      match a {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_categorize_enum_6 =
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false }
            ]
            a2_statement
            false
        in            
        TC.return chain
      in
      let expected_chain =
        TM.PatternNode.Enum {
          enum_identifier = mkid "A";
          table = Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal (Some a1_statement)
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal (Some a2_statement)
              );
            ];
          binder_identifier = Some (mkid "x");
        }
      in
      assert_equal
        ~printer:(Fn.compose FExpr.to_string TM.PatternNode.to_fexpr)
        ~cmp:TM.PatternNode.equal
        expected_chain chain;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
      match a {
        A1 => read_register r1,
        x  => read_register r2,
      }
  |} >:: test


let test_build_match_for_enum_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let a1_statement =        
        Ast.Statement.ReadRegister (mkid "r1")
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A2")
            ]
            a2_statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1")
            ]
            a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true }
            ]
            a2_statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
      match value1 {
        A1 => read_register r1,
        _  => read_register r2,
      }
  |} >:: test


let test_chain_building_suite =
  "chain building test suite" >::: [
    test_build_chain_enum_1;
    test_build_chain_enum_2;
    test_build_chain_enum_3;
  ]


let test_categorizing_suite =
  "chain building test suite" >::: [
    test_categorize_enum_1;
    test_categorize_enum_2;
    test_categorize_enum_3;
    test_categorize_enum_4;
    test_categorize_enum_5;
    test_categorize_enum_6;
  ]


let test_generate_match_suite =
  "match generation" >::: [
    test_build_match_for_enum_1;
    test_build_match_for_enum_2;
    test_build_match_for_enum_3;
  ]

let test_suite =
  "tuple pattern matching test suite" >::: [
    test_chain_building_suite;
    test_categorizing_suite;
    test_generate_match_suite;
  ]
