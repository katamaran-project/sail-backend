open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching


let dummy_location : Libsail.Ast.l =
  Libsail.Parse_ast.Unknown

let mkid    = Ast.Identifier.mk
let mkstm n = Ast.Statement.ReadRegister (mkid @@ Printf.sprintf "r%d" n)

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
  "building chain for (enum[A1,A2])" >:: test


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
  "building chain for (enum[A1,A2,A3])" >:: test


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
                (None, TM.PatternNode.Terminal (Some a1_statement));
              );
            ];
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
                (None, TM.PatternNode.Terminal (Some a1_statement))
              );
            ];          
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
                (Some (mkid "x"), TM.PatternNode.Terminal (Some a1_statement))
              );
            ];
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
                (None, TM.PatternNode.Terminal (Some a1_statement))
              );
              (
                mkid "A2",
                (None, TM.PatternNode.Terminal (Some a2_statement))
              );
            ];
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
      let* actual_chain =
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
                (
                  None,  (* no binder since pattern mentions A1 explicitly *)
                  TM.PatternNode.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  None,  (* no binder since pattern is wildcard *)
                  TM.PatternNode.Terminal (Some a2_statement)
                )
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
                (
                  None,  (* no binder since pattern mentions "A1" explicitly *)
                  TM.PatternNode.Terminal (Some a1_statement)
                )
              );
              (
                mkid "A2",
                (
                  Some (mkid "x"),  (* binder necessary because pattern requires the value to be bound to x *)
                  TM.PatternNode.Terminal (Some a2_statement)
                )
              );
            ];          
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in        
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
            false
        in        
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A2");
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
            false
        in        
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in        
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a2_a1_statement
            false
        in        
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a2_a2_statement
            false
        in        
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in        
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
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
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; enum_type ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = false };
              Pattern.EnumCase (mkid "A1");
            ]
            a1_a1_statement
            false
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "x"; wildcard = true };
              Pattern.EnumCase (mkid "A2");
            ]
            a1_a2_statement
            false
        in        
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
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
        expected_match_statement
        actual_match_statement;
      TC.return ()
    in
    ignore @@ run_tc tc
  in
  {|
      enum A { A1, A2 }
  
      match value1, value2 {
        x, A1 => r1,
        _, A2 => r2,
      }
  |} >:: test


let test_build_match_for_int_1 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true }
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier    = mkid "n";
          binding_statement_type = Ast.Type.Int;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement         = statement;
        }
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
      match intval {
        n => read_register r1,
      }
  |} >:: test


let test_build_match_for_int_2 =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = false }
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"] chain
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier    = mkid "n";
          binding_statement_type = Ast.Type.Int;
          binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement         = statement;
        }
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
      match intval {
        n => read_register r1,
      }
  |} >:: test


let test_build_match_for_int_int =
  let test _ =
    let tc =
      let statement =
        mkstm 1
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ Ast.Type.Int; Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.Binder { identifier = mkid "n"; wildcard = true };
              Pattern.Binder { identifier = mkid "k"; wildcard = true };
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "value1"; mkid "value2"] chain
      in
      let expected_match_statement =
        Ast.Statement.Let {
          variable_identifier        = mkid "n";
          binding_statement_type     = Ast.Type.Int;
          binding_statement          = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value1", Ast.Type.Int));
          body_statement             = Ast.Statement.Let {
              variable_identifier    = mkid "k";              
              binding_statement_type = Ast.Type.Int;
              binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "value2", Ast.Type.Int));
              body_statement         = statement;
            };
        }
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
      match (intval, intval) {
        (n, k) => read_register r1,
      }
  |} >:: test


let test_build_match_for_enum_int =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"]
      in
      let statement =
        mkstm 1
      in
      let* chain =
        let* chain = build_tuple_pattern_chain [ enum_type; Ast.Type.Int ]
        in
        let* chain = categorize
            chain
            [
              Pattern.EnumCase (mkid "A1");
              Pattern.Binder { identifier = mkid "k"; wildcard = true };
            ]
            statement
            false
        in
        TC.return chain
      in
      let* actual_match_statement =
        build_match [mkid "enum_value"; mkid "int_value"] chain
      in
      let expected_match_statement =
        Ast.Statement.Match begin
          Ast.Statement.MatchEnum {
            matched = mkid "enum_value";
            matched_type = mkid "A";
            cases = Ast.Identifier.Map.of_alist_exn [
                (
                  mkid "A1",
                  Ast.Statement.Let {
                    variable_identifier    = mkid "k";
                    binding_statement_type = Ast.Type.Int;
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (mkid "int_value", Ast.Type.Int));
                    body_statement         = statement
                  }
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
      enum A = { A1 }
    
      match (enumval, intval) {
        (A1, k) => read_register r1,
      }
  |} >:: test


let test_chain_building_suite =
  "chain building test suite" >::: [
    test_build_chain_enum_1;
    test_build_chain_enum_2;
    test_build_chain_enum_3;
  ]


let test_categorizing_suite =
  "categorizing test suite" >::: [
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
    test_build_match_for_enum_4;
    test_build_match_for_enum_5;
    test_build_match_for_enum_6;
    test_build_match_for_enum_7;
    test_build_match_for_enum_8;
    
    test_build_match_for_int_1;
    test_build_match_for_int_2;
    test_build_match_for_int_int;

    test_build_match_for_enum_int;
  ]


let test_suite =
  "tuple pattern matching test suite" >::: [
    test_chain_building_suite;
    test_categorizing_suite;
    test_generate_match_suite;
  ]
