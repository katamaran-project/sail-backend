open Base
open OUnit2
open Nanosail


let mkid  = Ast.Identifier.mk
let mkgid = Fn.compose Ast.Identifier.mk_generated Int.to_string


let test_simplify_unused_let_binder =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk_generated "a";
        binding_statement_type = Int None;
        binding_statement      = Expression (Value (Ast.Value.mk_int 5));
        body_statement         = Expression (Value (Ast.Value.mk_int 6));
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_unused_let_binder statement
    and expected : Ast.Statement.t =
      Seq (
        Expression (Value (Ast.Value.mk_int 5)),
        Expression (Value (Ast.Value.mk_int 6))
      )
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let x = 5 in 6

    should become

      5; 6
  |} >:: test


let test_simplify_unused_let_binder_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk_generated "a";
        binding_statement_type = Unit;
        binding_statement      = Expression (Value Unit);
        body_statement         = Expression (Variable (Ast.Identifier.mk "x", Unit))
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_unused_let_binder statement
    and expected : Ast.Statement.t =
      Seq (
        Expression (Value Unit),
        Expression (Variable (Ast.Identifier.mk "x", Unit))
      )
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let _ = () in x

    should become

      (); x
  |} >:: test


let test_simplify_seq_unit =
  let test _ =
    let statement : Ast.Statement.t =
      Seq (
        Expression (Value Ast.Value.Unit),
        Expression (Value (Ast.Value.mk_int 5))
      )
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_seq_unit statement
    and expected : Ast.Statement.t =
      Expression (Value (Ast.Value.mk_int 5))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      (); 5

    should become

      5
  |} >:: test


let test_simplify_aliases =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk "x";
        binding_statement_type = Unit;
        binding_statement      = Expression (Variable (Ast.Identifier.mk "y", String));
        body_statement         = Expression (Variable (Ast.Identifier.mk "x", String))
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Expression (Variable (Ast.Identifier.mk "y", String));
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let x = y
      in
      x

    should become

      y
  |} >:: test

let test_simplify_statement_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk_generated "a";
        binding_statement_type = Unit;
        binding_statement      = Expression (Value Unit);
        body_statement         = Expression (Variable (Ast.Identifier.mk "x", Unit))
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify statement
    and expected : Ast.Statement.t =
      Expression (Variable (Ast.Identifier.mk "x", Unit))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let _ = () in x

    should become

      x
  |} >:: test


let test_simplify_statement_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk_generated "a";
        binding_statement_type = Unit;
        binding_statement      = Let {
            binder                 = Ast.Identifier.mk_generated "a";
            binding_statement_type = Ast.Type.Unit;
            binding_statement      = Expression (Value Unit);
            body_statement         = Expression (Value Unit);
          };
        body_statement         = Expression (Variable (Ast.Identifier.mk "x", Unit))
      }
    in
    let actual =
      Ast.Statement.simplify statement
    and expected : Ast.Statement.t =
      Expression (Variable (Ast.Identifier.mk "x", Unit))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let _ = let _ = () in () in x

    should become

      x
  |} >:: test


let test_suite =
  "simplification" >::: [
    test_simplify_unused_let_binder;
    test_simplify_unused_let_binder_2;
    test_simplify_seq_unit;
    test_simplify_aliases;
    test_simplify_statement_1;
    test_simplify_statement_2;
  ]
