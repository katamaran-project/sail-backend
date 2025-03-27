open Base
open OUnit2
open Nanosail
include Shared


let evar (id : string) : Ast.Expression.t = Variable (mkid id, Int None)
let svar (id : string) : Ast.Statement.t  = Expression (evar id)

let eval (n : int) : Ast.Expression.t = Value (Ast.Value.mk_int n)
let sval (n : int) : Ast.Statement.t  = Expression (eval n)

let test_simplify_unused_let_binder =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk "x";
        binding_statement_type = Int None;
        binding_statement      = sval 5;
        body_statement         = sval 6;
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_unused_let_binder statement
    and expected : Ast.Statement.t =
      Seq (
        sval 5,
        sval 6
      )
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
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
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let _ = () in x

    should become

      (); x
  |} >:: test


let test_simplify_unused_let_binder_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = Ast.Identifier.mk "x";
        binding_statement_type = Int None;
        binding_statement      = Let {
            binder                 = Ast.Identifier.mk "y";
            binding_statement_type = Int None;
            binding_statement      = sval 1;
            body_statement         = sval 2;
          };
        body_statement         = sval 3;
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_unused_let_binder statement
    and expected : Ast.Statement.t =
      Seq (
        Seq (
          sval 1,
          sval 2
        ),
        sval 3
      )
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let x =
        let y = 1
        in
        2
      in
      3

    should become

      (1; 2); 3
  |} >:: test


let test_simplify_seq_unit =
  let test _ =
    let statement : Ast.Statement.t =
      Seq (
        Expression (Value Ast.Value.Unit),
        sval 5
      )
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_seq_unit statement
    and expected : Ast.Statement.t =
      Expression (Value (Ast.Value.mk_int 5))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
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
        binding_statement      = svar "y";
        body_statement         = svar "x";
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      svar "y"
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
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


let test_simplify_aliases_write_register =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = WriteRegister { register_identifier = mkid "reg" ; written_value = mkid "x" }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      WriteRegister { register_identifier = mkid "reg" ; written_value = mkid "y" }
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      let x = y
      in
      write_register(reg, x)

    should become

      write_register(reg, y)
  |} >:: test

  |} >:: test


let test_simplify_statement =
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
      ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
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
    test_simplify_unused_let_binder_3;
    test_simplify_seq_unit;
    test_simplify_aliases;
    test_simplify_aliases_write_register;
    test_simplify_statement;
    test_simplify_statement_2;
  ]
