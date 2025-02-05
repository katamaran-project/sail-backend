open Base
open OUnit2
open Nanosail


let mkid  = Ast.Identifier.mk
let mkgid = Fn.compose Ast.Identifier.mk_generated Int.to_string


let test_simplify_statement_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        variable_identifier    = Ast.Identifier.mk_generated "a";
        binding_statement_type = Ast.Type.Unit;
        binding_statement      = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
        body_statement         = Ast.Statement.Expression (Ast.Expression.Variable (Ast.Identifier.mk "x", Ast.Type.Unit))
      }
    in
    let actual = Simplify.simplify_statement statement
    and expected =
      Ast.Statement.Expression (Ast.Expression.Variable (Ast.Identifier.mk "x", Ast.Type.Unit))
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
        variable_identifier    = Ast.Identifier.mk_generated "a";
        binding_statement_type = Ast.Type.Unit;
        binding_statement      = Ast.Statement.Let {
            variable_identifier    = Ast.Identifier.mk_generated "a";
            binding_statement_type = Ast.Type.Unit;
            binding_statement      = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
            body_statement         = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
          };
        body_statement         = Ast.Statement.Expression (Ast.Expression.Variable (Ast.Identifier.mk "x", Ast.Type.Unit))
      }
    in
    let actual = Simplify.simplify_statement statement
    and expected =
      Ast.Statement.Expression (Ast.Expression.Variable (Ast.Identifier.mk "x", Ast.Type.Unit))
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
    test_simplify_statement_1;
    test_simplify_statement_2;
  ]
