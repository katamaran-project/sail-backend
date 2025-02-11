open Base
open OUnit2
open Nanosail


let mkid = Ast.Identifier.mk
let evar id = Ast.Expression.Variable (mkid id, Ast.Type.Int)


let test_rename_match_product_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "a")
        (mkid "b")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      match x {
        (left, right) => (x, y, left, right)
      }

    Renaming a -> b gives

      match x {
        (left, right) => (x, y, left, right)
      }
  |} >:: test


let test_rename_match_product_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "x")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "renamed";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "renamed"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      match x {
        (left, right) => (x, y, left, right)
      }

    Renaming x -> renamed gives

      match renamed {
        (left, right) => (renamed, y, left, right)
      }
  |} >:: test


let test_rename_match_product_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "left")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Ast.Type.Int;
          type_snd = Ast.Type.Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      match x {
        (left, right) => (x, y, left, right)
      }

    Renaming left -> renamed gives

      match renamed {
        (left, right) => (x, y, left, right)
      }
  |} >:: test


let test_suite =
  "renaming" >::: [
    test_rename_match_product_1;
    test_rename_match_product_2;
    test_rename_match_product_3;
  ]
