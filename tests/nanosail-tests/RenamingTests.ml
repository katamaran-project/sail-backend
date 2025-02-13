open OUnit2
open Nanosail
open Shared


let mkid = Ast.Identifier.mk
let evar id = Ast.Expression.Variable (mkid id, Ast.Type.Int)


let test_rename_match_product_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Int;
          type_snd = Int;
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
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal_statements expected actual
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
          type_fst = Int;
          type_snd = Int;
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
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "renamed"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal_statements expected actual
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
          type_fst = Int;
          type_snd = Int;
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
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal_statements expected actual
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


let test_rename_match_product_4 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Ast.Statement.Expression (Ast.Expression.Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "right")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Expression (Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match x {
        (left, right) => (x, y, left, right)
      }

    Renaming right -> renamed gives

      match renamed {
        (left, right) => (x, y, left, right)
      }
  |} >:: test


let test_rename_match_product_5 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Expression (Tuple [ evar "x"; evar "y"; evar "left"; evar "right" ])
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "y")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched  = mkid "x";
          type_fst = Int;
          type_snd = Int;
          id_fst   = mkid "left";
          id_snd   = mkid "right";
          body     = Expression (Tuple [ evar "x"; evar "renamed"; evar "left"; evar "right" ])
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match x {
        (left, right) => (x, y, left, right)
      }

    Renaming y -> renamed gives

      match renamed {
        (left, right) => (x, renamed, left, right)
      }
  |} >:: test


let test_rename_expression_var_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (evar "x")
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "a")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Expression (evar "x")
    in
    assert_equal_statements expected actual
  in
  {|
      x
    
    Renaming a -> renamed gives

      x
    
  |} >:: test


let test_rename_expression_var_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (evar "x")
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "x")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Expression (evar "renamed")
    in
    assert_equal_statements expected actual
  in
  {|
      x
    
    Renaming x -> renamed gives

      y
    
  |} >:: test


let test_rename_match_bool_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "cond")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "renamed";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match cond {
        true  => x,
        false => y
      }
    
    Renaming cond -> renamed gives

      match renamed {
        true  => x,
        false => y
      }
  |} >:: test


let test_rename_match_bool_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "a")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match cond {
        true  => x,
        false => y
      }
    
    Renaming a -> renamed gives

      match cond {
        true  => x,
        false => y
      }
  |} >:: test


let test_rename_match_bool_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
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
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "renamed");
          when_false = Expression (evar "y");
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match cond {
        true  => x,
        false => y
      }
    
    Renaming a -> renamed gives

      match cond {
        true  => renamed,
        false => y
      }
  |} >:: test


let test_rename_match_bool_4 =
  let test _ =
    let statement : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "y");
        }
      end
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "y")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Match begin
        MatchBool {
          condition = mkid "cond";
          when_true = Expression (evar "x");
          when_false = Expression (evar "renamed");
        }
      end
    in
    assert_equal_statements expected actual
  in
  {|
      match cond {
        true  => x,
        false => y
      }
    
    Renaming a -> renamed gives

      match cond {
        true  => x,
        false => renamed
      }
  |} >:: test


let test_rename_let_1 =
  let test _ =
    let statement : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "a")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    assert_equal_statements expected actual
  in
  {|
      let x = (x, y)
      in
      (x, z)
    
    Renaming a -> renamed gives

      let x = (x, y)
      in
      (x, z)
  |} >:: test


let test_rename_let_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "x")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "renamed"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    assert_equal_statements expected actual
  in
  {|
      let x = (x, y)
      in
      (x, z)
    
    Renaming x -> renamed gives

      let x = (renamed, y)
      in
      (x, z)
  |} >:: test


let test_rename_let_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "y")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "renamed" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    assert_equal_statements expected actual
  in
  {|
      let x = (x, y)
      in
      (x, z)
    
    Renaming y -> renamed gives

      let x = (x, renamed)
      in
      (x, z)
  |} >:: test


let test_rename_let_4 =
  let test _ =
    let statement : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "z" ]);
      }
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "z")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "y" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "renamed" ]);
      }
    in
    assert_equal_statements expected actual
  in
  {|
      let x = (x, y)
      in
      (x, z)
    
    Renaming z -> renamed gives

      let x = (x, y)
      in
      (x, renamed)
  |} >:: test


let test_rename_let_5 =
  let test _ =
    let statement : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "x"; evar "x" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "x" ]);
      }
    in
    let renamer =
      Ast.Renaming.create_renamer
        (mkid "x")
        (mkid "renamed")
    in
    let actual =
      Ast.Renaming.rename_in_statement renamer statement
    and expected : Ast.Statement.t =
      Ast.Statement.Let {
        variable_identifier    = mkid "x";
        binding_statement_type = Tuple [ Int; Int ];
        binding_statement      = Expression (Tuple [ evar "renamed"; evar "renamed" ]);
        body_statement         = Expression (Tuple [ evar "x"; evar "x" ]);
      }
    in
    assert_equal_statements expected actual
  in
  {|
      let x = (x, x)
      in
      (x, x)
    
    Renaming x -> renamed gives

      let x = (renamed, renamed)
      in
      (x, x)
  |} >:: test


let test_suite =
  "renaming" >::: [
    test_rename_match_product_1;
    test_rename_match_product_2;
    test_rename_match_product_3;
    test_rename_match_product_4;
    test_rename_match_product_5;

    test_rename_match_bool_1;
    test_rename_match_bool_2;
    test_rename_match_bool_3;
    test_rename_match_bool_4;

    test_rename_expression_var_1;
    test_rename_expression_var_2;

    test_rename_let_1;
    test_rename_let_2;
    test_rename_let_3;
    test_rename_let_4;
    test_rename_let_5;
  ]
