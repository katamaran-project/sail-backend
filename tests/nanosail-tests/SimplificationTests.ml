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


let test_simplify_aliases_match_list =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "tl", svar "a");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "tl", svar "a");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        hd::tl => a,
        []     => b
      }

    should become

      match lst {
        hd::tl => a,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "x";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "tl", svar "a");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "y";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "tl", svar "a");
          when_nil         = svar "b"
        }
      end
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
      match x {
        hd::tl => a,
        []     => b
      }

    should become

      match y {
        hd::tl => a,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "tl", svar "x");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "tl", svar "y");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        hd::tl => x,
        []     => b
      }

    should become

      match lst {
        hd::tl => y,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_4 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "tl", svar "a");
              when_nil         = svar "x"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "tl", svar "a");
          when_nil         = svar "y"
        }
      end
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
      match lst {
        hd::tl => a,
        []     => x
      }

    should become

      match lst {
        hd::tl => a,
        []     => y
      }
  |} >:: test


let test_simplify_aliases_match_list_5 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "x", mkid "tl", svar "a");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "x", mkid "tl", svar "a");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        x::tl  => a,
        []     => b
      }

    should become

      match lst {
        x::tl  => a,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_6 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "tl", svar "a");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "tl", svar "a");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        x::tl  => x,
        []     => b
      }

    should become

      match lst {
        x::tl  => x,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_7 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "x", svar "a");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "x", svar "a");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        hd::x  => a,
        []     => b
      }

    should become

      match lst {
        hd::x  => a,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_match_list_8 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchList {
              matched          = mkid "lst";
              element_type     = Int None;
              when_cons        = (mkid "hd", mkid "x", svar "x");
              when_nil         = svar "b"
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchList {
          matched          = mkid "lst";
          element_type     = Int None;
          when_cons        = (mkid "hd", mkid "x", svar "x");
          when_nil         = svar "b"
        }
      end
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
      match lst {
        hd::x  => x,
        []     => b
      }

    should become

      match lst {
        hd::x  => x,
        []     => b
      }
  |} >:: test


let test_simplify_aliases_destructure_record =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = DestructureRecord {
            record_type_identifier = (mkid "foo");
            field_identifiers      = [ mkid "field1"; mkid "field2" ];
            binders                = [ mkid "a"; mkid "b" ];
            destructured_record    = svar "record";
            body                   = svar "result";
          }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      DestructureRecord {
        record_type_identifier = (mkid "foo");
        field_identifiers      = [ mkid "field1"; mkid "field2" ];
        binders                = [ mkid "a"; mkid "b" ];
        destructured_record    = svar "record";
        body                   = svar "result";
      }
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
      let { field1 = a; field2 = b } = record
      in
      result

    should become

      let { field1 = a; field2 = b } = record
      in
      result
  |} >:: test


let test_simplify_aliases_destructure_record_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = DestructureRecord {
            record_type_identifier = (mkid "foo");
            field_identifiers      = [ mkid "field1"; mkid "field2" ];
            binders                = [ mkid "a"; mkid "b" ];
            destructured_record    = svar "x";
            body                   = svar "result";
          }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      DestructureRecord {
        record_type_identifier = (mkid "foo");
        field_identifiers      = [ mkid "field1"; mkid "field2" ];
        binders                = [ mkid "a"; mkid "b" ];
        destructured_record    = svar "y";
        body                   = svar "result";
      }
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
      let { field1 = a; field2 = b } = x
      in
      result

    should become

      let { field1 = a; field2 = b } = y
      in
      result
  |} >:: test


let test_simplify_aliases_destructure_record_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = DestructureRecord {
            record_type_identifier = (mkid "foo");
            field_identifiers      = [ mkid "field1"; mkid "field2" ];
            binders                = [ mkid "a"; mkid "b" ];
            destructured_record    = svar "record";
            body                   = svar "x";
          }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      DestructureRecord {
        record_type_identifier = (mkid "foo");
        field_identifiers      = [ mkid "field1"; mkid "field2" ];
        binders                = [ mkid "a"; mkid "b" ];
        destructured_record    = svar "record";
        body                   = svar "y";
      }
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
      let { field1 = a; field2 = b } = record
      in
      x

    should become

      let { field1 = a; field2 = b } = record
      in
      y
  |} >:: test


let test_simplify_aliases_destructure_record_4 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = DestructureRecord {
            record_type_identifier = (mkid "foo");
            field_identifiers      = [ mkid "field1"; mkid "field2" ];
            binders                = [ mkid "x"; mkid "b" ];
            destructured_record    = svar "record";
            body                   = svar "result";
          }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      DestructureRecord {
        record_type_identifier = (mkid "foo");
        field_identifiers      = [ mkid "field1"; mkid "field2" ];
        binders                = [ mkid "x"; mkid "b" ];
        destructured_record    = svar "record";
        body                   = svar "result";
      }
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
      let { field1 = x; field2 = b } = record
      in
      result

    should become

      let { field1 = x; field2 = b } = record
      in
      result
  |} >:: test


let test_simplify_aliases_destructure_record_5 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = DestructureRecord {
            record_type_identifier = (mkid "foo");
            field_identifiers      = [ mkid "field1"; mkid "field2" ];
            binders                = [ mkid "x"; mkid "b" ];
            destructured_record    = svar "record";
            body                   = svar "x";
          }
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      DestructureRecord {
        record_type_identifier = (mkid "foo");
        field_identifiers      = [ mkid "field1"; mkid "field2" ];
        binders                = [ mkid "x"; mkid "b" ];
        destructured_record    = svar "record";
        body                   = svar "x";
      }
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
      let { field1 = x; field2 = b } = record
      in
      x

    should become

      let { field1 = x; field2 = b } = record
      in
      x
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


let test_simplify_aliases_product =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchProduct {
              matched          = mkid "product";
              type_fst         = Int None;
              type_snd         = Int None;
              id_fst           = mkid "fst";
              id_snd           = mkid "snd";
              body             = svar "result";
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched          = mkid "product";
          type_fst         = Int None;
          type_snd         = Int None;
          id_fst           = mkid "fst";
          id_snd           = mkid "snd";
          body             = svar "result";
        }
      end
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
      match product {
        (fst, snd) => result
      }
    
    should become

      match product {
        (fst, snd) => result
      }
  |} >:: test


let test_simplify_aliases_product_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchProduct {
              matched          = mkid "x";
              type_fst         = Int None;
              type_snd         = Int None;
              id_fst           = mkid "fst";
              id_snd           = mkid "snd";
              body             = svar "result";
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched          = mkid "y";
          type_fst         = Int None;
          type_snd         = Int None;
          id_fst           = mkid "fst";
          id_snd           = mkid "snd";
          body             = svar "result";
        }
      end
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
      match x {
        (fst, snd) => result
      }
    
    should become

      match y {
        (fst, snd) => result
      }
  |} >:: test


let test_simplify_aliases_product_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Unit;
        binding_statement      = svar "y";
        body_statement         = Match begin
            MatchProduct {
              matched          = mkid "product";
              type_fst         = Int None;
              type_snd         = Int None;
              id_fst           = mkid "fst";
              id_snd           = mkid "snd";
              body             = svar "x";
            }
          end
      }
    in
    let actual : Ast.Statement.t =
      Ast.Statement.simplify_aliases statement
    and expected : Ast.Statement.t =
      Match begin
        MatchProduct {
          matched          = mkid "product";
          type_fst         = Int None;
          type_snd         = Int None;
          id_fst           = mkid "fst";
          id_snd           = mkid "snd";
          body             = svar "y";
        }
      end
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
      match product {
        (fst, snd) => x
      }
    
    should become

      match product {
        (fst, snd) => y
      }
  |} >:: test


let test_suite =
  "simplification" >::: [
    test_simplify_unused_let_binder;
    test_simplify_unused_let_binder_2;
    test_simplify_unused_let_binder_3;
    test_simplify_seq_unit;
    test_simplify_aliases;
    test_simplify_aliases_write_register;
    test_simplify_aliases_match_list;
    test_simplify_aliases_match_list_2;
    test_simplify_aliases_match_list_3;
    test_simplify_aliases_match_list_4;
    test_simplify_aliases_match_list_5;
    test_simplify_aliases_match_list_6;
    test_simplify_aliases_match_list_7;
    test_simplify_aliases_match_list_8;
    test_simplify_aliases_destructure_record;
    test_simplify_aliases_destructure_record_2;
    test_simplify_aliases_destructure_record_3;
    test_simplify_aliases_destructure_record_4;
    test_simplify_aliases_destructure_record_5;
    test_simplify_aliases_product;
    test_simplify_aliases_product_2;
    test_simplify_aliases_product_3;
    test_simplify_statement;
    test_simplify_statement_2;
  ]
