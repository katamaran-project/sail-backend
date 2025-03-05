open OUnit2
open Base
open Nanosail


let dummy_location : Libsail.Ast.l =
  Libsail.Parse_ast.Unknown


let mkid                                                          = Ast.Identifier.mk
let mkbinder identifier : SailToNanosail.Translate.Match.Binder.t = { identifier = mkid identifier; wildcard = false }
let mkwild   identifier : SailToNanosail.Translate.Match.Binder.t = { identifier = mkid identifier; wildcard = true }
let mkstm    n                                                    = Ast.Statement.ReadRegister (mkid @@ Printf.sprintf "r%d" n)


let pp_diff
    (to_fexpr           : 'a -> FExpr.t          )
    (formatter          : Stdlib.Format.formatter)
    ((expected, actual) : 'a * 'a                ) : unit
  =
  let highlight =
    PP.decorate [ BackgroundColor Red ]
  in
  let expected_fexpr = to_fexpr expected
  and actual_fexpr   = to_fexpr actual
  in
  let document =
    PP.vertical [
      PP.string "";
      PP.horizontal [
        PP.string "Expected: ";
        FExpr.pp expected_fexpr
      ];
      PP.horizontal [
        PP.string "Actual: ";
        FExpr.pp actual_fexpr
      ];
      PP.horizontal [
        PP.string "Difference: ";
        FExpr.pp_diff highlight ~compared_with:actual_fexpr ~printed:expected_fexpr
      ]
    ]
  in  
  Stdlib.Format.pp_print_string formatter @@ PP.to_string document


let assert_equal_statements
    (expected : Ast.Statement.t)
    (actual   : Ast.Statement.t) : unit
  =
  assert_equal
    ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
    ~cmp:Ast.Statement.equal
    (Normalize.normalize_statement expected)
    (Normalize.normalize_statement actual)


let eqmod (normalize : 'a -> 'a) (equal : 'a -> 'a -> bool) (x : 'a) (y : 'a) =
  equal (normalize x) (normalize y)
