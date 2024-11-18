open Base
open OUnit2


let z = Z.of_int
let c x = Nanosail.Ast.Numeric.Expression.Constant (z x)
let id x = Nanosail.Ast.Numeric.Expression.Id (Nanosail.Ast.Identifier.mk x)
let var x = Nanosail.Ast.Numeric.Expression.Var (Nanosail.Ast.Identifier.mk x)


let test_formatting =
  let test ast expected =
    expected >:: fun _ -> begin
        let document =
          Nanosail.NanosailToMicrosail.Numeric.Expression.pp ast
        in
        let document =
          Nanosail.NanosailToMicrosail.GenerationContext.generate document
        in
        let actual =
          Nanosail.PP.string_of_document document
        in
        let msg =
          Printf.sprintf "expected %s; got %s" expected actual
        in
        assert_equal ~msg expected actual
      end
  in
  let test_cases =
    let open Nanosail.Ast.Numeric.Expression in
    [
      (Constant (z 1), "1");
      (Constant (z 5), "5");
      (Constant (z 12345), "12345");
      (Add (c 1, c 2), "1 + 2");
      (Add (c 12, c 23), "12 + 23");
      (Minus (c 1, c 2), "1 - 2");
      (Minus (c 13, c 10), "13 - 10");
      (Times (c 2, c 9), "2 * 9");
      (Neg (c 10), "-10");
      (id "x", "x");
      (var "yx", "yx");
      (PowerOf2 (id "x"), "2^x");
      (Add (Add (c 1, c 2), Add (c 3, c 4)), "1 + 2 + 3 + 4");
      (Minus (c 1, Minus (c 2, c 3)), "1 - (2 - 3)");
      (Minus (Minus (c 1, c 2), c 3), "1 - 2 - 3");
]
  in
  "formatting" >::: List.map ~f:(Auxlib.uncurry test) test_cases

let test_suite =
  "numeric expression pretty printing tests" >::: [
      test_formatting
    ]

