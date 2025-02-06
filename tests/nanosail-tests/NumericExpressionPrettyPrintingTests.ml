open Nanosail.ExtBase
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
          Nanosail.NanosailToMicrosail.GenerationContext.generate Nanosail.Ast.empty_program document
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
    let open Nanosail.Ast.Numeric.Expression
    in
    let add x y =
      BinaryOperation (Add, x, y)
    and sub x y =
      BinaryOperation (Sub, x, y)
    and mul x y =
      BinaryOperation (Mul, x, y)
    in
    [
      (Constant (z 1), "1");
      (Constant (z 5), "5");
      (Constant (z 12345), "12345");
      (add (c 1) (c 2), "1 + 2");
      (add (c 12) (c 23), "12 + 23");
      (sub (c 1) (c 2), "1 - 2");
      (sub (c 13) (c 10), "13 - 10");
      (mul (c 2) (c 9), "2 * 9");
      (Neg (c 10), "-10");
      (id "x", "x");
      (var "yx", "yx");
      (PowerOf2 (id "x"), "2^x");
      (add (add (c 1) (c 2)) (add (c 3) (c 4)), "1 + 2 + 3 + 4");
      (sub (c 1) (sub (c 2) (c 3)), "1 - (2 - 3)");
      (sub (sub (c 1) (c 2)) (c 3), "1 - 2 - 3");
      (mul (mul (c 1) (c 2)) (mul (c 3) (c 4)), "1 * 2 * 3 * 4");
      (mul (add (c 1) (c 2)) (add (c 3) (c 4)), "(1 + 2) * (3 + 4)");
      (mul (sub (c 1) (c 2)) (sub (c 3) (c 4)), "(1 - 2) * (3 - 4)");
      (mul
         (add
            (mul
               (add (c 1) (c 2))
               (add (c 3) (c 4)))
            (mul (c 5) (c 6)))
         (add (c 7) (c 8)),
       "((1 + 2) * (3 + 4) + 5 * 6) * (7 + 8)"
      );
      (Neg (add (c 1) (c 2)), "-(1 + 2)");
      (PowerOf2 (add (c 1) (c 2)), "2^(1 + 2)");
      (PowerOf2 (mul (c 1) (c 2)), "2^(1 * 2)");
      (PowerOf2 (PowerOf2 (c 10)), "2^(2^10)");
    ]
  in
  "formatting" >::: List.map ~f:(Fn.uncurry test) test_cases

let test_suite =
  "numeric expression pretty printing tests" >::: [
      test_formatting
    ]
