open OUnit2
module S = Libsail

open Nanosail.SailToNanosail.Translate.Numeric

module TC = Nanosail.SailToNanosail.TranslationContext
module GC = Nanosail.NanosailToMicrosail.GenerationContext



let addloc (numexp : S.Ast.nexp_aux) =
  S.Ast.Nexp_aux (numexp, Unknown)


let string_of_numeric_expression (nexp : S.Ast.nexp)  =
  match TC.run @@ translate_numeric_expression nexp with
  | (TC.Success (nano_nexp, _)) -> begin
      let document = GC.generate Nanosail.Ast.Program.empty (Nanosail.NanosailToMicrosail.Numeric.Expression.pp nano_nexp)
      in
      Nanosail.PP.to_string document
    end
  | _ -> assert_failure "translation failed"


let test_suite_numeric_expression =
  let test_string_of_numeric_expression input expected =
    let test_name =
      Printf.sprintf "Expected %s" expected
    in
    test_name >:: fun _ -> (
        let actual = string_of_numeric_expression input
        in
        let message = Printf.sprintf "Expected %s, actual %s" expected actual
        in
        assert_equal ~msg:message expected actual
      )
  in
  let const c = addloc (S.Ast.Nexp_constant (Z.of_int c))
  and add x y = addloc (S.Ast.Nexp_sum (x, y))
  and sub x y = addloc (S.Ast.Nexp_minus (x, y))
  and mul x y = addloc (S.Ast.Nexp_times (x, y))
  and neg x   = addloc (S.Ast.Nexp_neg x)
  in
  let inputs = [
    (const 5, "5");
    (const 10, "10");
    (add (const 1) (const 2), "1 + 2");
    (sub (const 3) (const 1), "3 - 1");
    (mul (const 2) (const 4), "2 * 4");
    (mul (const 2) (add (const 3) (const 4)), "2 * (3 + 4)");
    (neg (const 1), "-1");
    (neg (add (const 1) (const 2)), "-(1 + 2)");
    (neg (mul (const 1) (const 2)), "-(1 * 2)");
  ]
  in
  "string_of_numeric_expression tests" >::: List.map (fun (input, expected) -> test_string_of_numeric_expression input expected) inputs


let test_suite =
  "conversion test suite" >::: [
    test_suite_numeric_expression
  ]
