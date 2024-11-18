open Base
open OUnit2


let z = Z.of_int
          

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
        assert_equal expected actual
      end
  in
  let test_cases =
    let open Nanosail.Ast.Numeric.Expression in
    [
      (Constant (z 1), "1");
    ]
  in
  "formatting" >::: List.map ~f:(Auxlib.uncurry test) test_cases

let test_suite =
  "numeric expression pretty printing tests" >::: [
      test_formatting
    ]

