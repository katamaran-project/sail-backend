open Base
open Auxlib
open OUnit2


let test_run input expected =
  input >:: fun _ -> begin
      let (actual, _)  = Slang.run_string Slang.prelude input
      in
      let msg =
        Printf.sprintf "expected = %s != %s = actual" (Slang.Value.to_string expected) (Slang.Value.to_string actual)
      in
      assert_equal ~msg expected actual
    end


let arithmetic_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
          (define (mod x y)
            (if (< x y)
              x
              (recurse (- x y) y)))
          (mod 9 5)
        |},
        Integer 4
      );
      (
        {|
          (define (filter pick? xs)
            (if (nil? xs)
                ()
                (if (pick? (car xs))
                    (cons (car xs) (recurse pick? (cdr xs)))
                    (recurse pick? (cdr xs)))))

          (filter (lambda (x) (> x 0)) '(-4 9 -2 4))
        |},
        list_to_cons [Integer 9; Integer 4]
      );
    ]
  in
  "advanced" >::: List.map ~f:(uncurry test_run) test_cases




let tests =
  "evaluation tests" >::: [
    arithmetic_tests;
  ]
