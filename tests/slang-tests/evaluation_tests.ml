open Base
open Auxlib
open OUnit2


let test_run input expected =
  input >:: fun _ -> begin
      let (actual, _)  = Slang.run_string Slang.prelude input
      in
      assert_equal expected actual
    end


let arithmetic_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("5", Integer 5 );
      ("(+)", Integer 0 );
      ("(+ 1)", Integer 1 );
      ("(+ 1 2)", Integer 3 );
      ("(+ 7 8 10)", Integer 25 );
      ("(-)", Integer 0 );
      ("(- 5)", Integer (-5) );
      ("(- 5 1)", Integer 4 );
      ("(- 5 1 2)", Integer 2 );
      ("(*)", Integer 1 );
      ("(* 2)", Integer 2 );
      ("(* 2 3)", Integer 6 );
      ("(/ 10 2)", Integer 5 );
      ("(/ 9 2)", Integer 4 );
      ("(/ 100 2 2)", Integer 25 );
      ("(+ 1 (* 2 3))", Integer 7);
    ]
  in
  "arithmetic" >::: List.map ~f:(uncurry test_run) test_cases


let lambda_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("((lambda () 1))", Integer 1 );
      ("((lambda () (+ 1 2)))", Integer 3 );
      ("((lambda (x) 5) 1)", Integer 5 );
      ("((lambda (x) x) 2)", Integer 2 );
      ("((lambda (x) (+ x 1)) 2)", Integer 3 );
      ("((lambda (x) (* 2 x)) 3)", Integer 6 );
    ]
  in
  "lambda" >::: List.map ~f:(uncurry test_run) test_cases


let define_function_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
          (define (sqr x) (* x x))
          (sqr 3)
        |},
        Integer 9
      );
      (
        {|
          (define (sqr x) (* x x))
          (define (1+ x) (+ 1 x))
          (1+ (sqr 3))
        |},
        Integer 10
      );
    ]
  in
  "define function" >::: List.map ~f:(uncurry test_run) test_cases


let tests =
  "evaluation tests" >::: [
    arithmetic_tests;
    lambda_tests;
    define_function_tests;
  ]
