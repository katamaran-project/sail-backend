(* TODO put these tests in monad test suite *)
open OUnit2
open Monads.Continuation


let test_simple_continuation _ =
  let expected = 10
  in
  let c = return expected
  in
  let result = callcc c
  in
  assert_equal expected result


let test_two_steps _ =
  let c =
    let* x = return 1
    in
    let* y = return 2
    in
    return (x + y)
  in
  let result = callcc c
  in
  assert_equal 3 result


let test_suite =
  "continuation tests" >::: [
    "simple continuation" >:: test_simple_continuation;
    "test_two_steps" >:: test_two_steps;
  ]
