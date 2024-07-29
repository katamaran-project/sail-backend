open Base
open Auxlib
open OUnit2
open Shared


let string_join_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
           (join " " '("a" "b" "c"))
        |},
        String "a b c"
      );
    ]
  in
  "arithmetic" >::: List.map ~f:(uncurry test_run) test_cases



let tests =
  "evaluation tests" >::: [
    string_join_tests;
  ]
