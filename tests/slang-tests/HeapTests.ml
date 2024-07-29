open Base
open Auxlib
open OUnit2
open Shared


let heap_access_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ( {|
          (define r (ref 1))
          (@ r)
        |}
      , Integer 1
      )
    ]
  in
  "accessing" >::: List.map ~f:(uncurry test_run) test_cases


let heap_update_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ( {|
          (define r (ref 1))
          (@= r 2)
          (@ r)
        |}
      , Integer 2
      )
    ]
  in
  "updating" >::: List.map ~f:(uncurry test_run) test_cases


let tests =
  "heap tests" >::: [
    heap_access_tests;
  ]
