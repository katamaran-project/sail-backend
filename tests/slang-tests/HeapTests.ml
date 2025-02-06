open Nanosail.ExtBase
open OUnit2
open Shared

module Slang = Nanosail.Slang


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
  "accessing" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  "updating" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let tests =
  "heap tests" >::: [
    heap_access_tests;
  ]
