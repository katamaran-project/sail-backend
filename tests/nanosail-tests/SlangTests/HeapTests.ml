open Nanosail.ExtBase
open OUnit2
open SlangShared

module Slang = Nanosail.Slang


let heap_access_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
          (define r (ref 1))
          (@ r)
        |}
      , Integer 1
      );
      (
        {|
          (define r (ref "abc"))
          (@ r)
        |}
      , String "abc"
      );
      (
        {|
          (define r (ref #t))
          (@ r)
        |}
      , Bool true
      );
      (
        {|
          (define r1 (ref 0))
          (define r2 (ref r1))
          (@ (@ r2))
        |}
      , Integer 0
      );
    ]
  in
  "accessing" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let heap_update_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
          (define r (ref 1))
          (@= r 2)
          (@ r)
        |}
      , Integer 2
      );
      (
        {|
          (define r (ref 1))
          (@= r "xyz")
          (@ r)
        |}
      , String "xyz"
      );
    ]
  in
  "updating" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let counter_test =
  let open Slang.Value
  in
  let program =
    {|
      (define (make-counter)
        (define counter (ref 0))
        (lambda ()
          (@= (+ 1 (@ counter)))
          (@ counter)))

      (define counter (make-counter))

      (list (counter) (counter) (counter))
    |}
  in
  test_run program (Slang.Value.list_to_cons [ Integer 1; Integer 2; Integer 3 ])


let tests =
  "heap tests" >::: [
    heap_access_tests;
    heap_update_tests;
    counter_test;
  ]
