open Base
open Nanosail.ExtBase
open OUnit2

open SlangShared
open ListMonadNotations

module Slang = Nanosail.Slang

let modulo_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* x = List.range 0 10
        and* y = List.range 1 10
        in
        [
          Printf.sprintf
          {|
            (define (mod x y)
              (if (< x y)
                x
                (recurse (- x y) y)))
            (mod %d %d)
          |} x y,
          Integer (x % y)
        ];
      end;
    end
  in
  "modulo tests" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let filter_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* xs, expected = [
          ("()", []);
          ("(1)", [1]);
          ("(-1)", []);
          ("(1 2)", [1; 2]);
          ("(-1 2)", [2]);
          ("(-1 2 -3 4 -5)", [2; 4]);
        ]
        in
        [
          Printf.sprintf
          {|
            (filter (lambda (x) (> x 0)) '%s)
          |} xs,
          list_to_cons @@ List.map expected ~f:Mk.integer
        ];
      end;
    end
  in
  "filter" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let mapping_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* alist, value, expected = [
          (
            {|()|},
            {|"a"|},
            Nil
          );
          (
            {|(("a" 1))|},
            {|"a"|},
            Integer 1
          );
          (
            {|(("a" 1))|},
            {|"b"|},
            Nil
          );
        ]
        in
        [
          Printf.sprintf
            {|
              (define (lookup value alist)
                (cond ((nil? alist)
                         ())
                      ((= (caar alist) value)
                         (cadar alist))
                      (#t
                         (recurse value (cdr alist)))))

              (lookup %s '%s)
            |} value alist,
            expected
        ];
      end;
    end
  in
  "mapping" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let nth_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* index, list, expected = [
          (
            0,
            {|(1 2 3)|},
            Integer 1
          );
          (
            1,
            {|(1 2 3)|},
            Integer 2
          );
          (
            2,
            {|(1 2 3)|},
            Integer 3
          );
          (
            0,
            {|("a" "bc" "def")|},
            String "a"
          );
          (
            1,
            {|("a" "bc" "def")|},
            String "bc"
          );
          (
            2,
            {|("a" "bc" "def")|},
            String "def"
          );
        ]
        in
        [
          Printf.sprintf
            {|
              (nth %d '%s)
            |} index list,
            expected
        ];
      end;
    end
  in
  "nth" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let last_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* list, expected = [
          (
            {|(1)|},
            Integer 1
          );
          (
            {|(1 2)|},
            Integer 2
          );
          (
            {|(1 2 3)|},
            Integer 3
          );
          (
            {|("a" "bc" "def" "xyzw")|},
            String "xyzw"
          );
          (
            {|("a" "bc" "def" "xyzw" #f)|},
            Bool false
          );
        ]
        in
        [
          Printf.sprintf
            {|
              (last '%s)
            |} list,
            expected
        ];
      end;
    end
  in
  "last" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let is_prime_tests =
  let open Slang.Value
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* value, expected = [
          (
            "1",
            Bool false
          );
          (
            "2",
            Bool true
          );
          (
            "3",
            Bool true
          );
          (
            "4",
            Bool false
          );
          (
            "5",
            Bool true
          );
          (
            "6",
            Bool false
          );
          (
            "7",
            Bool true
          );          
        ]
        in
        [
          Printf.sprintf
            {|
              (prime? %s)
            |} value,
            expected
        ];
      end;
    end
  in
  "is prime" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let tests =
  "advanced tests" >::: [
    modulo_tests;
    filter_tests;
    mapping_tests;
    nth_tests;
    last_tests;
    is_prime_tests;
  ]
