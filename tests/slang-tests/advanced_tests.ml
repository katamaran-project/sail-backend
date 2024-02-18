open Base
open Auxlib
open OUnit2

open Shared
open Shared.ListMonadNotations


let modulo_tests =
  let open Slang.Value
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
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
  "modulo tests" >::: List.map ~f:(uncurry test_run) test_cases


let filter_tests =
  let open Slang.Value
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
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
  "filter" >::: List.map ~f:(uncurry test_run) test_cases


let mapping_tests =
  let open Slang.Value
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
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
  "mapping" >::: List.map ~f:(uncurry test_run) test_cases


let tests =
  "advanced tests" >::: [
    modulo_tests;
    filter_tests;
    mapping_tests;
  ]
