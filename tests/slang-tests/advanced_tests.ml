open Base
open Auxlib
open OUnit2

open Shared.ListMonadNotations


let test_run input expected =
  input >:: fun _ -> begin
      let (actual, _)  = Slang.run_string Slang.prelude input
      in
      let msg =
        Printf.sprintf "expected = %s != %s = actual" (Slang.Value.to_string expected) (Slang.Value.to_string actual)
      in
      assert_equal ~msg expected actual
    end


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
            (define (filter pick? xs)
              (if (nil? xs)
                  ()
                  (if (pick? (car xs))
                      (cons (car xs) (recurse pick? (cdr xs)))
                      (recurse pick? (cdr xs)))))

            (filter (lambda (x) (> x 0)) '%s)
          |} xs,
          list_to_cons @@ List.map expected ~f:Mk.integer
        ];
      end;
    end
  in
  "advanced" >::: List.map ~f:(uncurry test_run) test_cases




let tests =
  "evaluation tests" >::: [
    modulo_tests;
    filter_tests;
  ]
