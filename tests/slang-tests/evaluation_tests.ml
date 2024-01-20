open Base
open Auxlib
open OUnit2
open Shared


let test_run input expected =
  input >:: fun _ -> begin
      let (actual, _)  = Slang.run_string Slang.prelude input
      in
      let msg =
        Printf.sprintf "%s != %s" (Slang.Value.to_string expected) (Slang.Value.to_string actual)
      in
      assert_equal ~msg expected actual
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
      ({|(+ "ab" "cd")|}, String "abcd");
      ({|(* "ab" 3)|}, String "ababab");
      ({|(* 3 "ab")|}, String "ababab");
    ]
  in
  "arithmetic" >::: List.map ~f:(uncurry test_run) test_cases


let quote_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("'5", Integer 5 );
      ("'abc", Symbol "abc" );
      ("'(1 2 3)", Cons (Integer 1, Cons (Integer 2, Cons (Integer 3, Nil))) );
    ]
  in
  "quoting" >::: List.map ~f:(uncurry test_run) test_cases


let list_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("(car (cons 1 2))", Integer 1 );
      ("(cdr (cons 1 2))", Integer 2 );
      ("(car (cons (+ 1 5) (cons 2 3)))", Integer 6 );
      ("(cdr (cons (+ 1 5) (cons 2 3)))", Cons (Integer 2, Integer 3) );
    ]
  in
  "lists" >::: List.map ~f:(uncurry test_run) test_cases


let atom_equality_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      "1";
      "2";
      "#t";
      "#f";
      {|"abc"|};
      "()";
    ]
  in
  "equality of atoms" >::: List.map ~f:(fun input -> test_run (Printf.sprintf "(= %s %s)" input input) (Bool true)) test_cases


let expression_equality_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("(+ 1 2)", "(/ 6 2)");
      ("(cons 1 2)", "(cons 1 (+ 1 1))");
    ]
  in
  "equality of expressions" >::: List.map ~f:(fun (lhs, rhs) -> test_run (Printf.sprintf "(= %s %s)" lhs rhs) (Bool true)) test_cases


let inequality_tests =
  let open Slang.Value
  in
  let expressions = [
    "0";
    "1";
    "(+ 2 3)";
    "#t";
    "#f";
    {|""|};
    {|"xyz"|};
    "()";
  ]
  in
  let pairs = Auxlib.unordered_pairs expressions
  in
  "inequality" >::: List.map ~f:(fun (lhs, rhs) -> test_run (Printf.sprintf "(= %s %s)" lhs rhs) (Bool false)) pairs


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
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
      addall begin
        let* k = List.range (-10) 10
        in
        return (
          Printf.sprintf {|
            (define (id x) x)
            (id %d)
          |} k,
          Integer k
        );
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        return (
          Printf.sprintf {|
            (define (sqr x) (* x x))
            (sqr %d)
          |} k,
          Integer (k * k)
        );
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        return (
          Printf.sprintf {|
            (define (double x) (* x 2))
            (double %d)
          |} k,
          Integer (k * 2)
        );
      end;

      addall begin
        let* k = List.range (-10) 10
        and* i = List.range (-10) 10
        in
        return (
          Printf.sprintf {|
            (define (add x y) (+ x y))
            (add %d %d)
          |} k i,
          Integer (k + i)
        );
      end;
    end
  in
  "define function" >::: List.map ~f:(uncurry test_run) test_cases


let define_variable_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
          (define x 1)
          (define y 2)
          (+ x y)
        |},
        Integer 3
      );
      (
        {|
          (define x (* 5 9))
          x
        |},
        Integer 45
      );
      (
        {|
          (define foo (lambda (x y) (+ x y)))
          (foo 13 12)
        |},
        Integer 25
      );
    ]
  in
  "define function" >::: List.map ~f:(uncurry test_run) test_cases


let predicate_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
      addall begin
        let* car = [ "1"; {|"abc"|}; "#t"; "#f"; "()"; "'xyz"; "(cons 1 2)" ]
        and* cdr = [ "1"; {|"abc"|}; "#t"; "#f"; "()"; "'xyz"; "(cons 1 2)" ]
        in
        return (
          Printf.sprintf "(cons? (cons %s %s))" car cdr,
          Bool true
        )
      end;

      addall begin
        let* value = [ "1"; {|"abc"|}; "#t"; "#f"; "()"; "'xyz" ]
        in
        return (
          Printf.sprintf "(cons? %s)" value,
          Bool false
        )
      end;

      addall begin
        let* value = [-431; 0; 312]
        in
        return (
          Printf.sprintf "(integer? %d)" value,
          Bool true
        )
      end;

      addall begin
        let* value = [ "(cons 1 2)"; {|"abc"|}; "#t"; "#f"; "()"; "'xyz" ]
        in
        return (
          Printf.sprintf "(integer? %s)" value,
          Bool false
        )
      end;

      addall begin
        let* value = ["abc"; "x"; "1+"; "="]
        in
        return (
          Printf.sprintf "(symbol? '%s)" value,
          Bool true
        )
      end;

      addall begin
        let* value = [ "(cons 1 2)"; {|"abc"|}; "#t"; "#f"; "()"; "4" ]
        in
        return (
          Printf.sprintf "(symbol? %s)" value,
          Bool false
        )
      end;

      addall begin
        let* value = ["abc"; "x"; "1+"; "="]
        in
        return (
          Printf.sprintf {|(string? "%s")|} value,
          Bool true
        )
      end;

      addall begin
        let* value = [ "(cons 1 2)"; "'aaa"; "#t"; "#f"; "()"; "4" ]
        in
        return (
          Printf.sprintf "(string? %s)" value,
          Bool false
        )
      end;

      addall begin
        let* value = ["#t"; "#f"]
        in
        return (
          Printf.sprintf {|(bool? %s)|} value,
          Bool true
        )
      end;

      addall begin
        let* value = [ "(cons 1 2)"; "'aaa"; {|"f"|}; "()"; "4" ]
        in
        return (
          Printf.sprintf "(bool? %s)" value,
          Bool false
        )
      end;
    end
  in
  "predicate" >::: List.map ~f:(uncurry test_run) test_cases


let tests =
  "evaluation tests" >::: [
    arithmetic_tests;
    quote_tests;
    lambda_tests;
    define_function_tests;
    define_variable_tests;
    atom_equality_tests;
    expression_equality_tests;
    inequality_tests;
    list_tests;
    predicate_tests;
  ]
