open Base
open Auxlib
open OUnit2
open Shared


let test_run input expected =
  input >:: fun _ -> begin
      let (actual, _)  = Slang.run_string Slang.prelude input
      in
      let msg =
        Printf.sprintf "expected = %s != %s = actual" (Slang.Value.to_string expected) (Slang.Value.to_string actual)
      in
      assert_equal ~msg expected actual
    end


let arithmetic_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ( "5", Integer 5 );
      ( "(+)", Integer 0 );
      ( "(+ 1)", Integer 1 );
      ( "(+ 1 2)", Integer 3 );
      ( "(+ 7 8 10)", Integer (7 + 8 + 10) );
      ( "(-)", Integer 0 );
      ( "(- 5)", Integer (-5) );
      ( "(- 5 1)", Integer (5 - 1) );
      ( "(- 5 1 2)", Integer (5 - 1 - 2) );
      ( "(*)", Integer 1 );
      ( "(* 2)", Integer 2 );
      ( "(* 2 3)", Integer 6 );
      ( "(/ 10 2)", Integer 5 );
      ( "(/ 9 2)", Integer 4 );
      ( "(/ 100 2 2)", Integer 25 );
      ( "(+ 1 (* 2 3))", Integer 7 );
      ( "(% 10 5)", Integer 0 );
      ( "(% 5 10)", Integer 5 );
      ( "(% 12 10)", Integer 2 );
      ( "(% 128 10)", Integer 8 );
      ( {|(+ "ab" "cd")|}, String "abcd" );
      ( {|(* "ab" 3)|}, String "ababab" );
      ( {|(* 3 "ab")|}, String "ababab" );
    ]
  in
  "arithmetic" >::: List.map ~f:(uncurry test_run) test_cases


let boolean_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("#t", Bool true);
      ("#f", Bool false);

      ("(or)", Bool false);

      ("(or #t)", Bool true);
      ("(or #f)", Bool false);

      ("(or #f #f)", Bool false);
      ("(or #t #f)", Bool true);
      ("(or #f #t)", Bool true);
      ("(or #t #t)", Bool true);

      ("(or #f #f #f)", Bool false);
      ("(or #t #f #f)", Bool true);
      ("(or #f #t #f)", Bool true);
      ("(or #f #f #t)", Bool true);

      ("(and)", Bool true);

      ("(and #t)", Bool true);
      ("(and #f)", Bool false);

      ("(and #t #t)", Bool true);
      ("(and #f #t)", Bool false);
      ("(and #t #f)", Bool false);
      ("(and #f #f)", Bool false);

      ("(and #t #t #t)", Bool true);
      ("(and #f #t #t)", Bool false);
      ("(and #t #f #t)", Bool false);
      ("(and #t #t #f)", Bool false);
    ]
  in
  "booleans" >::: List.map ~f:(uncurry test_run) test_cases


let quote_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("'5", Integer 5 );
      ("'abc", Symbol "abc" );
      ("'(1 2 3)", Cons (Integer 1, Cons (Integer 2, Cons (Integer 3, Nil))) );
      ("'((1 2 3))", list_to_cons [ list_to_cons [ Integer 1; Integer 2; Integer 3 ] ]);
      ({|'("abc")|}, Cons (String "abc", Nil));
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
      ("(any? (lambda (x) #t) '())", Bool false);
      ("(any? (lambda (x) (> x 10)) '(1 2 3))", Bool false);
      ("(any? (lambda (x) (> x 10)) '(1 15 7))", Bool true);
      ("(any? (lambda (x) (< x 10)) '(1 15 7))", Bool true);
      ("(any? (lambda (x) (< x 10)) '(10 15 17))", Bool false);
      ("(any? (lambda (x) (= x 8)) '(1 15 7))", Bool false);
      ("(any? (lambda (x) (= x 15)) '(1 15 7))", Bool true);
      ("(all? (lambda (x) #t) '())", Bool true);
      ("(all? (lambda (x) #f) '())", Bool true);
      ("(all? (lambda (x) (= x 10)) '(1 2 3))", Bool false);
      ("(all? (lambda (x) (= x 10)) '(10))", Bool true);
      ("(all? (lambda (x) (= x 10)) '(10 10))", Bool true);
      ("(all? (lambda (x) (> x 10)) '(1 2 3))", Bool false);
      ("(all? (lambda (x) (> x 3)) '(1 2 3))", Bool false);
      ("(all? (lambda (x) (> x 2)) '(1 2 3))", Bool false);
      ("(all? (lambda (x) (> x 1)) '(1 2 3))", Bool false);
      ("(all? (lambda (x) (> x 0)) '(1 2 3))", Bool true);
      ("(contains? '() 1)", Bool false);
      ("(contains? '(1) 1)", Bool true);
      ("(contains? '(1 2) 1)", Bool true);
      ("(contains? '(1 2) 3)", Bool false);
      ("(contains? '(#t 2) 3)", Bool false);
      ("(contains? '(#t 2) #f)", Bool false);
      ("(contains? '(#t 2) #t)", Bool true);
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
      ("()", "'()");
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
        [
          Printf.sprintf {|
            (define (id x) x)
            (id %d)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (define (sqr x) (* x x))
            (sqr %d)
          |} k,
          Integer (k * k)
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (define (double x) (* x 2))
            (double %d)
          |} k,
          Integer (k * 2)
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        and* i = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (define (add x y) (+ x y))
            (add %d %d)
          |} k i,
          Integer (k + i)
        ];
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
        [
          Printf.sprintf "(cons? (cons %s %s))" car cdr,
          Bool true
        ]
      end;

      addall begin
        let* value = [ "1"; {|"abc"|}; "#t"; "#f"; "()"; "'xyz" ]
        in
        [
          Printf.sprintf "(cons? %s)" value,
          Bool false
        ]
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
        [
          Printf.sprintf "(integer? %s)" value,
          Bool false
        ]
      end;

      addall begin
        let* value = ["abc"; "x"; "1+"; "="]
        in
        [
          Printf.sprintf "(symbol? '%s)" value,
          Bool true
        ]
      end;

      addall begin
        let* value = [ "(cons 1 2)"; {|"abc"|}; "#t"; "#f"; "()"; "4" ]
        in
        [
          Printf.sprintf "(symbol? %s)" value,
          Bool false
        ]
      end;

      addall begin
        let* value = ["abc"; "x"; "1+"; "="]
        in
        [
          Printf.sprintf {|(string? "%s")|} value,
          Bool true
        ]
      end;

      addall begin
        let* value = [ "(cons 1 2)"; "'aaa"; "#t"; "#f"; "()"; "4" ]
        in
        [
          Printf.sprintf "(string? %s)" value,
          Bool false
        ]
      end;

      addall begin
        let* value = ["#t"; "#f"]
        in
        [
          Printf.sprintf {|(bool? %s)|} value,
          Bool true
        ]
      end;

      addall begin
        let* value = [ "(cons 1 2)"; "'aaa"; {|"f"|}; "()"; "4" ]
        in
        [
          Printf.sprintf "(bool? %s)" value,
          Bool false
        ]
      end;

      addall begin
        let* value = ["()"]
        in
        [
          Printf.sprintf {|(nil? %s)|} value,
          Bool true
        ]
      end;

      addall begin
        let* value = [ "(cons 1 2)"; "'aaa"; {|"f"|}; "4"; "#t"; "#f" ]
        in
        [
          Printf.sprintf "(nil? %s)" value,
          Bool false
        ]
      end;
    end
  in
  "predicate" >::: List.map ~f:(uncurry test_run) test_cases


let if_then_else_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
           (if #t 5 1)
        |},
        Integer 5
      );
      (
        {|
           (if #f 5 1)
        |},
        Integer 1
      );
      (
        {|
           (if #t 5)
        |},
        Integer 5
      );
      (
        {|
           (if #f 5)
        |},
        Nil
      );
      (
        {|
           (if 2 5 1)
        |},
        Integer 5
      );
      (
        {|
           (if 0 5 1)
        |},
        Integer 5
      );
      (
        {|
           (if (= 1 1) 'yes 'no)
        |},
        Symbol "yes"
      );
      (
        {|
           (if (= 1 2) 'yes 'no)
        |},
        Symbol "no"
      );
      (
        {|
           (if (= "abc" "abc") (* 2 3) (+ 7 5))
        |},
        Integer 6
      );
      (
        {|
           (if (= "abc" "abcd") (* 2 3) (+ 7 5))
        |},
        Integer 12
      );
    ]
  in
  "conditionals" >::: List.map ~f:(uncurry test_run) test_cases


let comparison_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    build_list begin fun { addall; _ } ->
      addall begin
        let* args = [
          "";
          "1";
          "1 2";
          "1 3";
          "2 3";
          "1 2 3";
          "4 6 8";
          {| "x" |};
          {| "a" "b" |};
          {| "a" "b" "c" |};
          {| "aardvark" "zebra" |};
        ]
        in
        [
          Printf.sprintf "(< %s)" args,
          Bool true
        ]
      end;

      addall begin
        let* args = [
          "1 1";
          "3 1";
          "4 2 1";
          "4 1 4";
          "4 9 2";
          {| "b" "a" |};
          {| "a" "c" "b" |};
        ]
        in
        [
          Printf.sprintf "(< %s)" args,
          Bool false
        ]
      end;

      addall begin
        let* args = [
          "";
          "1";
          "1 2";
          "1 3";
          "2 3";
          "1 2 3";
          "4 6 8";
          "4 4";
          "4 4 4";
          "1 1 2 2 3 3";
          {| "x" |};
          {| "a" "b" |};
          {| "a" "b" "c" |};
          {| "a" "a" |};
          {| "a" "a" "a" |};
          {| "aardvark" "zebra" |};
        ]
        in
        [
          Printf.sprintf "(<= %s)" args,
          Bool true
        ]
      end;

      addall begin
        let* args = [
          "4 1";
          "1 3 2";
          "2 2 1";
          {| "a" "b" "a" |};
        ]
        in
        [
          Printf.sprintf "(<= %s)" args,
          Bool false
        ]
      end;

      addall begin
        let* args = [
          "";
          "1";
          "4 1";
          "6 5 2";
          {| "b" "a" |};
          {| "f" "b" "a" |};
        ]
        in
        [
          Printf.sprintf "(> %s)" args,
          Bool true
        ]
      end;

      addall begin
        let* args = [
          "1 2";
          "4 2 3";
          "1 1";
          "6 6";
          {| "b" "a" "a" |};
          {| "g" "t" |};
        ]
        in
        [
          Printf.sprintf "(> %s)" args,
          Bool false
        ]
      end;

      addall begin
        let* args = [
          "";
          "4";
          "5 2";
          "5 5";
          "9 9 8 8 3";
          "9 5 5 2 1 1";
          {| "x" |};
          {| "b" "b" |};
          {| "b" "a" |};
          {| "f" "b" "a" |};
          {| "f" "b" "b" "a" |};
        ]
        in
        [
          Printf.sprintf "(>= %s)" args,
          Bool true
        ]
      end;

      addall begin
        let* args = [
          "1 2";
          "4 2 9";
          "9 5 5 2 1 1 2";
          {| "x" "z" |};
        ]
        in
        [
          Printf.sprintf "(>= %s)" args,
          Bool false
        ]
      end;
    end
  in
  "comparisons" >::: List.map ~f:(uncurry test_run) test_cases


let cond_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
           (cond (#t 5))
        |},
        Integer 5
      );
      (
        {|
           (cond (#f 5)
                 (#t 6))
        |},
        Integer 6
      );
      (
        {|
           (cond ((= 1 2) 5)
                 ((= 2 2) 6))
        |},
        Integer 6
      );
      (
        {|
           (cond ((= 1 2) (* 2 5))
                 ((= 2 2) (* 3 5)))
        |},
        Integer 15
      );
      (
        {|
           (cond ((< 1 2) (* 2 5))
                 ((= 2 2) (* 3 5)))
        |},
        Integer 10
      );
      (
        {|
           (cond ((> 1 2) (* 2 5))
                 ((= 1 2) (* 3 5))
                 ((< 1 2) (+ 9 9)))
        |},
        Integer 18
      );
      (
        {|
           (cond ((> 1 2) (* 2 5))
                 ((= 1 2) (* 3 5))
                 ((< 5 2) (+ 9 9)))
        |},
        Nil
      );
    ]
  in
  "cond" >::: List.map ~f:(uncurry test_run) test_cases


let tests =
  "evaluation tests" >::: [
    arithmetic_tests;
    boolean_tests;
    quote_tests;
    lambda_tests;
    define_function_tests;
    define_variable_tests;
    atom_equality_tests;
    expression_equality_tests;
    inequality_tests;
    comparison_tests;
    list_tests;
    predicate_tests;
    if_then_else_tests;
    cond_tests;
  ]
