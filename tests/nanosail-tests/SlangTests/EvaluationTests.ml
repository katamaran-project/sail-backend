open Nanosail.ExtBase
open OUnit2
open SlangShared

module Slang = Nanosail.Slang


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
  "arithmetic" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  "booleans" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
      ({|'(("xyz"))|}, list_to_cons [ list_to_cons [ String "xyz" ] ]);
    ]
  in
  "quoting" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let list_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ("(car (cons 1 2))", Integer 1 );
      ("(cdr (cons 1 2))", Integer 2 );
      ("(car (cons (+ 1 5) (cons 2 3)))", Integer 6 );
      ("(cdr (cons (+ 1 5) (cons 2 3)))", Cons (Integer 2, Integer 3) );
      ("(list)", Nil);
      ("(list 1)", Cons (Integer 1, Nil));
      ("(list 1 2)", Cons (Integer 1, Cons (Integer 2, Nil)));
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
  "lists" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  let pairs = List.unordered_pairs expressions
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
  "lambda" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let define_function_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
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
  "define function" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  "define function" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let predicate_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
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
  "predicate" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  "conditionals" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let comparison_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
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
  "comparisons" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
  "cond" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let define_macro_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (define-macro (foo x)
              (cons '+ (cons 'x (cons x ()))))

            (foo %d)
          |} k,
          Integer (2 * k)
        ];
      end;
    end
  in
  "define function" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let keyword_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      ( ":a", Symbol ":a" );
      ( ":abc", Symbol ":abc" );
    ]
  in
  "arithmetic" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let destructuring_tests =
  let open Slang.Value in
  let open ListMonadNotations
  in
  let test_cases =
    List.build_list begin fun { addall; _ } ->
      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b c) '(%d 2 3)
              a)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b c) '(1 %d 3)
              b)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b c) '(1 2 %d)
              c)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c %d)) '(1 2)
              a)
          |} k,
          Integer 1
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c %d)) '(1 2)
              2)
          |} k,
          Integer 2
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c %d)) '(1 2)
              c)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* k = List.range (-10) 10
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c %d)) '(1 2)
              c)
          |} k,
          Integer k
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '()
              a)
          |} i j k n,
          Integer i
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '()
              b)
          |} i j k n,
          Integer j
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '()
              c)
          |} i j k n,
          Integer k
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '()
              d)
          |} i j k n,
          Integer n
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9)
              a)
          |} i j k n,
          Integer 9
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8)
              a)
          |} i j k n,
          Integer 9
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8)
              b)
          |} i j k n,
          Integer 8
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8)
              c)
          |} i j k n,
          Integer k
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8 7 6)
              a)
          |} i j k n,
          Integer 9
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8 7 6)
              c)
          |} i j k n,
          Integer 7
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-9; 0; 3]
        and* k = [-5; 0; 6]
        and* n = [-3; 0; 8]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((a %d) (b %d) (c %d) (d %d)) '(9 8 7 6)
              d)
          |} i j k n,
          Integer 6
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d)) ()
              a)
          |} i,
          Integer i
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d)) '(:a %d)
              a)
          |} i j,
          Integer j
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '()
              a)
          |} i j,
          Integer i
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '()
              b)
          |} i j,
          Integer j
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        and* k = [-1; 0; 2]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '(:a %d)
              a)
          |} i j k,
          Integer k
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        and* k = [-1; 0; 2]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '(:a %d)
              b)
          |} i j k,
          Integer j
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        and* k = [-1; 0; 2]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '(:a %d)
              b)
          |} i j k,
          Integer j
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        and* k = [-1; 0; 2]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '(:b %d)
              a)
          |} i j k,
          Integer i
        ];
      end;

      addall begin
        let* i = [-4; 0; 10]
        and* j = [-2; 0; 5]
        and* k = [-1; 0; 2]
        in
        [
          Printf.sprintf {|
            (destructuring-bind ((:a a %d) (:b b %d)) '(:b %d)
              b)
          |} i j k,
          Integer k
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Bool true);
          ("d", String "hello");
          ("x", Symbol ":foo");
          ("y", Integer 8);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1")
              %s)
          |} variable,
          value
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Integer 2);
          ("d", String "hello");
          ("x", Symbol ":foo");
          ("y", Integer 8);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1" 2)
              %s)
          |} variable,
          value
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Integer 2);
          ("d", Bool false);
          ("x", Symbol ":foo");
          ("y", Integer 8);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1" 2 #f)
              %s)
          |} variable,
          value
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Integer 2);
          ("d", Bool false);
          ("x", Symbol ":qux");
          ("y", Integer 8);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1" 2 #f :foo :qux)
              %s)
          |} variable,
          value
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Integer 2);
          ("d", Bool false);
          ("x", Symbol ":qux");
          ("y", Integer 100);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1" 2 #f :foo :qux :bar 100)
              %s)
          |} variable,
          value
        ];
      end;

      addall begin
        let* variable, value = [
          ("a", Integer 0);
          ("b", String "1");
          ("c", Integer 2);
          ("d", Bool false);
          ("x", Symbol ":foo");
          ("y", Integer 100);
        ]
        in
        [
          Printf.sprintf {|
            (destructuring-bind (a b (c #t) (d "hello") (:foo x :foo) (:bar y 8)) '(0 "1" 2 #f :bar 100)
              %s)
          |} variable,
          value
        ];
      end;
    end
  in
  "define function" >::: List.map ~f:(Fn.uncurry test_run) test_cases


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
    define_macro_tests;
    destructuring_tests;
    keyword_tests;
  ]
