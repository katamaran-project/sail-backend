open Nanosail.ExtBase
open OUnit2

module Slang = Nanosail.Slang
open Slang.Tokenizing
module T = Slang.Token


let tokenizer_tests =
  let test_cases =
    [
      ("(", [ T.LeftParenthesis ]);
      (")", [ T.RightParenthesis ]);
      ("'", [ T.Quote ]);
      ("'5", [ T.Quote; T.Integer 5 ]);
      ("()", [ T.LeftParenthesis; T.RightParenthesis ]);
      ("\"abc\"", [ T.String "abc" ]);
      ("#t", [ T.True ]);
      ("#f", [ T.False ]);
      ("#t #f", [ T.True; T.False ]);
      ("+", [ T.Symbol "+" ]);
      ("-", [ T.Symbol "-" ]);
      ("=", [ T.Symbol "=" ]);
      ("123", [ T.Integer 123 ]);
      ("-123", [ T.Integer (-123) ]);
      ("1+", [ T.Symbol "1+" ]);
      ("; 1", [ ]);
      ("1 ; 2\n3", [ T.Integer 1; T.Integer 3 ]);
      ("'((1 2))", T.([
           Quote;
           LeftParenthesis;
           LeftParenthesis;
           Integer 1;
           Integer 2;
           RightParenthesis;
           RightParenthesis
         ]));
      ("(= (1+ x) (foo 1))", [
          T.LeftParenthesis;
          T.Symbol "=";
          T.LeftParenthesis;
          T.Symbol "1+";
          T.Symbol "x";
          T.RightParenthesis;
          T.LeftParenthesis;
          T.Symbol "foo";
          T.Integer 1;
          T.RightParenthesis;
          T.RightParenthesis
        ]);
    ]
  in
  let test_tokenize (input, expected) =
    input >:: fun _ -> begin
        let actual = Sequence.to_list @@ Sequence.map ~f:Tuple.Triple.third @@ tokenize_string input
        in
        assert_equal expected actual
      end
  in
  "tokenizer tests" >::: List.map ~f:test_tokenize test_cases


let tests =
  "tokenizing tests" >::: [
    tokenizer_tests;
  ]
