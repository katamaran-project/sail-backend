open Base
open OUnit2
open Slang.Tokenizer


let tokenizer_tests =
  let test_cases =
    [
      ("(", [ TLeftParenthesis ]);
      (")", [ TRightParenthesis ]);
      ("()", [ TLeftParenthesis; TRightParenthesis ]);
      ("\"abc\"", [ TString "abc" ]);
      ("#t", [ TTrue ]);
      ("#f", [ TFalse ]);
      ("#t #f", [ TTrue; TFalse ]);
      ("+", [ TSymbol "+" ]);
      ("-", [ TSymbol "-" ]);
      ("=", [ TSymbol "=" ]);
      ("123", [ TInteger 123 ]);
      ("-123", [ TInteger (-123) ]);
      ("1+", [ TSymbol "1+" ]);
      ("; 1", [ ]);
      ("1 ; 2\n3", [ TInteger 1; TInteger 3 ]);
      ("(= (1+ x) (foo 1))", [
          TLeftParenthesis;
          TSymbol "=";
          TLeftParenthesis;
          TSymbol "1+";
          TSymbol "x";
          TRightParenthesis;
          TLeftParenthesis;
          TSymbol "foo";
          TInteger 1;
          TRightParenthesis;
          TRightParenthesis
        ]);
    ]
  in
  let test_tokenize (input, expected) =
    input >:: fun _ -> begin
        let actual = Sequence.to_list @@ tokenize_string input
        in
        assert_equal expected actual
      end
  in
  "tokenizer tests" >::: List.map ~f:test_tokenize test_cases


let tests =
  "tokenizing tests" >::: [
    tokenizer_tests;
  ]
