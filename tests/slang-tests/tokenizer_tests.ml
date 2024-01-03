open OUnit2
open Slang.Tokenizer


let raw_tokenizer_tests =
  let test_cases =
    [
      ("", []);
      (" ", []);
      ("\n\t \r", []);
      ("(", ["("]);
      (")", [")"]);
      ("((", ["("; "("]);
      ("))", [")"; ")"]);
      ("()", ["("; ")"]);
      ("abc", ["abc"]);
      ("abc xyz", ["abc"; "xyz"]);
      ("(abc xyz)", ["("; "abc"; "xyz"; ")"]);
    ]
  in
  let test_raw_tokenize (input, expected) =
    input >:: fun _ -> begin
        let actual = List.of_seq @@ raw_tokenize @@ String.to_seq input
        in
        assert_equal expected actual
      end
  in
  "raw tokenizer tests" >::: List.map test_raw_tokenize test_cases


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
      (* ("+", [ TSymbol "+" ]); *)
      (* ("-", [ TSymbol "-" ]); *)
      (* ("123", [ TInteger 123 ]); *)
    ]
  in
  let test_tokenize (input, expected) =
    input >:: fun _ -> begin
        let actual = List.of_seq @@ tokenize @@ String.to_seq input
        in
        assert_equal expected actual
      end
  in
  "tokenizer tests" >::: List.map test_tokenize test_cases


let tests =
  "tokenizing tests" >::: [
    raw_tokenizer_tests;
    tokenizer_tests;
  ]
