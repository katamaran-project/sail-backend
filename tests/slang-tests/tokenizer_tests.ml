open OUnit2
open Slang


let tests =
  let test_cases =
    [
      ("(", [ TLeftParenthesis ]);
      (")", [ TRightParenthesis ]);
      ("()", [ TLeftParenthesis; TRightParenthesis ]);
      ("\"abc\"", [ TString "abc" ]);
      ("#t", [ TTrue ]);
      ("#f", [ TFalse ]);
      ("#t #f", [ TTrue; TFalse ]);
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
