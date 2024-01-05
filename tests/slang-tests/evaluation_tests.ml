open Base
open OUnit2


let evaluation_tests =
  let test_cases =
    [
      ("5", [ Slang.Value.Integer 5 ]);
      ("(+)", [ Slang.Value.Integer 0 ]);
      ("(+ 1)", [ Slang.Value.Integer 1 ]);
      ("(+ 1 2)", [ Slang.Value.Integer 3 ]);
      ("(+ 7 8 10)", [ Slang.Value.Integer 25 ]);
      ("(-)", [ Slang.Value.Integer 0 ]);
      ("(- 5)", [ Slang.Value.Integer (-5) ]);
      ("(- 5 1)", [ Slang.Value.Integer 4 ]);
      ("(- 5 1 2)", [ Slang.Value.Integer 2 ]);
      ("(*)", [ Slang.Value.Integer 1 ]);
      ("(* 2)", [ Slang.Value.Integer 2 ]);
      ("(* 2 3)", [ Slang.Value.Integer 6 ]);
      ("(/ 10 2)", [ Slang.Value.Integer 5 ]);
      ("(/ 9 2)", [ Slang.Value.Integer 4 ]);
      ("(/ 100 2 2)", [ Slang.Value.Integer 25 ]);
    ]
  in
  let test_run (input, expected) =
    input >:: fun _ -> begin
        let asts = Slang.Parser.parse_string input
        in
        let (actual, _)  = Slang.Evaluation.run asts
        in
        assert_equal expected actual
      end
  in
  "run tests" >::: List.map ~f:test_run test_cases


let tests =
  "evaluation tests" >::: [
    evaluation_tests;
  ]
