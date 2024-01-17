open OUnit2

module V = Slang.Value
module M = Slang.Multimethods



let tests =
  "type tests" >::: [
    "5" >:: (
      fun _ -> begin
          let value = V.Integer 5
          in
          let actual = M.integer value
          in
          assert_equal 5 actual
        end
    );
    "a" >:: (
      fun _ -> begin
          let value = V.Symbol "a"
          in
          let actual = M.symbol value
          in
          assert_equal "a" actual
        end
    );
    "a" >:: (
      fun _ -> begin
          let value = V.Symbol "a"
          in
          let actual = M.symbol value
          in
          assert_equal "a" actual
        end
    );
    "(1)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Integer 1, V.Nil)
          in
          let actual = M.cons M.integer M.nil value
          in
          assert_equal (1, ()) actual
        end
    );
    "()" >:: (
      fun _ -> begin
          let value = V.Nil
          in
          let actual = M.list M.integer value
          in
          assert_equal [] actual
        end
    );
    "(1)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Integer 1, V.Nil)
          in
          let actual = M.list M.integer value
          in
          assert_equal [1] actual
        end
    );
    "(1 2 3)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Integer 1, V.Cons (V.Integer 2, V.Cons (V.Integer 3, V.Nil)))
          in
          let actual = M.list M.integer value
          in
          assert_equal [1; 2; 3] actual
        end
    );
    "(a b c)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Symbol "a", V.Cons (V.Symbol "bc", V.Cons (V.Symbol "xyz", V.Nil)))
          in
          let actual = M.list M.symbol value
          in
          assert_equal ["a"; "bc"; "xyz"] actual
        end
    );
  ]
