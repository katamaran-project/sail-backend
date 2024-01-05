open OUnit2

module V = Slang.Value
module T = Slang.Types



let tests =
  "type tests" >::: [
    "5" >:: (
      fun _ -> begin
          let value = V.Integer 5
          in
          let actual = T.integer value
          in
          assert_equal (Some 5) actual
        end
    );
    "a" >:: (
      fun _ -> begin
          let value = V.Symbol "a"
          in
          let actual = T.symbol value
          in
          assert_equal (Some "a") actual
        end
    );
    "a" >:: (
      fun _ -> begin
          let value = V.Symbol "a"
          in
          let actual = T.symbol value
          in
          assert_equal (Some "a") actual
        end
    );
    "(1)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Integer 1, V.Nil)
          in
          let actual = T.list T.integer value
          in
          assert_equal (Some [1]) actual
        end
    );
    "(1 2 3)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Integer 1, V.Cons (V.Integer 2, V.Cons (V.Integer 3, V.Nil)))
          in
          let actual = T.list T.integer value
          in
          assert_equal (Some [1; 2; 3]) actual
        end
    );
    "(a b c)" >:: (
      fun _ -> begin
          let value = V.Cons (V.Symbol "a", V.Cons (V.Symbol "bc", V.Cons (V.Symbol "xyz", V.Nil)))
          in
          let actual = T.list T.symbol value
          in
          assert_equal (Some ["a"; "bc"; "xyz"]) actual
        end
    );
  ]
