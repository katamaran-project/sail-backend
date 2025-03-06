open Base
open OUnit2
open Monads


module IntList          = Dispenser.MakeListSource(struct type t = int end)
module IntListDispenser = Dispenser.Make(IntList)


let test_sum =
  "sum" >:: fun _ -> begin
                let open Monads.Notations.Star(IntListDispenser) in
                let open IntListDispenser
                in
                let source = [1; 2; 3; 4; 5]
                in
                let rec sum () =
                  let* n = current
                  in
                  match n with
                  | Some n -> let* () = next in let* rest = sum () in return @@ n + rest
                  | None   -> return 0
                in
                let (result, rest) = run (sum ()) source
                in
                assert_equal 15 result;
                assert_equal rest []
              end


let test_suite =
  "reader monad test suite" >::: [
      test_sum
  ]
