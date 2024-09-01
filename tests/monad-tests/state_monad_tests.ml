open OUnit2


module IntState = Monads.State.Make(Int)

open Monads.Notations.Star(IntState)
open Monads.Util.Make(IntState)


let increment =
  let open IntState
  in
  let* n = get
  in
  put @@ n + 1


let get_and_inc =
  let open IntState
  in
  let* result = get
  in
  let* _ = increment
  in
  return result


let test_collect =
  "collect get_and_inc" >:: fun _ ->
    let (result, state) = IntState.run (repeat 5 ~f:get_and_inc) 0
    in
    assert_equal result [0; 1; 2; 3; 4];
    assert_equal state 5


let test_act =
  let open IntState
  in
  let flag = ref false
  in
  let set_flag () =
    flag := true
  in  
  "act" >:: fun _ ->
    let monadic =
      let* () = act set_flag
      in
      return 0
    in
    assert_equal false (!flag);
    let _ = IntState.run monadic 0
    in
    assert_equal true (!flag)
      

let test_suite =
  "state monad test suite" >::: [
    test_collect;
    test_act;
  ]
