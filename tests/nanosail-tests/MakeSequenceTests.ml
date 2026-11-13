open Base
open OUnit2
open Nanosail
open Shared


(*
   Compares structurally, without normalization: the nesting of Seq is what is being tested.
*)
let assert_equal_statements_exactly
    (expected : Ast.Statement.t)
    (actual   : Ast.Statement.t) : unit
  =
  assert_equal
    ~pp_diff:(pp_diff Ast.Statement.to_fexpr)
    ~cmp:Ast.Statement.equal
    expected
    actual


let test_make_sequence =
  let open Ast.Statement in
  let test_cases : (string * t list * t) list =
    [
      (
        "single statement",
        [ mkstm 1 ],
        mkstm 1
      );
      (
        "two statements",
        [ mkstm 1; mkstm 2 ],
        Seq (mkstm 1, mkstm 2)
      );
      (
        "three statements",
        [ mkstm 1; mkstm 2; mkstm 3 ],
        Seq (mkstm 1, Seq (mkstm 2, mkstm 3))
      );
      (
        "four statements",
        [ mkstm 1; mkstm 2; mkstm 3; mkstm 4 ],
        Seq (mkstm 1, Seq (mkstm 2, Seq (mkstm 3, mkstm 4)))
      );
    ]
  in
  let run_test (description, statements, expected) =
    description >:: fun _ -> assert_equal_statements_exactly expected (make_sequence statements)
  in
  let empty_list_test =
    "empty list fails" >:: fun _ ->
      assert_raises
        (Failure "Cannot build a sequence of zero statements")
        (fun () -> make_sequence [])
  in
  "make_sequence" >::: (empty_list_test :: List.map ~f:run_test test_cases)


let test_suite =
  "make sequence" >::: [
    test_make_sequence;
  ]
