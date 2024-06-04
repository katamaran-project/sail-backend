open Base
open OUnit2

module PP = Nanosail.NanosailToMicrosail.PP

module StringOutput : PP.Output with type t = string = struct
  type t = string

  let parenthesize = Printf.sprintf "(%s)"
end

module Prec = struct
  include PP.PrecedenceFormatter(StringOutput)

  let int = Fn.compose define_atom Int.to_string
  let add = define_binary_operator 1 (fun x y -> Printf.sprintf "%s + %s" x y)
  let sub = define_binary_operator 1 (fun x y -> Printf.sprintf "%s - %s" x y)
  let mul = define_binary_operator 2 (fun x y -> Printf.sprintf "%s * %s" x y)
  let div = define_binary_operator 2 (fun x y -> Printf.sprintf "%s / %s" x y)
  let pow = define_binary_operator 3 (fun x y -> Printf.sprintf "%s ^ %s" x y)
  
  let to_string = output_of
end

let test_formatting =
  let test (ast, expected) =
    let f _ =
      let actual = Prec.to_string ast
      in
      let msg    = Printf.sprintf "Expected %s, got %s" expected actual
      and cmp    = String.equal
      in
      OUnit2.assert_equal ~msg ~cmp expected actual
    in
    expected >:: f
  in
  let test_cases =
    let open Prec in
    [
      (int 1, "1");
      (int 5, "5");
      (add (int 1) (int 2), "1 + 2");
      (add (int 1) (add (int 2) (int 3)), "1 + 2 + 3");
      (add (add (int 1) (int 2)) (int 3), "1 + 2 + 3");
    ]
  in
  "formatting" >::: List.map ~f:test test_cases

let test_suite =
  "precedence formatter test suite" >::: [
      test_formatting
    ]

