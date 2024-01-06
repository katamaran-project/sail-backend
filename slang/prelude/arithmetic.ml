open Base
open Util
open Evaluation
open Evaluation_context
open Evaluation_context.Notations


module T = Types
module V = Value

open T.Notations


let addition args =
  let* evaluated_args = map evaluate args
  in
  let=! ns = T.map T.integer evaluated_args
  in
  let result = List.fold_left ~f:Int.(+) ~init:0 ns
  in
  return @@ V.Integer result


let subtraction args =
  let* evaluated_args = map evaluate args
  in
  let=! ns = T.map T.integer evaluated_args
  in
  let result = match ns with
    | []    -> 0
    | [n]   -> -n
    | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
  in
  return @@ V.Integer result


let multiplication args =
  let* evaluated_args = map evaluate args
  in
  let=! ns = T.map T.integer evaluated_args
  in
  let result = List.fold_left ~f:Int.( * ) ~init:1 ns
  in
  return @@ V.Integer result


let division args =
  let* evaluated_args = map evaluate args
  in
  let=! ns = T.map T.integer evaluated_args
  in
  let result = match ns with
    | []    -> raise T.TypeError
    | [_]   -> raise T.TypeError
    | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
  in
  return @@ V.Integer result


let library env =
  extend_environment env (fun { native_function; _ } ->
      native_function "+" addition;
      native_function "-" subtraction;
      native_function "*" multiplication;
      native_function "/" division;
    )
