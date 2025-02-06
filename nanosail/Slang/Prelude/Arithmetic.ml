open Base
open! ExtBase
open Evaluation
open Monads.Notations.Star(EvaluationContext)
open Functions

module EV = EvaluationContext
module C  = Converters

open Shared


let addition =
  let id = "+"

  and add_integers evaluated_args =
    let=?? ns = List.map ~f:C.integer evaluated_args
    in
    let result = List.fold_left ~f:Int.(+) ~init:0 ns
    in
    EC.return @@ Some (Value.Integer result)

  and add_strings evaluated_args =
    let=?? strings = List.map ~f:C.string evaluated_args
    in
    let result = String.concat strings
    in
    EC.return @@ Some (Value.String result)

  in
  (id, mk_multimethod [ add_integers; add_strings; error id ])


let subtraction =
  let id = "-"

  and impl args =
    let* evaluated_args = EC.map ~f:evaluate args
    in
    let=!! ns = List.map ~f:C.integer evaluated_args
    in
    let result = match ns with
      | []    -> 0
      | [n]   -> -n
      | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
    in
    EC.return @@ Value.Integer result
  in
  (id, impl)


let multiplication =
  let id = "*"

  and multiply_integers evaluated_args =
    let=?? ns = List.map ~f:C.integer evaluated_args
    in
    let result = List.fold_left ~f:Int.( * ) ~init:1 ns
    in
    EC.return @@ Some (Value.Integer result)

  and multiply_string_with_int evaluated_args =
    let=? string, integer = C.map2 C.string C.integer evaluated_args
    in
    let result = String.concat @@ List.init integer ~f:(Fn.const string)
    in
    EC.return @@ Some (Value.String result)

  and multiply_int_with_string evaluated_args =
    let=? integer, string = C.map2 C.integer C.string evaluated_args
    in
    let result = String.concat @@ List.init integer ~f:(Fn.const string)
    in
    EC.return @@ Some (Value.String result)

  in
  (id, mk_multimethod [
      multiply_integers;
      multiply_string_with_int;
      multiply_int_with_string;
      error id;
    ])


let division =
  let id = "/"

  and impl args =
    let* evaluated_args = EC.map ~f:evaluate args
    in
    let=!! ns = List.map ~f:C.integer evaluated_args
    in
    let result = match ns with
      | []
      | [_]   -> raise @@ Exception.SlangError "invalid division"
      | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
    in
    EC.return @@ Value.Integer result
  in
  (id, impl)


let modulo =
  let id = "%"
  in
  let impl args =
    let=! (x, y) = C.(map2 integer integer) args
    in
    EC.return @@ Some (Value.Integer (x % y))
  in
  (id, mk_multimethod [ impl; error id ])


let initialize =
  let definitions = [
    addition;
    subtraction;
    multiplication;
    division;
    modulo;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
