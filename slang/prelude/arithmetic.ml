open Base
open Util


let addition args =
  let ns = List.map ~f:as_int args
  in
  let result = List.fold_left ~f:Int.(+) ~init:0 ns
  in
  Value.Integer result


let subtraction args =
  let ns = List.map ~f:as_int args
  in
  let result = match ns with
    | []    -> 0
    | [n]   -> -n
    | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
  in
  Value.Integer result


let multiplication args =
  let ns = List.map ~f:as_int args
  in
  let result = List.fold_left ~f:Int.( * ) ~init:1 ns
  in
  Value.Integer result


let division args =
  let ns = List.map ~f:as_int args
  in
  let result = match ns with
    | []    -> raise TypeError
    | [_]   -> raise TypeError
    | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
  in
  Value.Integer result


let library env =
  extend_environment env (fun { native_function; _ } ->
      native_function "+" addition;
      native_function "-" subtraction;
      native_function "*" multiplication;
      native_function "/" division;
    )
