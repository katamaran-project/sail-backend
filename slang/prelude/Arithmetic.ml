open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M  = Multimethods

open Shared


let addition args =
  let add_integers evaluated_args =
    let=?? ns = List.map ~f:M.integer evaluated_args
    in
    let result = List.fold_left ~f:Int.(+) ~init:0 ns
    in
    EV.return @@ Some (Value.Integer result)

  and add_strings evaluated_args =
    let=?? strings = List.map ~f:M.string evaluated_args
    in
    let result = String.concat strings
    in
    EV.return @@ Some (Value.String result)

  in
  M.mk_multimethod [ add_integers; add_strings ] args


let subtraction args =
  let* evaluated_args = EV.map evaluate args
  in
  let=!! ns = List.map ~f:M.integer evaluated_args
  in
  let result = match ns with
    | []    -> 0
    | [n]   -> -n
    | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
  in
  EV.return @@ Value.Integer result


let multiplication args =
  let multiply_integers evaluated_args =
    let=?? ns = List.map ~f:M.integer evaluated_args
    in
    let result = List.fold_left ~f:Int.( * ) ~init:1 ns
    in
    EV.return @@ Some (Value.Integer result)

  and multiply_string_with_int evaluated_args =
    let=? string, integer = M.map2 M.string M.integer evaluated_args 
    in
    let result = String.concat @@ List.init integer ~f:(Fn.const string)
    in
    EV.return @@ Some (Value.String result)

  and multiply_int_with_string evaluated_args =
    let=? integer, string = M.map2 M.integer M.string evaluated_args
    in
    let result = String.concat @@ List.init integer ~f:(Fn.const string)
    in
    EV.return @@ Some (Value.String result)

  in
  M.mk_multimethod [
    multiply_integers;
    multiply_string_with_int;
    multiply_int_with_string;
  ] args


let division args =
  let* evaluated_args = EV.map evaluate args
  in
  let=!! ns = List.map ~f:M.integer evaluated_args
  in
  let result = match ns with
    | []
    | [_]   -> raise @@ Exception.SlangError "invalid division"
    | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
  in
  EV.return @@ Value.Integer result


let modulo args =
  let impl args =
    let=! (x, y) = M.(map2 integer integer) args
    in
    EV.return @@ Some (Value.Integer (x % y))
  in
  M.mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "+" addition;
      callable "-" subtraction;
      callable "*" multiplication;
      callable "/" division;
      callable "%" modulo;
    )
