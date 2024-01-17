open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M  = Multimethods
module V  = Value


let addition args =
  let*  evaluated_args = EV.map evaluate args                 in
  let   ns             = List.map ~f:M.integer evaluated_args in
  let   result         = List.fold_left ~f:Int.(+) ~init:0 ns
  in
  EV.return @@ V.Integer result


let subtraction args =
  let* evaluated_args = EV.map evaluate args           in
  let  ns             = List.map ~f:M.integer evaluated_args
  in
  let result = match ns with
    | []    -> 0
    | [n]   -> -n
    | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
  in
  EV.return @@ V.Integer result


let multiplication args =
  let* evaluated_args = EV.map evaluate args                  in
  let  ns             = List.map ~f:M.integer evaluated_args
  in
  let result = List.fold_left ~f:Int.( * ) ~init:1 ns
  in
  EV.return @@ V.Integer result


let division args =
  let* evaluated_args = EV.map evaluate args           in
  let  ns             = List.map ~f:M.integer evaluated_args
  in
  let result = match ns with
    | []    -> raise @@ M.DispatchFailure
    | [_]   -> raise @@ M.DispatchFailure
    | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
  in
  EV.return @@ V.Integer result


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "+" addition;
      native_function "-" subtraction;
      native_function "*" multiplication;
      native_function "/" division;
    )
