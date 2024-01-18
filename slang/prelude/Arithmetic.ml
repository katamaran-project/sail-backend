open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M  = Multimethods
module V  = Value


let addition args =
  let add_integers evaluated_args =
    let ns = List.map ~f:M.integer evaluated_args
    in
    let result = List.fold_left ~f:Int.(+) ~init:0 ns
    in
    EV.return @@ V.Integer result
      
  and add_strings evaluated_args =
    let strings = List.map ~f:M.string evaluated_args
    in
    let result = String.concat strings
    in
    EV.return @@ V.String result

  in
  let*  evaluated_args = EV.map evaluate args
  in
  M.combine [ add_integers; add_strings ] evaluated_args


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
