open Base
open Evaluation
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)


module T = Types
module V = Value


let equality_check args =
  let* evaluated_args = map evaluate args
  in
  let rec aux values =
    match values with
    | []       -> return @@ V.Bool true
    | [_]      -> return @@ V.Bool true
    | x::y::xs ->
      if not (V.equal x y)
      then return @@ V.Bool false
      else aux @@ y::xs
  in
  aux evaluated_args


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "=" equality_check;
    )
