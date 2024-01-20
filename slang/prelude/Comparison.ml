open Base
open Evaluation
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)

module M = Multimethods
module V = Value

open Shared


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


let less_than args =
  let less_than_ints args =
    let=?? ns = List.map ~f:M.integer args
    in
    let result = Value.Bool (List.for_all ~f:(Auxlib.uncurry (<)) @@ Auxlib.consecutive_overlapping_pairs ns)
    in
    EC.return @@ Some result
  in
  let less_than_strings args =
    let=?? ns = List.map ~f:M.string args
    in
    let result = Value.Bool (List.for_all ~f:(Auxlib.uncurry (String.(<))) @@ Auxlib.consecutive_overlapping_pairs ns)
    in
    EC.return @@ Some result
  in
  M.mk_multimethod [ less_than_ints; less_than_strings ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "=" equality_check;
      native_function "<" equality_check;
    )
