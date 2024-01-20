open Base
open Evaluation
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)

module M = Multimethods
module V = Value

(* open Shared *)


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



(* let less_than args = *)
(*   let less_than_ints ns = *)
(*     V.Bool (List.for_all ~f:(Auxlib.uncurry (<)) @@ Auxlib.consecutive_overlapping_pairs ns) *)
(*   in *)
(*   let less_than_strings strings = *)
(*     V.Bool (List.for_all ~f:(Auxlib.uncurry (String.(<))) @@ Auxlib.consecutive_overlapping_pairs strings) *)
(*   in *)
(*   let* evaluated_args = map evaluate args *)
(*   in *)
(*   (T.map T.integer, less_than_ints) <||> (T.map T.string, less_than_strings) @@ evaluated_args *)




let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "=" equality_check;
    )
