open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)


module EV = EvaluationContext
module M = Multimethods

open Shared


let is_cons (args : Value.t list) =
  let impl args =
    let=? v = M.(map1 value) args
    in
    match v with
    | Value.Cons (_, _) -> EC.return @@ Some (Value.Bool true)
    | _                 -> EC.return @@ Some (Value.Bool false)
  in
  M.mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "cons?" is_cons;
    )
