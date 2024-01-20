open Base
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M = Multimethods

open Shared


let quote (args : Value.t list) : Value.t EV.t =
  let=! value = M.(map1 value) args
  in
  EV.return value


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "quote" quote;
    )
