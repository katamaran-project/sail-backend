open Base
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C = Converters

open Shared


let quote (args : Value.t list) : Value.t EV.t =
  let=! value = C.(map1 value) args
  in
  EV.return value


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "quote" quote;
    )
