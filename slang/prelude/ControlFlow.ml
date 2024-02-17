open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C  = Converters
module P  = Value.Predicate

open Shared


let conditional =
  let if_then args =
    let=? condition, then_clause = C.(map2 value value) args
    in
    let* evaluated_condition = evaluate condition
    in
    if Value.truthy evaluated_condition
    then EV.lift ~f:Option.some @@ evaluate then_clause
    else EV.return @@ Some (Value.Nil)
  in

  let if_then_else args =
    let=? condition, then_clause, else_clause = C.(map3 value value value) args
    in
    let* evaluated_condition = evaluate condition
    in
    if Value.truthy evaluated_condition
    then EV.lift ~f:Option.some @@ evaluate then_clause
    else EV.lift ~f:Option.some @@ evaluate else_clause
  in

  ("if", M.mk_multimacro [ if_then_else; if_then ])


let library env =
  EnvironmentBuilder.extend_environment env @@ fun { callable; _ } -> begin
      List.iter ~f:(Auxlib.uncurry callable) [ conditional ] 
    end
