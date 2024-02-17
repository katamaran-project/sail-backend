open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C  = Converters
module P  = Value.Predicate

open Shared


let if_then_else =
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


let cond =
  let impl (args : Value.t list) =
    let=?? clauses = List.map ~f:C.(tuple2 value value) args
    in
    let rec evaluate_cond (clauses : (Value.t * Value.t) list) : Value.t Option.t EC.t =
      match clauses with
      | [] -> EV.return @@ Some (Value.Nil)
      | (condition, expression) :: clauses -> begin
          let* evaluated_condition = evaluate condition
          in
          if Value.truthy evaluated_condition
          then EV.lift ~f:Option.some @@ evaluate expression
          else evaluate_cond clauses
        end
    in
    evaluate_cond clauses
  in
  ("cond", M.mk_multimacro [ impl ])


let library env =
  EnvironmentBuilder.extend_environment env @@ fun { callable; _ } -> begin
    List.iter ~f:(Auxlib.uncurry callable) [
      if_then_else;
      cond;
    ] 
  end
