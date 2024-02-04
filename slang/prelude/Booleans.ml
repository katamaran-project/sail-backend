open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C  = Converters

open Shared


let rec conjunction args =
  match args with
  | []    -> EC.return @@ Value.Bool true
  | x::xs -> begin
      let* x' = evaluate x
      in
      if Value.truthy x'
      then conjunction xs
      else EC.return @@ Value.Bool false
    end


let rec disjunction args =
  match args with
  | []    -> EC.return @@ Value.Bool false
  | x::xs -> begin
      let* x' = evaluate x
      in
      if Value.truthy x'
      then EC.return @@ Value.Bool true
      else disjunction xs
    end


let negation args =
  let impl args =
    let=? b = C.(map1 truthy) args
    in
    EC.return @@ Some (Value.Bool (not b))
  in
  M.mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "and" conjunction;
      callable "or"  disjunction;
      callable "not" negation;
    )
