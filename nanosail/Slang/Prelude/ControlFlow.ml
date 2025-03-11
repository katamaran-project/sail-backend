open! ExtBase
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C  = Converters
module P  = Value.Predicate

open Shared


let if_then_else =
  let id = "if"

  and if_then args =
    let=? condition, then_clause = C.(map2 anything anything) args
    in
    let* evaluated_condition = evaluate condition
    in
    if Value.truthy evaluated_condition
    then EV.lift ~f:Option.some @@ evaluate then_clause
    else EV.return @@ Some (Value.Nil)
  in

  let if_then_else args =
    let=? condition, then_clause, else_clause = C.(map3 anything anything anything) args
    in
    let* evaluated_condition = evaluate condition
    in
    if Value.truthy evaluated_condition
    then EV.lift ~f:Option.some @@ evaluate then_clause
    else EV.lift ~f:Option.some @@ evaluate else_clause
  in

  (id, Functions.mk_multi_special_form [ if_then_else; if_then; error id ])


let cond =
  let id = "cond"

  and impl (args : Value.t list) =
    let=?? clauses = List.map ~f:C.(tuple2 anything anything) args
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

  (id, Functions.mk_multi_special_form [ impl; error id ])


let initialize =
  let definitions = [
    if_then_else;
    cond
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
