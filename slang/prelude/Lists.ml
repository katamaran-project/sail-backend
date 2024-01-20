open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)


module EV = EvaluationContext
module M = Multimethods

open Shared


let cons (args : Value.t list) =
  let impl args =
    let=? car, cdr = M.(map2 value value) args
    in
    EC.return @@ Some (Value.Cons (car, cdr))
  in
  M.mk_multimethod [ impl ] args


let car args =
  let impl args =
    let=? (car, _) = M.(map1 (cons value value)) args
    in
    EC.return @@ Some car
  in
  M.mk_multimethod [ impl ] args


let cdr args =
  let impl args =
    let=? (_, cdr) = M.(map1 (cons value value)) args
    in
    EC.return @@ Some cdr
  in
  M.mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "cons" cons;
      callable "car" car;
      callable "cdr" cdr;
    )
