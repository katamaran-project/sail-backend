open Base
open Evaluation
open Multimethods
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C = Converters

open Shared


let cons (args : Value.t list) =
  let impl args =
    let=? car, cdr = C.(map2 value value) args
    in
    EC.return @@ Option.some @@ Value.Cons (car, cdr)
  in
  mk_multimethod [ impl ] args


let car args =
  let impl args =
    let=? (car, _) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some car
  in
  mk_multimethod [ impl ] args


let cdr args =
  let impl args =
    let=? (_, cdr) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some cdr
  in
  mk_multimethod [ impl ] args


let any args =
  let impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.exists ~f:predicate items
  in
  mk_multimethod [ impl ] args


let all args =
  let impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.forall ~f:predicate items
  in
  mk_multimethod [ impl ] args


let contains args =
  let impl args =
    let=? list, value = C.(map2 (list value) value) args
    in
    EC.return @@ Option.some @@ Value.Mk.bool @@ List.exists ~f:(Value.equal value) list
  in
  mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "cons" cons;
      callable "car" car;
      callable "cdr" cdr;
      callable "any?" any;
      callable "all?" all;
      callable "contains?" contains
    )
