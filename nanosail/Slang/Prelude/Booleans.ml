open! ExtBase
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C  = Converters

open Shared


let conjunction =
  let id = "and"
  in
  let rec impl args =
    match args with
    | []    -> EC.return @@ Value.Bool true
    | x::xs -> begin
        let* x' = evaluate x
        in
        if Value.truthy x'
        then impl xs
        else EC.return @@ Value.Bool false
      end

  in
  (id, impl)


let disjunction =
  let id = "or"
  in
  let rec impl args =
    match args with
    | []    -> EC.return @@ Value.Bool false
    | x::xs -> begin
        let* x' = evaluate x
        in
        if Value.truthy x'
        then EC.return @@ Value.Bool true
        else impl xs
      end
  in
  (id, impl)


let negation =
  let id = "not"
  in
  let impl args =
    let=? b = C.(map1 truthy) args
    in
    EC.return @@ Some (Value.Bool (not b))
  in
  (id, Functions.mk_multimethod [ impl; error id ])


let initialize =
  let definitions = [
    conjunction;
    disjunction;
    negation;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
