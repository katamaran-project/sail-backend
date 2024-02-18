open Base
open Evaluation
open Multimethods
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C = Converters

open Shared


(* (cons x y) creates a pair with values x and y *)
let cons =
  let id = "cons"
  and impl args =
    let=? car, cdr = C.(map2 value value) args
    in
    EC.return @@ Option.some @@ Value.Cons (car, cdr)
  in
  (id, mk_multimethod id [ impl ])


(* (car pair) returns the first element of the given pair *)
let car =
  let id = "car"
  and impl args =
    let=? (car, _) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some car
  and error args =
    let error_message =
      Printf.sprintf "error evaluating (car %s)" (String.concat ~sep:" " @@ List.map ~f:Value.to_string args)
    in
    raise @@ Exception.SlangError error_message
  in
  (id, mk_multimethod id [ impl; error ])


(* (cdr pair) returns the second element of the given pair *)
let cdr =
  let id = "cdr"
  and impl args =
    let=? (_, cdr) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some cdr
  in
  (id, mk_multimethod id [ impl ])


(* (any? predicate list) *)
let any =
  let id = "any?"
  and impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.exists ~f:predicate items
  in
  (id, mk_multimethod id [ impl ])


(* (all? predicate list) *)
let all =
  let id = "all?"
  and impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.forall ~f:predicate items
  in
  (id, mk_multimethod id [ impl ])


(* (contains? list value) *)
let contains =
  let id = "contains?"
  and impl args =
    let=? list, value = C.(map2 (list value) value) args
    in
    EC.return @@ Option.some @@ Value.Mk.bool @@ List.exists ~f:(Value.equal value) list
  in
  (id, mk_multimethod id [ impl ])


let library env =
  let definitions = [
    cons;
    car;
    cdr;
    any;
    all;
    contains;
  ]
  in
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      List.iter
        ~f:(Auxlib.uncurry callable)
        definitions
    )

let initialize =
  let definitions = [
    cons;
    car;
    cdr;
    any;
    all;
    contains;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
