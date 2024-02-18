open Base
open Multimethods
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
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
  bind_callable id @@ mk_multimethod [ impl ]


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
  bind_callable id @@ mk_multimethod [ impl; error ]


(* (cdr pair) returns the second element of the given pair *)
let cdr =
  let id = "cdr"
  and impl args =
    let=? (_, cdr) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some cdr
  in
  bind_callable id @@ mk_multimethod [ impl ]


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
  bind_callable id @@ mk_multimethod [ impl ]


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
  bind_callable id @@ mk_multimethod [ impl ]


(* (contains? list value) *)
let contains =
  let id = "contains?"
  and impl args =
    let=? list, value = C.(map2 (list value) value) args
    in
    EC.return @@ Option.some @@ Value.Mk.bool @@ List.exists ~f:(Value.equal value) list
  in
  bind_callable id @@ mk_multimethod [ impl ]


let caar =
  EC.ignore @@ Evaluation.evaluate_string "(define (caar x) (car (car x)))"

let cadr =
  EC.ignore @@ Evaluation.evaluate_string "(define (cadr x) (car (cdr x)))"

let cdar =
  EC.ignore @@ Evaluation.evaluate_string "(define (cdar x) (cdr (car x)))"

let cadar =
  EC.ignore @@ Evaluation.evaluate_string "(define (cadar x) (car (cdr (car x))))"


let initialize =
  let definitions = [
    cons;
    car;
    cdr;
    caar;
    cadr;
    cdar;
    cadar;
    any;
    all;
    contains;
  ]
  in
  EC.sequence definitions
