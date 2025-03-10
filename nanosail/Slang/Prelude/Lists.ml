open! ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


(* Helper function to return values successfully *)
let return = Fn.compose EC.return Option.some

(* Helper function to define functions using Slang code *)
let define = Fn.compose EC.ignore Evaluation.parse_and_evaluate_string

(* (cons x y) creates a pair with values x and y *)
let cons =
  let id = "cons"
  and impl args =
    let=? car, cdr = C.(map2 value value) args
    in
    return @@ Value.Cons (car, cdr)
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


(* (list x y z) is equivalent to (cons x (cons y (cons z nil))) *)
let list =
  let id = "list"
  and impl args =
    return @@ Value.list_to_cons args
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


(* (car pair) returns the first element of the given pair *)
let car =
  let id = "car"
  and impl args =
    let=? (car, _) = C.(map1 (cons value value)) args
    in
    return car
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


(* (cdr pair) returns the second element of the given pair *)
let cdr =
  let id = "cdr"
  and impl args =
    let=? (_, cdr) = C.(map1 (cons value value)) args
    in
    return cdr
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


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
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


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
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


(* (contains? list value) *)
let contains =
  let id = "contains?"
  and impl args =
    let=? list, value = C.(map2 (list value) value) args
    in
    return @@ Value.Mk.bool @@ List.exists ~f:(Value.equal value) list
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


let caar =
  EC.ignore @@ Evaluation.parse_and_evaluate_string "(define (caar x) (car (car x)))"

let cadr =
  EC.ignore @@ Evaluation.parse_and_evaluate_string "(define (cadr x) (car (cdr x)))"

let cdar =
  EC.ignore @@ Evaluation.parse_and_evaluate_string "(define (cdar x) (cdr (car x)))"

let cadar =
  EC.ignore @@ Evaluation.parse_and_evaluate_string "(define (cadar x) (car (cdr (car x))))"

let filter =
  define {|
      (define (filter pick? xs)
        (cond ((nil? xs)
                  ())
              ((pick? (car xs))
                  (cons (car xs)
                        (recurse pick? (cdr xs))))
              (#t
                  (recurse pick? (cdr xs)))))
    |}

let nth =
  define {|
      (define (nth index xs)
        (if (= index 0)
            (car xs)
            (recurse (- index 1)
                     (cdr xs))))
    |}

let last =
  define {|
      (define (last xs)
        (if (nil? (cdr xs))
            (car xs)
            (recurse (cdr xs))))
    |}

let range =
  let id = "range"
  and impl args =
    let=? start, stop = C.(map2 integer integer) args
    in
    EC.return @@ Option.some @@ Value.list_to_cons @@ List.map ~f:Value.Mk.integer @@ List.range ~start:`inclusive ~stop:`exclusive start stop
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]
  

let initialize =
  let definitions = [
    cons;
    list;
    car;
    cdr;
    caar;
    cadr;
    cdar;
    cadar;
    any;
    all;
    contains;
    filter;
    nth;
    last;
    range;
  ]
  in
  EC.ignore @@ EC.sequence definitions
