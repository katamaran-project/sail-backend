open Base
open Evaluation
open Multimethods
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module C = Converters

open Shared


(* (cons x y) creates a pair with values x and y *)
let cons =
  let impl args =
    let=? car, cdr = C.(map2 value value) args
    in
    EC.return @@ Option.some @@ Value.Cons (car, cdr)
  in
  ("cons", mk_multimethod [ impl ])


(* (car pair) returns the first element of the given pair *)
let car =
  let impl args =
    let=? (car, _) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some car
  in
  ("car", mk_multimethod [ impl ])


(* (cdr pair) returns the second element of the given pair *)
let cdr =
  let impl args =
    let=? (_, cdr) = C.(map1 (cons value value)) args
    in
    EC.return @@ Some cdr
  in
  ("cdr", mk_multimethod [ impl ])


(* (any? predicate list) *)
let any =
  let impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.exists ~f:predicate items
  in
  ("any?", mk_multimethod [ impl ])


(* (all? predicate list) *)
let all =
  let impl args =
    let=? predicate, items = C.(map2 callable (list value)) args
    in
    let predicate arg = EC.lift ~f:Value.truthy @@ predicate [ arg ]
    in
    EC.lift ~f:(Fn.compose Option.some Value.Mk.bool) @@ EC.forall ~f:predicate items
  in
  ("all?", mk_multimethod [ impl ])


(* (contains? list value) *)
let contains =
  let impl args =
    let=? list, value = C.(map2 (list value) value) args
    in
    EC.return @@ Option.some @@ Value.Mk.bool @@ List.exists ~f:(Value.equal value) list
  in
  ("contains?", mk_multimethod [ impl ])


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      List.iter
        ~f:(Auxlib.uncurry callable)
        [ cons; car; cdr; any; all; contains ]
    )
