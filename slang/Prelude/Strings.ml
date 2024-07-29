open Base
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


(* (cons x y) creates a pair with values x and y *)
let join =
  let id = "join"
  and impl args =
    let=? separator, strings = C.(map2 string (list string)) args
    in
    let result =
      String.concat ~sep:separator strings
    in
    EC.return @@ Option.some @@ Value.String result
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


let initialize =
  let definitions = [
    join;
  ]
  in
  EC.ignore @@ EC.sequence definitions
