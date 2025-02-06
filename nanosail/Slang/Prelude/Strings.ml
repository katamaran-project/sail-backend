open! ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let join =
  let id = "join"
  and impl args =
    let=? separator, strings = C.(map2 string (list string)) args
    in
    let result =
      Value.String (String.concat ~sep:separator strings)
    in
    EC.return @@ Some result
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


let starts_with =
  let id = "string-starts-with?"
  and impl args =
    let=? prefix, string = C.(map2 string string) args
    in
    let result =
      Value.Bool (String.is_prefix ~prefix string)
    in
    EC.return @@ Some result
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


let ends_with =
  let id = "string-ends-with?"
  and impl args =
    let=? suffix, string = C.(map2 string string) args
    in
    let result =
      Value.Bool (String.is_suffix ~suffix string)
    in
    EC.return @@ Some result
  in
  bind_callable id @@ Functions.mk_multimethod [ impl; error id ]


let initialize =
  let definitions = [
      join;
      starts_with;
      ends_with
  ]
  in
  EC.ignore @@ EC.sequence definitions
