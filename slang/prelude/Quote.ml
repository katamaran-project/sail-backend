open Base
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let quote =
  let id = "quote"
  in
  let impl args =
    let=! value = C.(map1 value) args
    in
    EV.return value
  in
  (id, impl)


let initialize =
  let definitions = [
    quote;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
