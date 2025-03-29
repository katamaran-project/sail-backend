open ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let print =
  let id = "print"
  and impl args =
    Stdio.printf "%s\n" @@ String.concat ~sep:" " @@ List.map ~f:Value.to_string args;
    EC.return @@ Option.some @@ Value.Nil
  in
  (id, Functions.mk_multimethod [ impl; error id ])


let initialize =
  let definitions = [
    print;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
