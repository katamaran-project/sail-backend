open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)
open Functions

module EV = EvaluationContext
module C  = Converters

open Shared


let create_refcell =
  let id = "ref"
  and impl args =
    match args with
    | [ value ] -> begin
        let* address = EC.heap_allocate value
        in
        EC.return @@ Value.Mk.reference address
      end
    | _ -> EC.fail "ref expects one argument"
  in
  (id, impl)


let initialize =
  let definitions = [
    create_refcell;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
