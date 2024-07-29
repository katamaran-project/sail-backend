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


let read_refcell =
  let id = "@"
  and impl args =
    match args with
    | [ Value.Reference address ] -> begin
        let* value = EC.heap_access address
        in
        EC.return value
      end
    | _ -> EC.fail "@ expects a reference as single argument"
  in
  (id, impl)


let initialize =
  let definitions = [
    create_refcell;
    read_refcell;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
