open ExtBase
open Monads.Notations.Star(EvaluationContext)

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
  (id, Functions.mk_strict_function impl)


let read_refcell =
  let id = "@"
  and impl args =
    match args with
    | [ Value.Reference address ] -> begin
        let* value = EC.heap_access address
        in
        EC.return value
      end
    | [ nonreference ] -> EC.fail @@ "@ expects a reference as single argument; instead got " ^ (Value.to_string nonreference)
    | []               -> EC.fail @@ "@ expects a reference as single argument; instead got no arguments"
    | _                -> EC.fail @@ "@ expects a reference as single argument; instead got more arguments"
  in
  (id, Functions.mk_strict_function impl)


let write_refcell =
  let id = "@="
  and impl args =
    match args with
    | [ Value.Reference address; value ] -> begin
        let* () = EC.heap_update address value
        in
        EC.return Value.Nil
      end
    | _ -> EC.fail "@= expects a reference and a value"
  in
  (id, Functions.mk_strict_function impl)


let initialize =
  let definitions = [
    create_refcell;
    read_refcell;
    write_refcell;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
