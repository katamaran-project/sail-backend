open Evaluation

module EC = EvaluationContext
open Monads.Notations.Star(EC)


let strict_function f =
  let wrapper arguments =
    let* evaluated_arguments = EC.map ~f:evaluate arguments
    in
    f evaluated_arguments
  in
  Value.Mk.callable wrapper
