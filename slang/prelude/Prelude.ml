module Shared = Shared


let initialize : unit EvaluationContext.t =
  let libraries = [
      Arithmetic.initialize;
      Booleans.initialize;
      Functions.initialize;
      Comparison.initialize;
      ControlFlow.initialize;
      Predicates.initialize;
      Quote.initialize;
      IO.initialize;
      Lists.initialize;
  ]
  in
  EvaluationContext.iter ~f:(fun f -> f) libraries
