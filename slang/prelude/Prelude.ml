module Shared = Shared


let initialize : unit EvaluationContext.t =
  let libraries = [
      Arithmetic.initialize;
      Booleans.initialize;
      Functions.initialize;
      Comparison.initialize;
      Lists.initialize;
      Predicates.initialize;
      Quote.initialize;
      ControlFlow.initialize;
      IO.initialize;
  ]
  in
  EvaluationContext.iter ~f:(fun f -> f) libraries
