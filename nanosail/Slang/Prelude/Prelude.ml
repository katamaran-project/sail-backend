module Shared = Shared


let initialize : unit EvaluationContext.t =
  let libraries = [
      Arithmetic.initialize;
      Booleans.initialize;
      Lambda.initialize;
      Define.initialize;
      Macros.initialize;
      Comparison.initialize;
      ControlFlow.initialize;
      Predicates.initialize;
      Quote.initialize;
      IO.initialize;
      Lists.initialize;
      HeapFunctionality.initialize;
      Strings.initialize;
  ]
  in
  EvaluationContext.iter ~f:(fun f -> f) libraries
