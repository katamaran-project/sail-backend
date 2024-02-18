module Shared = Shared

let prelude =
  EnvironmentBuilder.extend_environment Environment.empty (fun { extend; _ } ->
      extend Arithmetic.library;
      extend Booleans.library;
      extend Functions.library;
      extend Comparison.library;
      extend Lists.library;
      extend Predicates.library;
      extend Quote.library;
      extend ControlFlow.library;
      extend IO.library;
    )


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
