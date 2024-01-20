module Shared = Shared

let prelude =
  EnvironmentBuilder.extend_environment Environment.empty (fun { extend; _ } ->
      extend Arithmetic.library;
      extend Functions.library;
      extend Comparison.library;
      extend Lists.library;
      extend Predicates.library;
      extend Quote.library;
      extend ControlFlow.library;
    )
