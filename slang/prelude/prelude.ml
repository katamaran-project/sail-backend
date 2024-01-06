let prelude =
  Environment_builder.extend_environment Environment.empty (fun { extend; _ } ->
      extend Arithmetic.library;
      extend Functions.library;
    )
