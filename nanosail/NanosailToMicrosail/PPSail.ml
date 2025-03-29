let pp_sail_definition (sail_definition : Libsail.Type_check.typed_def) : PP.t =
  PP.string @@ StringOf.Sail.definition sail_definition
