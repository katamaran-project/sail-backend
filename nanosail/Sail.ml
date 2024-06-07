type type_annotation = Libsail.Type_check.tannot

type sail_definition = type_annotation Libsail.Ast.def

let string_of_sail_definition
      ?(buffer_initial_size = 1000)
      ?(line_width = 160)
      ?(ribbon_width = 1.0)
      sail_definition =
  let buffer = Buffer.create buffer_initial_size
  in
  let document = Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  PPrint.ToBuffer.pretty ribbon_width line_width buffer document;
  Buffer.contents buffer
