open! ExtBase


let pp_sail_definition (sail_definition : Libsail.Type_check.typed_def) : PP.t =
  let str =
    StringOf.Sail.definition sail_definition
  in
  PP.from_multiline_string str
