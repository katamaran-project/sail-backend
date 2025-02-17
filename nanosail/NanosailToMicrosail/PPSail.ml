open! ExtBase


let string_of_pprint_document (document : PPrint.document) : string =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer


(* todo remove duplication (see StringOf.Sail.definition) *)
let pp_sail_definition (sail_definition : Libsail.Type_check.typed_def) : PP.document =
  let document =
    Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  let str =
    String.rstrip @@ string_of_pprint_document document
  in
  let lines =
    List.map ~f:String.rstrip @@ String.split_lines str
  in
  PP.annotate [%here] @@ PP.vertical @@ List.map ~f:PP.string lines
