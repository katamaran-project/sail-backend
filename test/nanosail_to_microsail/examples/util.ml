let dummy_sail_def =
  let open Libsail.Ast in
  let def = DEF_pragma ("", "", Unknown)
  and annotation = { doc_comment=None; attrs=[]; loc=Unknown }
  in
  DEF_aux (def, annotation)
