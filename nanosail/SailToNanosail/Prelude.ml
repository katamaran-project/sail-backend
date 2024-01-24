open Ast


let prelude = [
    TypeDefinition (TD_enum { identifier = "unit"; cases = [ "()" ] });
  ]
