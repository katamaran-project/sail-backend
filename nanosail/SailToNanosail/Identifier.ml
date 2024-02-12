open Base

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)


let translate_identifier ocaml_location (S.Id_aux (aux, sail_location)) : string TC.t =
  match aux with
  | Id id       -> TC.return id
  | Operator op -> TC.not_yet_implemented ~message:(Printf.sprintf "Operator %s" op) ocaml_location sail_location
