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


let unwrap_identifier
      (ocaml_location : Lexing.position)
      (id             : Libsail.Ast.id ) : string TC.t
  =
  let S.Id_aux (unwrapped_id, id_location) = id
  in
  match unwrapped_id with
  | S.Id string -> TC.return string
  | S.Operator _ -> TC.not_yet_implemented ocaml_location id_location


let translate_identifier
      (ocaml_location : Lexing.position)
      (identifier     : S.id           ) : N.identifier TC.t
  =
  let S.Id_aux (aux, sail_location) = identifier
  in
  match aux with
  | Id id       -> TC.return @@ Id.mk id
  | Operator op -> TC.not_yet_implemented ~message:(Printf.sprintf "Operator %s" op) ocaml_location sail_location
