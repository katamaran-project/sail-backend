open! ExtBase

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

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
      (identifier     : S.id           ) : Ast.Identifier.t TC.t
  =
  
  TC.translation_block [%here] (PP.string @@ "Translating identifier " ^ StringOf.Sail.id identifier) begin
    let S.Id_aux (unwrapped_identifier, sail_location) = identifier
    in
    match unwrapped_identifier with
    | Id id       -> TC.return @@ Ast.Identifier.mk id
    | Operator op -> TC.not_yet_implemented ~message:(Printf.sprintf "Operator %s" op) ocaml_location sail_location
  end
