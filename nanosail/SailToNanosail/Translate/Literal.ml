module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC        = TranslationContext
module Bindings  = Libsail.Ast_util.Bindings
module StringMap = Map.String

open! ExtBase
open Monads.Notations.Star(TC)


let value_of_literal (literal : S.lit) : Ast.Value.t TC.t =
  let label =
    Logging.Message.horizontal [
      Logging.Message.string "Translating literal ";
      Logging.Message.from_multiline_string @@ StringOf.Sail.lit literal
    ]
  in
  TC.translation_block [%here] label begin
    let S.L_aux (unwrapped_literal, literal_location) = literal
    in
    match unwrapped_literal with
    | L_true     -> TC.return @@ Ast.Value.Bool true
    | L_false    -> TC.return @@ Ast.Value.Bool false
    | L_num n    -> TC.return @@ Ast.Value.Int n
    | L_unit     -> TC.return @@ Ast.Value.Unit
    | L_string s -> TC.return @@ Ast.Value.String s
    | L_zero     -> TC.return @@ Ast.Value.Bit false
    | L_one      -> TC.return @@ Ast.Value.Bit true
    | L_hex s    -> TC.not_yet_implemented ~message:s [%here] literal_location
    | L_bin s    -> TC.not_yet_implemented ~message:s [%here] literal_location
    | L_undef    -> TC.not_yet_implemented [%here] literal_location
    | L_real _   -> TC.not_yet_implemented [%here] literal_location
  end
