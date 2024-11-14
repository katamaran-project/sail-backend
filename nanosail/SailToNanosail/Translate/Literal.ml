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

open Base
open Monads.Notations.Star(TC)


let value_of_literal (literal : S.lit) : Ast.Value.t TC.t =
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
  | L_hex _    -> TC.not_yet_implemented [%here] literal_location
  | L_bin _    -> TC.not_yet_implemented [%here] literal_location
  | L_undef    -> TC.not_yet_implemented [%here] literal_location
  | L_real _   -> TC.not_yet_implemented [%here] literal_location
