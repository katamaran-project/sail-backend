module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC        = TranslationContext
module Bindings  = Libsail.Ast_compare.Bindings

open Monads.Notations.Star(TC)


(*
   Converts a binary or hexadecimal literal into its list of bits, most significant bit first.
   Returns None for other literals.
*)
let bits_of_literal (literal : S.lit) : bool list option =
  let S.L_aux (unwrapped_literal, _) = literal
  in
  let bool_of_bit (bit : Libsail.Ast.Bit.bit) : bool =
    match bit with
    | B0 -> false
    | B1 -> true
  in
  match unwrapped_literal with
  | L_bin digits -> Some (List.map bool_of_bit @@ Libsail.Ast.BitList.of_bin_lit digits)
  | L_hex digits -> Some (List.map bool_of_bit @@ Libsail.Ast.BitList.of_hex_lit digits)
  | _            -> None


(*
   Since Sail 0.20, bit literals (bitzero, bitone) are represented as single digit binary literals.
   We rely on the type to distinguish them from bitvectors of length 1.

   Sail 0.20 also dropped the vector_string_pats_to_bit_list rewrite, which used to
   turn bitvector literals into vectors of bits. We now deal with them here.
*)
let value_of_literal
    (literal : S.lit)
    (typ     : S.typ) : Ast.Value.t TC.t
  =
  let label =
    PP.horizontal [
      PP.string "Translating literal ";
      PP.string @@ StringOf.Sail.lit literal
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
    | L_bin [Non_empty (Bin_0, [])] when S.is_bit_typ typ -> TC.return @@ Ast.Value.Bit false
    | L_bin [Non_empty (Bin_1, [])] when S.is_bit_typ typ -> TC.return @@ Ast.Value.Bit true
    | L_hex _
    | L_bin _    -> begin
        match bits_of_literal literal with
        | Some bits -> TC.return @@ Ast.Value.Bitvector bits
        | None      -> TC.fail [%here] "bits_of_literal should succeed on binary/hexadecimal literals"
      end
    | L_real _   -> TC.not_yet_implemented [%here] literal_location
  end
