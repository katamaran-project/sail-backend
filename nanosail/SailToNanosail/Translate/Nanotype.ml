open Base

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Numeric


let rec nanotype_of_sail_type (S.Typ_aux (typ, location)) : N.nanotype TC.t =
  (*
    Types are representing as strings in Sail.
  *)
  let rec type_of_identifier identifier : N.nanotype TC.t =
    let* identifier' = translate_identifier [%here] identifier
    in
    match Id.string_of identifier' with
    | "bool"      -> TC.return @@ N.Ty_bool
    | "nat"       -> TC.return @@ N.Ty_nat
    | "int"       -> TC.return @@ N.Ty_int
    | "unit"      -> TC.return @@ N.Ty_unit
    | "string"    -> TC.return @@ N.Ty_string
    | "atom"      -> TC.return @@ N.Ty_atom
    | _           -> TC.return @@ N.Ty_custom identifier'

  (*
     Sail represents types with parameters with Typ_app (id, type_args).
     This function translates these to their corresponding nanotype.
  *)
  and translate_type_constructor
      (identifier     : S.id          )
      (type_arguments : S.typ_arg list) =
    let* type_arguments' = TC.map ~f:translate_type_argument type_arguments
    and* identifier'     = translate_identifier [%here] identifier
    in
    match (Id.string_of identifier'), type_arguments' with
    | "list" , [ N.TA_type t ]  -> TC.return @@ N.Ty_list t
    | id     , _                -> TC.return @@ N.Ty_app (Id.mk id, type_arguments')

  and translate_type_argument (S.A_aux (type_argument, _location)) : N.type_argument TC.t =
    match type_argument with
    | A_nexp e -> begin
        let* e' = translate_numeric_expression e
        in
        TC.return @@ N.TA_numexp e'
      end
    | A_typ t  -> begin
        let* t' = nanotype_of_sail_type t
        in
        TC.return @@ N.TA_type t'
      end
    | A_bool b -> begin
        let* b' = translate_numeric_constraint b
        in
        TC.return @@ N.TA_bool b'
      end
  in

  match typ with
  | Typ_tuple items -> begin
      let* items' = TC.map ~f:nanotype_of_sail_type items
      in
      TC.return @@ N.Ty_tuple items'
    end
  | Typ_id id                       -> type_of_identifier id
  | Typ_app (identifier, type_args) -> translate_type_constructor identifier type_args
  | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
  | Typ_var _                       -> TC.not_yet_implemented [%here] location
  | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
  | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location
  | Typ_exist (ids, nc, typ)        -> begin
      let ids' = String.concat ~sep:", " @@ List.map ~f:Libsail.Ast_util.string_of_kinded_id ids
      and numeric_constraint' = Libsail.Ast_util.string_of_n_constraint nc
      and typ' = Libsail.Ast_util.string_of_typ typ
      in
      Stdio.printf "Encountered Typ_exist\nKinded ids: %s\nNumeric constraint: %s\nType: %s\n\n" ids' numeric_constraint' typ';
      nanotype_of_sail_type typ
    end
