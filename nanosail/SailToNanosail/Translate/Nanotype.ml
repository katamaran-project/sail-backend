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
  let rec nanotype_of_identifier (identifier : S.id) : N.nanotype TC.t =
    let* identifier' = translate_identifier [%here] identifier
    in
    match Id.string_of identifier' with
    | "bool"      -> TC.return @@ N.Ty_bool
    | "nat"       -> TC.return @@ N.Ty_nat
    | "int"       -> TC.return @@ N.Ty_int
    | "unit"      -> TC.return @@ N.Ty_unit
    | "string"    -> TC.return @@ N.Ty_string
    | "atom"      -> TC.fail [%here] "Atoms should be intercepted higher up"
    | "atom_bool" -> TC.fail [%here] "Atoms should be intercepted higher up"
    | _           -> TC.return @@ N.Ty_custom identifier'

  (*
     Sail represents types with parameters with Typ_app (id, type_args).
     This function translates these to their corresponding nanotype.
  *)
  and nanotype_of_type_constructor
      (identifier     : S.id          )
      (type_arguments : S.typ_arg list)
    =
    let* type_arguments' = TC.map ~f:nanotype_of_type_argument type_arguments
    and* identifier'     = translate_identifier [%here] identifier
    in
    match (Id.string_of identifier'), type_arguments' with
    | "list" , [ N.TA_type t ]  -> TC.return @@ N.Ty_list t
    | "atom", [ _ ]             -> TC.return N.Ty_int
    | "atom_bool", [ _ ]        -> TC.return N.Ty_bool
    | _, _                      -> begin
        let* constructor = nanotype_of_identifier identifier
        in
        TC.return @@ N.Ty_app (constructor, type_arguments')
      end

  and nanotype_of_type_argument (type_argument : Libsail.Ast.typ_arg) : N.type_argument TC.t =
    let S.A_aux (unwrapped_type_argument, _location) = type_argument
    in
    match unwrapped_type_argument with
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

  and nanotype_of_existential
      (ids         : Libsail.Ast.kinded_id list)
      (constraints : Libsail.Ast.n_constraint  )
      (typ         : Libsail.Ast.typ           )
    =
    let ids'                = String.concat ~sep:", " @@ List.map ~f:Libsail.Ast_util.string_of_kinded_id ids
    and numeric_constraint' = StringOf.Sail.n_constraint constraints
    and typ'                = Libsail.Ast_util.string_of_typ typ
    in
    begin
      if
        Configuration.(get print_warnings)
      then
        Stdio.printf "Encountered Typ_exist\nKinded ids: %s\nNumeric constraint: %s\nType: %s\n\n" ids' numeric_constraint' typ'
    end;
    nanotype_of_sail_type typ

  and nanotype_of_tuple (items : Libsail.Ast.typ list) =
    let* items' = TC.map ~f:nanotype_of_sail_type items
    in
    TC.return @@ N.Ty_tuple items'

  in
  match typ with
  | Typ_tuple items                 -> nanotype_of_tuple items
  | Typ_id id                       -> nanotype_of_identifier id
  | Typ_app (identifier, type_args) -> nanotype_of_type_constructor identifier type_args
  | Typ_exist (ids, nc, typ)        -> nanotype_of_existential ids nc typ
  | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
  | Typ_var _                       -> TC.not_yet_implemented [%here] location
  | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
  | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location
