open Base

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Numeric


let rec nanotype_of_sail_type (S.Typ_aux (typ, location)) : Ast.Type.t TC.t =
  match typ with
  | Typ_tuple items                 -> nanotype_of_tuple items
  | Typ_id id                       -> nanotype_of_identifier id
  | Typ_app (identifier, type_args) -> nanotype_of_type_constructor identifier type_args
  | Typ_exist (ids, nc, typ)        -> nanotype_of_existential ids nc typ
  | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
  | Typ_var _                       -> TC.not_yet_implemented [%here] location
  | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
  | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location

(*
  Types are representing as strings in Sail.
*)
and nanotype_of_identifier (identifier : S.id) : Ast.Type.t TC.t =
  let Id_aux (_, location) = identifier
  in
  let* identifier' = translate_identifier [%here] identifier
  in
  let id_as_string = Ast.Identifier.string_of identifier'
  in
  match id_as_string with
  | "bool"      -> TC.return @@ Ast.Type.Bool
  | "int"       -> TC.return @@ Ast.Type.Int
  | "unit"      -> TC.return @@ Ast.Type.Unit
  | "string"    -> TC.return @@ Ast.Type.String
  | "atom"      -> TC.fail [%here] "Atoms should be intercepted higher up"
  | "atom_bool" -> TC.fail [%here] "Atoms should be intercepted higher up"
  | _           -> begin
      let* type_definition : Ast.Definition.Type.t option = TC.lookup_type_definition identifier'
      in
      match type_definition with
      | Some (Abbreviation { identifier; abbreviation }) -> begin
          let _ = identifier (* todo remove this *)
          in
          match abbreviation with
          | Ast.Definition.Type.Abbreviation.NumericExpression (_, _)     -> TC.not_yet_implemented [%here] location
          | Ast.Definition.Type.Abbreviation.NumericConstraint (_, _)     -> TC.not_yet_implemented [%here] location
          | Ast.Definition.Type.Abbreviation.Alias (type_quantifier, typ) -> begin
              match type_quantifier with
              | []   -> TC.return @@ Ast.Type.Alias (identifier', typ)
              | _::_ -> TC.not_yet_implemented [%here] location
            end
        end
      | Some (Variant _) -> TC.return @@ Ast.Type.Variant identifier'
      | Some (Record _)  -> TC.return @@ Ast.Type.Record identifier'
      | Some (Enum _)    -> TC.return @@ Ast.Type.Enum identifier'
      | None             -> TC.fail [%here] @@ Printf.sprintf "Unknown type %s" id_as_string
    end

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
  match (Ast.Identifier.string_of identifier'), type_arguments' with
  | "list" , [ Type t ]    -> TC.return @@ Ast.Type.List t
  | "atom", [ _ ]             -> TC.return Ast.Type.Int
  | "atom_bool", [ _ ]        -> TC.return Ast.Type.Bool
  | _, _                      -> begin
      let* constructor = nanotype_of_identifier identifier
      in
      TC.return @@ Ast.Type.Application (constructor, type_arguments')
    end

and nanotype_of_type_argument (type_argument : Libsail.Ast.typ_arg) : Ast.TypeArgument.t TC.t =
  let S.A_aux (unwrapped_type_argument, _location) = type_argument
  in
  match unwrapped_type_argument with
  | A_nexp e -> begin
      let* e' = translate_numeric_expression e
      in
      TC.return @@ Ast.TypeArgument.NumericExpression e'
    end
  | A_typ t  -> begin
      let* t' = nanotype_of_sail_type t
      in
      TC.return @@ Ast.TypeArgument.Type t'
    end
  | A_bool b -> begin
      let* b' = translate_numeric_constraint b
      in
      TC.return @@ Ast.TypeArgument.Bool b'
    end

and nanotype_of_existential
    (ids         : Libsail.Ast.kinded_id list)
    (constraints : Libsail.Ast.n_constraint  )
    (typ         : Libsail.Ast.typ           ) : Ast.Type.t TC.t
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
  match items' with
  | [ t1; t2 ] -> TC.return @@ Ast.Type.Product (t1, t2) (* represent pairs using products *)
  | _          -> TC.return @@ Ast.Type.Tuple items'
