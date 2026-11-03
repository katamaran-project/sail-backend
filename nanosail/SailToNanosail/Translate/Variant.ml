open ExtBase

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


(*
   Translates a union that the configuration asks to be translated to muSail's ty.sum.
   Unlike regular variants, constructor fields are not flattened:
   each side of the sum holds exactly one value (unit for a nullary constructor, a tuple for an n-ary one).
*)
let translate_sum
      (identifier      : Ast.Identifier.t        )
      (type_quantifier : Ast.TypeQuantifier.t    )
      (sum_union       : Configuration.sum_union )
      (constructors    : S.type_union list       ) : Ast.Definition.Type.t TC.t
  =
  let* constructors' : Ast.Definition.Type.Sum.constructor list =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, constructor_identifier), _annotation)) =
      let* constructor_identifier = Identifier.translate_identifier [%here] constructor_identifier
      and* field_type             = Type.translate_type typ
      in
      TC.return ({ constructor_identifier; field_type } : Ast.Definition.Type.Sum.constructor)
    in
    TC.map ~f:translate_constructor constructors
  in
  let find_constructor (constructor_identifier : Ast.Identifier.t) : Ast.Definition.Type.Sum.constructor TC.t =
    let has_identifier (constructor : Ast.Definition.Type.Sum.constructor) : bool =
      Ast.Identifier.equal constructor.constructor_identifier constructor_identifier
    in
    match List.find constructors' ~f:has_identifier with
    | Some constructor -> TC.return constructor
    | None             -> begin
        let message =
          Printf.sprintf
            "translate-union-as-sum: union %s has no constructor %s"
            (Ast.Identifier.to_string identifier)
            (Ast.Identifier.to_string constructor_identifier)
        in
        TC.fail [%here] message
      end
  in
  let* () =
    if
      List.length constructors' = 2
    then
      TC.return ()
    else
      let message =
        Printf.sprintf
          "translate-union-as-sum: union %s should have exactly two constructors, but it has %d"
          (Ast.Identifier.to_string identifier)
          (List.length constructors')
      in
      TC.fail [%here] message
  in
  let* left  = find_constructor sum_union.left_constructor
  and* right = find_constructor sum_union.right_constructor
  in
  let* () =
    if
      Ast.Identifier.equal left.constructor_identifier right.constructor_identifier
    then
      let message =
        Printf.sprintf
          "translate-union-as-sum: union %s needs two different constructors"
          (Ast.Identifier.to_string identifier)
      in
      TC.fail [%here] message
    else
      TC.return ()
  in
  TC.return @@ Ast.Definition.Type.Sum {
      identifier;
      type_quantifier;
      left;
      right;
    }


let translate_regular_variant
      (identifier'      : Ast.Identifier.t    )
      (type_quantifier' : Ast.TypeQuantifier.t)
      (constructors     : S.type_union list   ) : Ast.Definition.Type.t TC.t
  =
  let* constructors'    =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      let* identifier' = Identifier.translate_identifier [%here] identifier
      and* typ' = Type.translate_type typ
      in
      let field_nanotypes =
        match typ' with
        | Tuple ts         -> ts
        | Unit             -> []
        | _                -> [ typ' ]
      in
      TC.return @@ (identifier', field_nanotypes)
    in
    TC.map ~f:translate_constructor constructors
  in
  TC.return @@ Ast.Definition.Type.Variant {
      identifier      = identifier'     ;
      type_quantifier = type_quantifier';
      constructors    = constructors'   ;
    }


let translate_variant
      (_definition_annotation : Sail.definition_annotation  )
      (_type_annotation       : Sail.type_annotation S.annot)
      (identifier             : S.id                        )
      (type_quantifier        : S.typquant                  )
      (constructors           : S.type_union list           )
      (_flag                  : bool                        ) : Ast.Definition.Type.t TC.t
  =
  let* identifier'      = Identifier.translate_identifier [%here] identifier
  and* type_quantifier' = TypeQuantifier.translate_type_quantifier type_quantifier
  in
  match Configuration.sum_union_for identifier' with
  | Some sum_union -> translate_sum identifier' type_quantifier' sum_union constructors
  | None           -> translate_regular_variant identifier' type_quantifier' constructors
