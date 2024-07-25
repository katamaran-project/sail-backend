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


let translate_enum
      (_definition_annotation : S.def_annot)
      (_type_annotation       : 'a S.annot )
      (identifier             : S.id       )
      (cases                  : S.id list  ) : Ast.Definition.Type.t TC.t
  =
  let* identifier' = Identifier.translate_identifier [%here] identifier
  and* cases'      = TC.map ~f:(Identifier.translate_identifier [%here]) cases
  in
  TC.return @@ Ast.Definition.Type.Enum {
      identifier = identifier';
      cases      = cases'     ;
    }
