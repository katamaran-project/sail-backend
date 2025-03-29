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
      (_definition_annotation : Sail.definition_annotation)
      (_annotation            : 'a S.annot                )
      (identifier             : S.id                      )
      (cases                  : S.id list                 ) : Ast.Definition.Type.t TC.t
  =
  TC.translation_block [%here] (PP.string @@ "translating enum " ^ StringOf.Sail.id identifier) begin
    let* identifier' = Identifier.translate_identifier [%here] identifier
    and* cases'      = TC.map ~f:(Identifier.translate_identifier [%here]) cases
    in
    let* () = TC.log [%here] Logging.info @@ lazy (PP.format "Translated enum %s" (Ast.Identifier.to_string identifier'))
    in
    TC.return @@ Ast.Definition.Type.Enum {
      identifier = identifier';
      cases      = cases'     ;
    }
  end
