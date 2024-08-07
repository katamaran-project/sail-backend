open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext



let pp_program_module
      (program_name                          : string                                                                           )
      (base_name                             : string                                                                           )
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list                          )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document GC.t
  =
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  in
  let* contents =
    let* function_declaration_kit =
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
    and* function_definition_kit =
      FunDefKit.pp_function_definition_kit function_definitions top_level_type_constraint_definitions
    and* foreign_kit =
      ForeignKit.pp_foreign_kit ()
    in
    GC.return @@ PP.vertical ~separator:PP.(twice hardline) [
      function_declaration_kit;
      Coq.pp_sentence @@ PP.string @@ "Include FunDeclMixin " ^ base_identifier;
      function_definition_kit;
      Coq.pp_sentence @@ PP.string @@"Include DefaultRegStoreKit " ^ base_identifier;
      foreign_kit;
      Coq.pp_sentence @@ PP.string @@ "Include ProgramMixin " ^ base_identifier;
    ]
  in
  GC.return @@ Coq.pp_module
    ~flag:flag
    ~includes:includes
    identifier
    contents
