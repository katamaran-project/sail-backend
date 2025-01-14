open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let genblock loc label doc =
  GC.generation_block loc label doc


let pp_require_imports () : PP.document =
  let coq_imports =
    Auxlib.build_list (fun { add; _ } ->
        if Configuration.(get use_list_notations) then add "Lists.List";
        add "Classes.EquivDec";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(paragraphs [
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Coq"      ) ~import:true coq_imports;
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Katamaran") ~import:true [ "Semantics.Registers"; "Bitvector"; "Program" ];
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "stdpp"    ) ~import:true [ "finite" ];
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Equations") ~import:true [ "Equations" ];
    ])


let pp_imports () : PP.document =
  let imports =
    Auxlib.build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        add "env.notations";
        add "bv.notations";
        if Configuration.(get use_list_notations) then add "ListNotations";
      )
  in
  PP.annotate [%here] @@ Coq.pp_imports imports


let pp_open_scopes () : PP.document =
  let scopes =
    Auxlib.build_list (fun { add; _ } ->
        add "string_scope";
        add "list_scope";
      )
  in
  PP.annotate [%here] @@ Coq.pp_open_scopes scopes


let pp_import_base () : PP.document =
  PP.annotate [%here] @@ Coq.pp_imports [ Configuration.(get base_name) ]


let generate_program_prelude () : PP.document GC.t =
  GC.return begin
      PP.annotate [%here] begin
          PP.paragraphs [
              pp_require_imports ();
              pp_imports ();
              pp_open_scopes ();
              pp_import_base ();
            ]
        end
    end


let pp_program_module
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list              )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list) : PP.document GC.t
  =
  genblock [%here] "Program Module" begin
    let flag            = Coq.Import
    and identifier      = Configuration.(get program_name)
    and base_identifier = Configuration.(get base_name)
    in
    let includes        = [ "Program"; base_identifier ]
    in
    let* contents =
      let* function_declaration_kit =
        GC.pp_annotate [%here] begin
            FunDeclKit.generate @@ List.map ~f:snd function_definitions;
          end

      and* function_definition_kit =
        GC.pp_annotate [%here] begin
            FunDefKit.pp_function_definition_kit function_definitions top_level_type_constraint_definitions
          end

      and* foreign_kit =
        GC.pp_annotate [%here] begin
            ForeignKit.pp_foreign_kit ()
          end

      in
      GC.return @@ PP.paragraphs [
        PP.annotate [%here] @@ function_declaration_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include FunDeclMixin " ^ base_identifier;
        PP.annotate [%here] @@ function_definition_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@"Include DefaultRegStoreKit " ^ base_identifier;
        PP.annotate [%here] @@ foreign_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include ProgramMixin " ^ base_identifier;
      ]
    in
    GC.return @@ PP.annotate [%here] @@  Coq.pp_module
      ~flag:flag
      ~includes:includes
      identifier
      contents
  end
