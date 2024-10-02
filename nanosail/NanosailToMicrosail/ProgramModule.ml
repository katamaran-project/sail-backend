open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let genblock loc label doc =
  let* doc = doc
  in
  GC.generation_block loc (PP.string label) doc


let pp_require_imports () : PP.document =
  let coq_imports =
    Auxlib.build_list (fun { add; _ } ->
        if Configuration.(get use_list_notations) then add "Lists.List";
        add "Classes.EquivDec";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(separate (twice hardline) [
      Coq.pp_require ~from:(Some "Coq"      ) ~import:true coq_imports;
      Coq.pp_require ~from:(Some "Katamaran") ~import:true [ "Semantics.Registers"; "Bitvector"; "Program" ];
      Coq.pp_require ~from:(Some "stdpp"    ) ~import:true [ "finite" ];
      Coq.pp_require ~from:(Some "Equations") ~import:true [ "Equations" ];
    ])


let pp_imports () : PP.document =
  let imports =
    Auxlib.build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        add "env.notations";
        if Configuration.(get use_list_notations) then add "ListNotations";
      )
  in
  Coq.pp_imports imports


let pp_open_scopes () : PP.document =
  let scopes =
    Auxlib.build_list (fun { add; _ } ->
        add "string_scope";
        add "list_scope";
      )
  in
  Coq.pp_open_scopes scopes


let pp_import_base () : PP.document =
  Coq.pp_imports [ Configuration.(get base_name) ]


let generate_program_prelude () : PP.document GC.t =
  GC.return @@ PP.(separate (twice hardline) [
      pp_require_imports ();
      pp_imports ();
      pp_open_scopes ();
      pp_import_base ();
    ])


let generate_base_prelude () : PP.document GC.t =
  GC.return @@ PP.separate PP.hardline [
    Coq.pp_require ~from:(Some "Coq"      ) ~import:true  [ "Classes.EquivDec"; "Strings.String" ];
    Coq.pp_require ~from:(Some "stdpp"    ) ~import:false [ "finite" ];
    Coq.pp_require ~from:(Some "Equations") ~import:true  [ "Equations" ];
    Coq.pp_require                          ~import:true  [ "Katamaran.Base" ];
  ]


let pp_program_module
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list                          )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document GC.t
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
  end
