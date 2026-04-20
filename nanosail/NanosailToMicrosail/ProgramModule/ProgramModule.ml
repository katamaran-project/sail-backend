open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let genblock loc label doc =
  GC.generation_block loc label doc


let pp_require_imports () : PP.t =
  let coq_imports =
    List.build_list (fun { add; _ } ->
        if Configuration.(get pretty_print_lists) then add "Lists.List";
        add "Classes.EquivDec";
        add "Strings.String";
        add "ZArith.BinInt"
      )
  in
  PP.(paragraphs [
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Coq"      ) ~mode:(Some Coq.Import) coq_imports;
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Katamaran") ~mode:(Some Coq.Import) [ "Semantics.Registers"; "Bitvector"; "Program" ];
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "stdpp"    ) ~mode:(Some Coq.Import) [ "finite" ];
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Equations") ~mode:(Some Coq.Import) [ "Equations" ];
    ])


let pp_imports () : PP.t =
  let imports =
    List.build_list (fun { add; _ } ->
        add "ctx.notations";
        add "ctx.resolution";
        add "env.notations";
        add "bv.notations";
        if Configuration.(get pretty_print_lists) then add "ListNotations";
      )
  in
  PP.annotate [%here] @@ Coq.pp_imports imports


let pp_open_scopes () : PP.t =
  let scopes =
    List.build_list (fun { add; _ } ->
        add "string_scope";
        add "list_scope";
      )
  in
  PP.annotate [%here] @@ Coq.pp_open_scopes scopes


let pp_import_base () : PP.t =
  PP.annotate [%here] @@ Coq.pp_imports [ Configuration.(get base_name) ]


let generate_program_prelude () : PP.t GC.t =
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
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list) : PP.t GC.t
  =
  genblock [%here] "Program Module" begin
    let mode            = Some Coq.Import
    and identifier      = Configuration.(get program_name)
    and base_identifier = Configuration.(get base_name)
    in
    let module_types    = [ PP.separate_horizontally ~separator:PP.space [ PP.string "Program"; PP.string base_identifier ] ]
    in
    (* bind `contents` monadically to a PP.t value in the GC.t monad *)
    let* contents =
      (* bind all these kits monadically to PP.t values in the GC.t monad *)
      let* function_declaration_kit =
        GC.pp_annotate [%here] begin
            FunDeclKit.generate @@ Ast.Definition.Select.drop_sail_definitions function_definitions;
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
      (*
        combine the PP.t terms that are the different kits with some
        text into a big PP.t. wrap this in GC and monadically bind it
        to `contents`.
      *)
      GC.return @@ PP.paragraphs [
        PP.annotate [%here] @@ function_declaration_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include FunDeclMixin " ^ base_identifier;
        PP.annotate [%here] @@ function_definition_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@"Include DefaultRegStoreKit " ^ base_identifier;
        PP.annotate [%here] @@ foreign_kit;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include ProgramMixin " ^ base_identifier;
        PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Import callgraph.

  Lemma fundef_bindfree (Δ : PCtx) (τ : Ty) (f : Fun Δ τ) :
    stm_bindfree (FunDef f).
  Proof. destruct f; now vm_compute. Qed.

  Definition 𝑭_call_graph := generic_call_graph.
  Lemma 𝑭_call_graph_wellformed : CallGraphWellFormed 𝑭_call_graph.
  Proof. apply generic_call_graph_wellformed, fundef_bindfree. Qed.

  Definition 𝑭_accessible {Δ τ} (f : 𝑭 Δ τ) : option (Accessible 𝑭_call_graph f) :=
    None";
      ]
    in
    (* 
      the function as a whole must return a (PP.t GC.t).
      have the `contents` pretty-printed by `Coq.pp_module` and wrap
      that in a GC.
     *)
    GC.return @@ PP.annotate [%here] @@ Coq.pp_module
      ~mode
      ~module_types
      identifier
      contents
  end
