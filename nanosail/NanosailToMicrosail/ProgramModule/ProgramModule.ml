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
      PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Stdlib"   ) ~mode:(Some Coq.Import) coq_imports;
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


(*
   stm_bindfree of each body, by vm_compute. A body with branches that Sail's flow typing
   specialises (Ast.Statement.Specialize) matches on PeanoNat.Nat.eq_dec of a size variable,
   which vm_compute cannot reduce: those definitions are unfolded and the decisions case-split first.
*)
(* Whether the function's body decides a branch fact or equality in Rocq (see Statements.pp_specialize) *)
let decides_in_rocq (function_definition : Ast.Definition.Function.t) : bool =
  let parameters = List.map ~f:fst @@ FunDeclKit.type_level_parameters function_definition
  in
  let is_parameter (identifier : Ast.Identifier.t) = List.mem parameters identifier ~equal:Ast.Identifier.equal
  in
  List.exists (Ast.Statement.specialisations function_definition.function_body) ~f:(fun (equalities, facts) ->
      List.exists equalities ~f:(fun (identifier, _) -> is_parameter identifier)
      || List.exists facts ~f:(fun fact ->
          match TypeLevelParameters.constraint_variables fact with
          | Some (_ :: _ as variables) -> List.for_all variables ~f:is_parameter
          | Some [] | None             -> false
        )
    )


let fundef_bindfree
    ?(qualifier           : Ast.Identifier.t -> string option = fun _ -> None)
    (function_definitions : (Sail.sail_definition * Ast.Definition.Function.t) list) : string
  =
  let specialised =
    List.concat_map function_definitions ~f:(fun (_, (function_definition : Ast.Definition.Function.t)) ->
        if List.is_empty function_definition.monomorphs then [ function_definition ] else function_definition.monomorphs
      )
    |> List.filter ~f:(fun (function_definition : Ast.Definition.Function.t) -> decides_in_rocq function_definition)
    |> List.map ~f:(fun (function_definition : Ast.Definition.Function.t) ->
        let identifier = Ast.Identifier.to_string @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
        in
        match qualifier function_definition.function_name with
        | Some library -> library ^ "." ^ identifier
        | None         -> identifier
      )
  in
  let proof =
    match specialised with
    | [] -> "Proof. destruct f; now vm_compute. Qed."
    | _ :: _ -> begin
        String.concat ~sep:"\n" [
          "Proof.";
          "    destruct f; cbv [FunDef " ^ String.concat ~sep:" " specialised ^ "];";
          "      repeat match goal with";
          "             | |- context [PeanoNat.Nat.eq_dec ?a ?b] => destruct (PeanoNat.Nat.eq_dec a b)";
          "             | |- context [sail_decide ?b] => destruct (sail_decide b)";
          "             end;";
          "      now vm_compute.";
          "  Qed.";
        ]
      end
  in
  "  Lemma fundef_bindfree (Δ : PCtx) (τ : Ty) (f : Fun Δ τ) :\n    stm_bindfree (FunDef f).\n  " ^ proof


(* ProgramMixin and the call graph, which close the program module *)
let pp_program_mixin_and_call_graph
    ?(qualifier           : Ast.Identifier.t -> string option = fun _ -> None)
    (function_definitions : (Sail.sail_definition * Ast.Definition.Function.t) list) : PP.t
  =
  let base_identifier = Configuration.(get base_name)
  in
  PP.paragraphs [
    PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include ProgramMixin " ^ base_identifier;
    PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Import callgraph.

"
      ^ fundef_bindfree ~qualifier function_definitions ^ "

  Definition 𝑭_call_graph := generic_call_graph.
  Lemma 𝑭_call_graph_wellformed : CallGraphWellFormed 𝑭_call_graph.
  Proof. apply generic_call_graph_wellformed, fundef_bindfree. Qed.

  Definition 𝑭_accessible {Δ τ} (f : 𝑭 Δ τ) : option (Accessible 𝑭_call_graph f) :=
    None";
  ]


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
      (*
         The function definitions come first: their calls to externs determine
         which foreign functions FunDeclKit (FunX) and ForeignKit contain.
      *)
      let* function_definition_kit =
        GC.pp_annotate [%here] begin
            FunDefKit.pp_function_definition_kit function_definitions top_level_type_constraint_definitions
          end
      in
      let* function_declaration_kit =
        GC.pp_annotate [%here] begin
            FunDeclKit.generate @@ Ast.Definition.Select.drop_sail_definitions function_definitions;
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
        PP.annotate [%here] @@ pp_program_mixin_and_call_graph function_definitions;
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


(*
   The program, when the configuration asks to preserve the file structure of the Sail model
   (preserve-file-structure; see FileStructure and docs/usage.md#preserve-file-structure):

   - declarations: the module with the declarations of the functions (Fun, FunX, and Stm through
     FunDeclMixin), which the bodies need;
   - sail_files: for each Sail file with functions, the bodies of its functions (Definition fun_f);
   - assembly: the bodies of the other functions (Sail's library, and functions without a source
     location), then the program module, which includes the declarations and whose FunDef refers
     to the bodies in the files of the Sail files.

   The declarations and the files of the Sail files do not start with the Require of the base
   library: its name is only known once all templates are processed (see Templates.AdditionalFiles).
*)
type files =
  {
    declarations         : PP.t;
    sail_files_directory : string;
    sail_files           : (FileStructure.ModulePath.t * PP.t) list;
    assembly             : PP.t;
  }


(* The name of the module with the declarations of the functions *)
let declarations_module_name () : string =
  Configuration.(get program_name) ^ "Decl"


let generate_files
    ~(declarations_library                 : string                                                                 )
    ~(library_directories                  : string list                                                            )
    ~(current_directory                    : string                                                                 )
    ~(sail_files_directory                 : string                                                                 )
    ~(root                                 : string option                                                          )
    ~(reserved_names                       : string list                                                            )
    (type_definitions                      : (Sail.sail_definition * Ast.Definition.Type.t) list                    )
    (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list              )
    (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list) : files GC.t
  =
  let grouping : (Sail.sail_definition * Ast.Definition.Function.t) FileStructure.grouping =
    (* the functions that Sail generates for a bitfield have no location: they go to the file of the bitfield *)
    let type_names =
      List.map type_definitions ~f:(fun (_, type_definition) -> Ast.Identifier.to_string @@ Ast.Definition.Type.identifier type_definition)
    in
    let file_of_generated_function (function_definition : Ast.Definition.Function.t) : string option =
      FileStructure.type_of_generated_function ~type_names (Ast.Identifier.to_string function_definition.function_name)
      |> Option.bind ~f:(fun type_name ->
          List.find_map type_definitions ~f:(fun (sail_definition, type_definition) ->
              if String.equal type_name (Ast.Identifier.to_string @@ Ast.Definition.Type.identifier type_definition)
              then FileStructure.file_of_sail_definition sail_definition
              else None
            )
        )
    in
    let file_of ((sail_definition, function_definition) : Sail.sail_definition * Ast.Definition.Function.t) : string option =
      match FileStructure.file_of_sail_definition sail_definition with
      | Some file -> Some file
      | None      -> file_of_generated_function function_definition
    in
    (* the directories of the Sail files are relative to the root: by default the longest common directory of the model's files *)
    let root : string list =
      let model_files =
        List.filter_map function_definitions ~f:file_of
        |> List.filter ~f:(fun file -> not @@ FileStructure.is_library_file ~library_directories file)
      in
      FileStructure.root_directory ~current_directory ~root model_files
    in
    let destination_of (definition : Sail.sail_definition * Ast.Definition.Function.t) : FileStructure.Destination.t =
      FileStructure.destination_of_file ~library_directories ~current_directory ~root (file_of definition)
    in
    FileStructure.group ~directory:sail_files_directory ~destination_of ~reserved_names function_definitions
  in
  let* () =
    let message = lazy begin
      let describe (file : string) (definitions : (Sail.sail_definition * Ast.Definition.Function.t) list) : PP.t =
        PP.vertical [
          PP.string file;
          PP.indent @@ PP.vertical @@ List.map definitions ~f:(fun (_, (definition : Ast.Definition.Function.t)) -> Identifier.pp definition.function_name)
        ]
      in
      PP.vertical begin
        List.build_list (fun { add; addall; _ } ->
            add @@ PP.string "Files of the functions (preserve-file-structure)";
            addall @@ List.map grouping.sail_files ~f:(fun (sail_file : (Sail.sail_definition * Ast.Definition.Function.t) FileStructure.sail_file) -> describe (FileStructure.ModulePath.to_file ~directory:sail_files_directory sail_file.module_path) sail_file.items);
            add @@ describe "<program>" grouping.assembly
          )
      end
    end
    in
    GC.log [%here] Logging.info message
  in
  (* the bodies of a function and its monomorphs are in the file of the function *)
  let qualifier : Ast.Identifier.t -> string option =
    let libraries : string Ast.Identifier.Map.t =
      let entries =
        List.concat_map grouping.sail_files ~f:(fun ({ module_path; items; path = _; relative_path = _ } : (Sail.sail_definition * Ast.Definition.Function.t) FileStructure.sail_file) ->
            List.concat_map items ~f:(fun (_, (definition : Ast.Definition.Function.t)) ->
                List.map
                  (definition :: definition.monomorphs)
                  ~f:(fun (function_definition : Ast.Definition.Function.t) -> (function_definition.function_name, FileStructure.ModulePath.to_library ~directory:sail_files_directory module_path))
              )
          )
      in
      List.fold entries ~init:Ast.Identifier.Map.empty ~f:(fun map (key, data) -> Ast.Identifier.Map.overwrite map ~key ~data)
    in
    Ast.Identifier.Map.find libraries
  in
  let base_identifier         = Configuration.(get base_name)
  and declarations_identifier = declarations_module_name ()
  in
  let pp_import_declarations : PP.t =
    PP.annotate [%here] @@ Coq.pp_imports [ declarations_identifier ]
  in
  (*
     The bodies come first: their calls to externs determine
     which foreign functions FunDeclKit (FunX) and ForeignKit contain.
  *)
  let* sail_file_bodies : (FileStructure.ModulePath.t * string * PP.t list) list =
    GC.map grouping.sail_files ~f:(fun ({ module_path; relative_path; items; path = _ } : (Sail.sail_definition * Ast.Definition.Function.t) FileStructure.sail_file) ->
        let* bodies = FunDefKit.pp_function_definitions items top_level_type_constraint_definitions
        in
        GC.return (module_path, relative_path, bodies)
      )
  in
  let* assembly_bodies =
    FunDefKit.pp_function_definitions grouping.assembly top_level_type_constraint_definitions
  in
  let* declarations =
    let* program_prelude = generate_program_prelude ()
    and* function_declaration_kit =
      genblock [%here] "Function Declarations" begin
        FunDeclKit.generate @@ Ast.Definition.Select.drop_sail_definitions function_definitions
      end
    in
    let declarations_module =
      Coq.pp_module
        ~mode:(Some Coq.Export)
        ~module_types:[ PP.string @@ "FunDecl " ^ base_identifier ]
        declarations_identifier
        begin
          PP.paragraphs [
            PP.annotate [%here] @@ function_declaration_kit;
            PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include FunDeclMixin " ^ base_identifier;
          ]
        end
    in
    GC.return @@ PP.paragraphs [ program_prelude; declarations_module ]
  in
  let* sail_files =
    GC.map sail_file_bodies ~f:(fun (module_path, relative_path, bodies) ->
        let* program_prelude = generate_program_prelude ()
        in
        let header =
          PP.annotate [%here] @@ Coq.pp_comment @@ PP.string @@ Printf.sprintf "The functions of %s" relative_path
        in
        GC.return (module_path, PP.paragraphs (header :: program_prelude :: pp_import_declarations :: bodies))
      )
  in
  let* assembly =
    let* program_prelude = generate_program_prelude ()
    and* fundef =
      FunDefKit.pp_fundef_inductive_type ~qualifier @@ Ast.Definition.Select.drop_sail_definitions function_definitions
    and* foreign_kit =
      GC.pp_annotate [%here] @@ ForeignKit.pp_foreign_kit ()
    in
    let requires =
      PP.vertical begin
        List.build_list (fun { add; _ } ->
            add @@ Coq.pp_require ~mode:(Some Coq.Export) [ declarations_library ];
            if not @@ List.is_empty grouping.sail_files
            then add @@ Coq.pp_require (List.map grouping.sail_files ~f:(fun ({ module_path; path = _; relative_path = _; items = _ } : (Sail.sail_definition * Ast.Definition.Function.t) FileStructure.sail_file) -> FileStructure.ModulePath.to_library ~directory:sail_files_directory module_path))
          )
      end
    and program_module =
      Coq.pp_module
        ~mode:(Some Coq.Import)
        ~module_types:[ PP.string @@ "Program " ^ base_identifier ]
        Configuration.(get program_name)
        begin
          PP.paragraphs [
            PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include " ^ declarations_identifier;
            PP.annotate [%here] @@ fundef;
            PP.annotate [%here] @@ Coq.pp_sentence @@ PP.string @@ "Include DefaultRegStoreKit " ^ base_identifier;
            PP.annotate [%here] @@ foreign_kit;
            PP.annotate [%here] @@ pp_program_mixin_and_call_graph ~qualifier function_definitions;
          ]
        end
    in
    GC.return @@ PP.paragraphs (List.concat [ [ requires; program_prelude; pp_import_declarations ]; assembly_bodies; [ program_module ] ])
  in
  GC.return { declarations; sail_files_directory; sail_files; assembly }
