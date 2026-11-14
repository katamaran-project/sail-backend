module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module Big_int = Nat_big_num
module TC = TranslationContext

open Monads.Notations.Star(TC)
open ExtBase


let should_ignore_definition (definition : Sail.sail_definition) : bool =
  let open Libsail.Ast
  in
  let Libsail.Ast.DEF_aux (definition, _annotation) = definition
  in
  let should_ignore_pragma (identifier : string) =
    Configuration.(get ignore_pragma_predicate) identifier

  and should_ignore_function_definition (function_definition : 'a fundef) =
    Configuration.should_ignore_function_definition (Sail.identifier_of_function_definition function_definition)

  and should_ignore_type_definition (type_definition : 'a type_def) : bool =
    Configuration.(get ignore_type_definition_predicate) (Sail.identifier_of_type_definition type_definition)

  and should_ignore_value_definition (pattern : Libsail.Type_check.tannot pat) : bool =
    Configuration.should_ignore_value_definition (Sail.identifier_of_value_definition pattern)

  and should_ignore_top_level_type_constraint (top_level_type_constraint : Sail.type_annotation val_spec) =
    Configuration.should_ignore_top_level_type_constraint (Sail.identifier_of_top_level_type_constraint top_level_type_constraint)

  and should_ignore_default_definition (_default_spec : default_spec) : bool =
    Configuration.(get ignore_default_order)

  and should_ignore_overload (_identifier : string) : bool =
    Configuration.(get ignore_overloads)

  in
  match definition with
  | DEF_type type_definition          -> should_ignore_type_definition type_definition
  | DEF_fundef function_definition    -> should_ignore_function_definition function_definition
  | DEF_mapdef _                      -> false
  | DEF_impl _                        -> false
  | DEF_let (pattern, _)              -> should_ignore_value_definition pattern
  | DEF_val top_level_type_constraint -> should_ignore_top_level_type_constraint top_level_type_constraint
  | DEF_outcome (_, _)                -> false
  | DEF_instantiation (_, _)          -> false
  | DEF_fixity (_, _, _)              -> false
  | DEF_overload (identifier, _)      -> should_ignore_overload (StringOf.Sail.id identifier)
  | DEF_default default_spec          -> should_ignore_default_definition default_spec
  | DEF_scattered _                   -> false
  | DEF_measure (_, _, _)             -> false
  | DEF_loop_measures (_, _)          -> false
  | DEF_register _                    -> false
  | DEF_internal_mutrec _             -> false
  | DEF_constraint _                  -> false
  | DEF_pragma (identifier, _)        -> should_ignore_pragma identifier


(*
   Runs the translation of a definition and stores its result.
   A NotYetImplemented or Failure error makes it an untranslated definition.
*)
let translate_with_recovery
    (sail_definition : Sail.sail_definition      )
    (translation     : Ast.Definition.t TC.t     ) : (Sail.sail_definition * Ast.Definition.t) TC.t
  =
  let S.DEF_aux (_, annotation) = sail_definition
  in
  let translation =
    let* result : Ast.Definition.t = translation
    in
    let* () = TC.store_definition result
    in
    (* print the definition's nanosail ast if the configuration says
      so. *)
    let () =
      if Configuration.(get dump_nanosail_ast)
        then Stdio.printf "Nanosail AST of a Sail definition:\n%s\n" (FExpr.to_string (Ast.Definition.to_fexpr result))
        else ()
    in
    TC.return (sail_definition, result)
  in
  TC.recover translation begin fun error ->
    match error with
    | NotYetImplemented (ocaml_location, sail_location, message) -> begin
        let untranslated_definition = Ast.Definition.UntranslatedDefinition
            {
              filename      = ocaml_location.pos_fname;
              line_number   = ocaml_location.pos_lnum ;
              sail_location                           ;
              message                                 ;
            }
        in
        TC.return (sail_definition, untranslated_definition)
      end
    | Failure (ocaml_location, message) -> begin
        let location_string =
          Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
        and pretty_printed_sail_code =
          PP.string @@ StringOf.Sail.definition sail_definition
        in
        let* () =
          let message = lazy begin
            let open PP
            in
            vertical [
              format "Assertion error at %s" location_string;
              horizontal [ string "Message: "; string message ];
              horizontal [ string "Sail code: "; pretty_printed_sail_code ];
            ]
          end
          in
          TC.log [%here] Logging.error message
        in
        let untranslated_definition = Ast.Definition.UntranslatedDefinition
            {
              filename      = ocaml_location.pos_fname;
              line_number   = ocaml_location.pos_lnum ;
              sail_location = annotation.loc          ;
              message       = Some message            ;
            }
        in
        TC.return (sail_definition, untranslated_definition)
      end
  end


(*
   Is the definition that of a union with type parameters that is translated to monomorphic copies?
*)
let is_specialised_union (sail_definition : Sail.sail_definition) : bool TC.t =
  let S.DEF_aux (unwrapped_sail_definition, _annotation) = sail_definition
  in
  let* union_instances = TC.union_instances
  in
  match unwrapped_sail_definition with
  | DEF_type (TD_aux (type_definition, _)) -> begin
      match type_definition with
      | TD_variant (identifier, _, _, _) -> TC.return @@ UnionInstances.is_specialised union_instances (Ast.Identifier.mk @@ Libsail.Ast_util.string_of_id identifier)
      | TD_abbrev (_, _, _)              -> TC.return false
      | TD_record (_, _, _, _)           -> TC.return false
      | TD_enum (_, _, _)                -> TC.return false
      | TD_bitfield (_, _, _)            -> TC.return false
      | TD_abstract (_, _, _)            -> TC.return false
    end
  | DEF_fundef _               -> TC.return false
  | DEF_mapdef _               -> TC.return false
  | DEF_impl _                 -> TC.return false
  | DEF_let (_, _)             -> TC.return false
  | DEF_val _                  -> TC.return false
  | DEF_outcome (_, _)         -> TC.return false
  | DEF_instantiation (_, _)   -> TC.return false
  | DEF_fixity (_, _, _)       -> TC.return false
  | DEF_overload (_, _)        -> TC.return false
  | DEF_default _              -> TC.return false
  | DEF_scattered _            -> TC.return false
  | DEF_measure (_, _, _)      -> TC.return false
  | DEF_loop_measures (_, _)   -> TC.return false
  | DEF_register _             -> TC.return false
  | DEF_internal_mutrec _      -> TC.return false
  | DEF_constraint _           -> TC.return false
  | DEF_pragma (_, _)          -> TC.return false


let translate_definition (sail_definition : Sail.sail_definition) : (Sail.sail_definition * Ast.Definition.t) TC.t =
  let pp_sail_definition : PP.t =
    PP.string @@ StringOf.Sail.definition sail_definition
  in
  let label =
    let open PP
    in
    vertical [
      string "Processing definition";
      indent pp_sail_definition
    ]
  in
  TC.translation_block [%here] label begin
    let S.DEF_aux (unwrapped_sail_definition, annotation) = sail_definition
    in
    let* specialised_union = is_specialised_union sail_definition
    in
    if
      should_ignore_definition sail_definition
    then
      let* () =
        let message = lazy begin
          PP.vertical [
            PP.string "Skipping this definition";
            pp_sail_definition
          ]
        end
        in
        TC.log [%here] Logging.info message
      in
      TC.return (sail_definition, Ast.Definition.IgnoredDefinition)
    else if
      specialised_union
    then
      (* The union is translated to its monomorphic copies, which are emitted separately (see UnionInstances) *)
      TC.return (sail_definition, Ast.Definition.IgnoredDefinition)
    else begin
      translate_with_recovery sail_definition begin
        match unwrapped_sail_definition with
        | DEF_type type_definition                 -> Translate.TypeDefinition.translate_type_definition annotation type_definition
        | DEF_let (pattern, expression)            -> Translate.ValueDefinition.translate_value_definition annotation pattern expression
        | DEF_val value_specification              -> Translate.TopLevelTypeConstraint.translate_top_level_type_constraint annotation value_specification
        | DEF_register specification               -> Translate.Register.translate_register annotation specification
        | DEF_fundef function_definition           -> Translate.Function.translate_function_definition sail_definition annotation function_definition
        | DEF_pragma (pragma, _argument)           -> TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] annotation.loc
        | DEF_fixity (_, _, _)                     -> TC.return Ast.Definition.IgnoredDefinition
        | DEF_outcome (_, _)                       -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_impl _                               -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_mapdef _                             -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_instantiation (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_overload (_, _)                      -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_default _                            -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_scattered _                          -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_measure (_, _, _)                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_loop_measures (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_internal_mutrec _                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_constraint _                         -> TC.not_yet_implemented [%here] annotation.loc
      end
    end
  end


(*
   Translates a monomorphic copy of a union with type parameters (see UnionInstances).
   It is paired with the Sail definition of the union.
*)
let translate_union_instance (instance : UnionInstances.Instance.t) : (Sail.sail_definition * Ast.Definition.t) list TC.t =
  let* union_instances = TC.union_instances
  in
  match UnionInstances.union_definition union_instances instance with
  | None -> TC.return []
  | Some sail_definition -> begin
      let label =
        PP.vertical [
          PP.string "Processing definition";
          PP.indent @@ PP.string @@ "copy of union " ^ UnionInstances.Instance.to_string instance
        ]
      in
      TC.translation_block [%here] label begin
        let* result =
          translate_with_recovery sail_definition begin
            let constructors =
              let make_constructor ((S.Tu_aux (Tu_ty_id (_, sail_identifier), annotation)), field_type, identifier) =
                let S.Id_aux (_, location) = sail_identifier
                in
                S.Tu_aux (Tu_ty_id (field_type, S.Id_aux (S.Id (Ast.Identifier.to_string identifier), location)), annotation)
              in
              List.map ~f:make_constructor instance.constructors
            in
            let* variant = Translate.Variant.translate_regular_variant instance.identifier (Ast.TypeQuantifier.TypeQuantifier []) constructors
            in
            TC.return @@ Ast.Definition.TypeDefinition variant
          end
        in
        TC.return [ result ]
      end
    end


(*
   Translates a definition, followed by the monomorphic copies of unions that are emitted after it.
*)
let translate_indexed_definition ((index, sail_definition) : int * Sail.sail_definition) : (Sail.sail_definition * Ast.Definition.t) list TC.t =
  let* translated = translate_definition sail_definition
  in
  let* union_instances = TC.union_instances
  in
  let* copies = TC.map ~f:translate_union_instance (UnionInstances.instances_after union_instances index)
  in
  TC.return @@ translated :: List.concat copies


(*
   Evaluates all top-level lets with Sail's interpreter, which is what Sail's constant folding uses.
   The interpreter evaluates the lets in order when it builds its initial state, so a let can refer
   to earlier ones (e.g., let max_bounds_cap = { null_cap with ... }).

   For each let, the result is its value, or why the interpreter could not compute it
   (e.g., because it reads a register or calls an extern that the interpreter does not know).

   Sail's rewrites turn implicit size arguments into make_the_value(n) and size_itself_int(n),
   externs that the interpreter does not know; both are the identity on integers.
*)
let evaluate_top_level_lets
    (ast : Sail.ast               )
    (env : Libsail.Type_check.env) : (Libsail.Ast.value, string) Result.t Libsail.Ast_compare.Bindings.t
  =
  let module Bindings = Libsail.Ast_compare.Bindings
  in
  let module Interpreter = Libsail.Interpreter
  in
  let primops =
    let identity (arguments : Libsail.Ast.value list) : Libsail.Ast.value =
      match arguments with
      | [ argument ] -> argument
      | []           -> failwith "expected one argument"
      | _ :: _ :: _  -> failwith "expected one argument"
    in
    Libsail.Constant_fold.safe_primops
    |> Libsail.Value.StringMap.add "make_the_value"  identity
    |> Libsail.Value.StringMap.add "size_itself_int" identity
  in
  match Interpreter.initial_state ~registers:false ast env primops with
  | exception _ -> Bindings.empty
  | (_local_state, global_state) -> begin
      (* Runs the interpreter on the expression of a let it could not evaluate, to find out why *)
      let explain_failure (expression : Sail.type_annotation Libsail.Ast.exp) : string =
        let rec run (frame : Interpreter.frame) : string =
          match frame with
          | Done (_, _)                         -> "the interpreter's value does not match the let's pattern"
          | Fail (_, _, _, _, message)          -> message
          | Step (_, _, _, _)                   -> run (Interpreter.eval_frame frame)
          | Break frame                         -> run (Interpreter.eval_frame frame)
          | Effect_request (_, _, _, _)         -> "evaluation needs an effect (e.g., reading a register)"
        in
        try
          run (Interpreter.Step (lazy "", ({ locals = Bindings.empty }, global_state), Interpreter.Monad.pure expression, []))
        with exn -> Stdlib.Printexc.to_string exn
      in
      let add_let
          (bindings   : (Libsail.Ast.value, string) Result.t Bindings.t)
          (definition : Sail.sail_definition                            ) : (Libsail.Ast.value, string) Result.t Bindings.t
        =
        let Libsail.Ast.DEF_aux (unwrapped_definition, _) = definition
        in
        match unwrapped_definition with
        | DEF_let (pattern, expression) -> begin
            let add_identifier
                (bindings   : (Libsail.Ast.value, string) Result.t Bindings.t)
                (identifier : Libsail.Ast.id                                  ) : (Libsail.Ast.value, string) Result.t Bindings.t
              =
              match Bindings.find_opt identifier global_state.letbinds with
              | Some value -> Bindings.add identifier (Result.Ok value) bindings
              | None       -> Bindings.add identifier (Result.Error (explain_failure expression)) bindings
            in
            List.fold_left ~init:bindings ~f:add_identifier (Libsail.Ast_compare.IdSet.elements (Libsail.Ast_util.pat_ids pattern))
          end
        | DEF_type _                  -> bindings
        | DEF_fundef _                -> bindings
        | DEF_mapdef _                -> bindings
        | DEF_impl _                  -> bindings
        | DEF_val _                   -> bindings
        | DEF_outcome (_, _)          -> bindings
        | DEF_instantiation (_, _)    -> bindings
        | DEF_fixity (_, _, _)        -> bindings
        | DEF_overload (_, _)         -> bindings
        | DEF_default _               -> bindings
        | DEF_scattered _             -> bindings
        | DEF_measure (_, _, _)       -> bindings
        | DEF_loop_measures (_, _)    -> bindings
        | DEF_register _              -> bindings
        | DEF_internal_mutrec _       -> bindings
        | DEF_constraint _            -> bindings
        | DEF_pragma (_, _)           -> bindings
      in
      List.fold_left ~init:Bindings.empty ~f:add_let ast.defs
    end


(* turn a given Sail ast into a Nanosail program, which is a list of
   sail definition - ast definition tuples together with a list of
   function monomorphizations. *)
let translate
    (ast : Sail.ast               )
    (env : Libsail.Type_check.env) : Ast.Program.t
  =
  (* a translation context with a list of sail definition - ast
     definition tuples *)
  let translate =
    (* store the definitions from the prelude in the translation
      context *)
    let* () = Prelude.register_types ()
    in
    let* () = TC.set_evaluated_top_level_values (evaluate_top_level_lets ast env)
    in
    let* () =
      let is_sum (identifier : Ast.Identifier.t) : bool =
        Option.is_some (Configuration.sum_union_for identifier)
      in
      TC.set_union_instances (UnionInstances.collect ast.defs env should_ignore_definition is_sum)
    in
    let* () =
      let* union_instances = TC.union_instances
      in
      let message = lazy begin
        PP.vertical [
          PP.string "Monomorphic copies of unions with type parameters";
          PP.indent @@ PP.vertical @@ List.map ~f:(fun instance -> PP.string @@ UnionInstances.Instance.to_string instance) (UnionInstances.instances union_instances)
        ]
      end
      in
      TC.log [%here] Logging.info message
    in
    (* the translation block just adds "entering" and "exiting" lines in
       the log. *)
    TC.translation_block
      [%here]
      (PP.string "Translating Sail to Nanosail")
      (* pair up each sail definition in the list with its nanosail
         definition.  thread the list of these computations monadically
         into one translation context. *)
      (TC.lift ~f:List.concat @@ TC.map ~f:translate_indexed_definition (List.mapi ast.defs ~f:(fun index definition -> (index, definition))))
  in
  (* run the context we built. it's a monadic Result with
     - a list of sail definition - nanosail definition pairs
     - a list of monomorphs
   *)
  let result = TC.run translate
  in
  match result with
  | TC.Success (definitions, polymorphic_argtypes) -> { definitions; polymorphic_argtypes }
  | TC.Failure _                                   -> failwith "Bug: failures should have been recovered from earlier"
