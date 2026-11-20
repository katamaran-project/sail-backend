(*
   What a template block can generate (docs/usage.md#template-files):

     (*< base-translation >*)
     (*< (when untranslated-definitions "Theorem UNTRANSLATED_DEFINITIONS : False. Qed.") >*)

   A block contains output names, which are replaced by the outputs below, strings, which are
   copied, and (when <condition> <contents> ...), whose contents are generated only if the
   condition holds. The generated texts are concatenated.
*)
open ExtBase

module GC = struct
  include NanosailToMicrosail.GenerationContext
  include Monads.Util.Make(NanosailToMicrosail.GenerationContext)
end


let string_of_document =
  PP.to_string <. PP.undecorate


let html_of_document =
  PP.to_html


let output_names : string list =
  [
    "base-translation";
    "base-html-translation";
    "program-translation";
    "program-html-translation";
    "ignored-definitions";
    "untranslated-definitions";
    "argument-types-of-polymorphic-function-calls";
  ]


let condition_names : string list =
  [
    "untranslated-definitions";
  ]


(* The text of the output with the given name, if there is one *)
let output
    (translation : NanosailToMicrosail.Katamaran.katamaran)
    (name        : string                                 ) : string option
  =
  let string_of (document : PP.t GC.t) : string =
    string_of_document @@ GC.generate translation#program document
  and html_of (document : PP.t GC.t) : string =
    Html.to_string @@ html_of_document @@ GC.generate translation#program document
  in
  match name with
  | "base-translation"         -> Some (string_of translation#pp_base)
  | "base-html-translation"    -> Some (html_of translation#pp_base)
  | "program-translation"      -> Some (string_of translation#pp_program)
  | "program-html-translation" -> Some (html_of translation#pp_program)
  | "ignored-definitions"      -> begin
      let ignored_definitions =
        List.map ~f:fst translation#ignored_definitions
      in
      let formatted_ignored_definitions =
        let open Monads.Notations.Star(GC)
        in
        let result =
          let* ignored_definitions' =
            GC.map ~f:NanosailToMicrosail.Ignored.generate ignored_definitions
          in
          GC.return @@ PP.paragraphs ignored_definitions'
        in
        GC.generate translation#program result
      in
      Some (string_of (GC.return formatted_ignored_definitions))
    end
  | "untranslated-definitions" -> begin
      (* Each definition is escaped on its own, so that the listing is a valid Rocq comment *)
      let format_untranslated_definition
          (sail_definition         : Sail.sail_definition         )
          (untranslated_definition : Ast.Definition.Untranslated.t) : string
        =
        NanosailToMicrosail.Coq.escape_for_rocq_comment @@
        string_of_document @@
        NanosailToMicrosail.Untranslated.generate sail_definition untranslated_definition
      in
      Some begin
        String.concat ~sep:"\n\n" @@
        List.map ~f:(Fn.uncurry format_untranslated_definition) translation#untranslated_definitions
      end
    end
  | "argument-types-of-polymorphic-function-calls" -> Some (string_of translation#pp_argument_types_of_polymorphic_function_calls)
  | _ -> None


(* Whether the condition with the given name holds, if there is one *)
let condition
    (translation : NanosailToMicrosail.Katamaran.katamaran)
    (name        : string                                 ) : bool option
  =
  match name with
  | "untranslated-definitions" -> Some (not @@ List.is_empty translation#untranslated_definitions)
  | _                          -> None


(* The text a block generates *)
let render
    (translation : NanosailToMicrosail.Katamaran.katamaran)
    ~(file       : string                                 )
    (contents    : string                                 ) : string
  =
  let module Node = ConfigurationFile.Node
  in
  let error = ConfigurationFile.error
  in
  let rec render_node (node : Node.t) : string =
    match node with
    | Atom { text; quoted = true; _ } -> text
    | Atom { text; quoted = false; location } -> begin
        match output translation text with
        | Some text -> text
        | None      -> error location @@ Printf.sprintf "unknown template output %s; expected one of %s" text (String.concat ~sep:", " output_names)
      end
    | List { elements = Atom { text = "when"; quoted = false; _ } :: Atom { text = name; quoted = false; location } :: contents; _ } -> begin
        match condition translation name with
        | Some true  -> String.concat ~sep:"" @@ List.map contents ~f:render_node
        | Some false -> ""
        | None       -> error location @@ Printf.sprintf "unknown template condition %s; expected one of %s" name (String.concat ~sep:", " condition_names)
      end
    | List { location; _ } -> error location @@ Printf.sprintf "expected an output name, a string or (when <condition> ...), not %s" (Node.to_string node)
  in
  String.concat ~sep:"" @@ List.map (ConfigurationFile.parse_string ~file contents) ~f:render_node
