open! ExtBase
open Monads.Notations.Star(Slang.EvaluationContext)
open Slang.Prelude.Shared

module Blocks = Blocks

module EC = Slang.EvaluationContext

module GC = struct
  include NanosailToMicrosail.GenerationContext
  include Monads.Util.Make(NanosailToMicrosail.GenerationContext)
end


let string_of_document =
  Fn.compose PP.string_of_document PP.undecorate


let html_of_document =
  PP.html_of_document


let nullary_string_function id func =
  (id, Slang.Helpers.Function.to_string id func)


let nullary_boolean_function id func =
  (id, Slang.Helpers.Function.to_bool id func)


let prelude (translation : NanosailToMicrosail.Katamaran.katamaran) =
  (*
    Allocate a refcell that holds a list of generated strings
    (in reverse order, for efficiency reasons)
  *)
  let* generated_output_reference =
    EC.heap_allocate Slang.Value.Nil
  in

  (* Adds the given string to the list of generated strings; not exported directly *)
  let generate_string (str : string) =
    let* current_output =
      EC.heap_access generated_output_reference
    in
    let extended_output =
      Slang.Value.Cons (Slang.Value.String str, current_output)
    in
    EC.heap_update generated_output_reference extended_output
  in

  (* Returns generated strings in concatenated form *)
  let fetch_generated =
    let* generated_string_list = EC.heap_access generated_output_reference
    in
    match Slang.Converters.(list string) generated_string_list with
    | Some strings -> begin
        EC.return @@ String.concat ~sep:"" @@ List.rev strings
      end
    | None -> failwith "Bug: somehow the list got corrupted"
  in

  (* (generate string1 string2 ...) *)
  let exported_generate =
    let id = "generate"
    and impl args =
      let=?? strings = List.map ~f:Slang.Converters.string args
      in
      let* () = EC.iter ~f:generate_string strings
      in
      EC.return (Some Slang.Value.Nil)
    in
      (id, Slang.Value.Callable (Slang.Functions.mk_multimethod [ impl; error id ]))
  in

  let exported_base_translation =
    let id = "base-translation"
    in
    let f () =
      EC.return @@ string_of_document @@ GC.generate translation#program translation#pp_base
    in
    nullary_string_function id f
  in

  let exported_base_html_translation =
    let id = "base-html-translation"
    in
    let f () =
      EC.return @@ Html.to_string @@ html_of_document @@ GC.generate translation#program translation#pp_base
    in
    nullary_string_function id f
  in

  let exported_program_translation =
    let id = "program-translation"
    in
    let f () =
      EC.return @@ string_of_document @@ GC.generate translation#program translation#pp_program
    in
    nullary_string_function id f
  in

  let exported_program_html_translation =
    let id = "program-html-translation"
    in
    let f () =
      EC.return @@ Html.to_string @@ html_of_document @@ GC.generate translation#program translation#pp_program
    in
    nullary_string_function id f
  in

  let exported_ignored_definitions =
    let id = "ignored-definitions"
    in
    let f () =
      let ignored_definitions =
        List.map ~f:fst translation#ignored_definitions
      in
      let formatted_ignored_definitions =
        (* todo improve this *)
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
      EC.return @@ string_of_document formatted_ignored_definitions
    in
    nullary_string_function id f
  in

  let exported_untranslated_definitions =
    let id = "untranslated-definitions"
    in
    let f () =
      let untranslated_definitions =
        translation#untranslated_definitions
      in
      let formatted_untranslated_definitions =
        PP.(paragraphs @@ List.map ~f:(Fn.uncurry NanosailToMicrosail.Untranslated.generate) untranslated_definitions)
      in
      EC.return @@ string_of_document formatted_untranslated_definitions
    in
    nullary_string_function id f
  in

  let exported_untranslated_definitions_predicate =
    let id = "untranslated-definitions?"
    in
    let f () =
      EC.return @@ not @@ List.is_empty translation#untranslated_definitions
    in
    nullary_boolean_function id f
  in

  let exported : (string * Slang.Value.t) list = [
    exported_generate;
    exported_base_translation;
    exported_base_html_translation;
    exported_program_translation;
    exported_program_html_translation;
    exported_ignored_definitions;
    exported_untranslated_definitions;
    exported_untranslated_definitions_predicate;
  ]
  in
  let* () = EC.iter exported ~f:(fun (id, callable) -> EC.add_binding id callable)
  in
  EC.return fetch_generated
