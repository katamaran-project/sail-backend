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
  Fn.compose PP.to_string PP.undecorate


let html_of_document =
  PP.to_html


let nullary_string_function id func =
  (id, Slang.Helpers.Function.to_string id func)


let nullary_boolean_function id func =
  (id, Slang.Helpers.Function.to_bool id func)


class virtual exported_function (identifier : string) = object
  method identifier = identifier
  method virtual callable : Slang.Value.t
end


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

  let exported_functions : exported_function list = [
    object(self)
      inherit exported_function "generate"

      method callable =
        let implementation args =
          let=?? strings = List.map ~f:Slang.Converters.string args
          in
          let* () = EC.iter ~f:generate_string strings
          in
          EC.return (Some Slang.Value.Nil)
        in
        Slang.Value.Callable (Slang.Functions.mk_multimethod [ implementation; error self#identifier ])
    end;

    object(self)
      inherit exported_function "base-translation"

      method callable =
        let implementation () =
          EC.return @@ string_of_document @@ GC.generate translation#program translation#pp_base
        in
        Slang.Helpers.Function.to_string self#identifier implementation
    end;

    object(self)
      inherit exported_function "base-html-translation"

      method callable =
        let implementation () =
          EC.return @@ Html.to_string @@ html_of_document @@ GC.generate translation#program translation#pp_base
        in
        Slang.Helpers.Function.to_string self#identifier implementation        
    end;

    object(self)
      inherit exported_function "program-translation"

      method callable =
        let implementation () =
          EC.return @@ string_of_document @@ GC.generate translation#program translation#pp_program
        in
        Slang.Helpers.Function.to_string self#identifier implementation
    end;

    object(self)
      inherit exported_function "program-html-translation"

      method callable =
        let implementation () =
          EC.return @@ Html.to_string @@ html_of_document @@ GC.generate translation#program translation#pp_program
        in
        Slang.Helpers.Function.to_string self#identifier implementation
    end;

    object(self)
      inherit exported_function "ignored-definitions"

      method callable =
        let implementation () =
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
          EC.return @@ string_of_document formatted_ignored_definitions
        in
        Slang.Helpers.Function.to_string self#identifier implementation
    end;

    object(self)
      inherit exported_function "untranslated-definitions"

      method callable =
        let implementation () =
          let untranslated_definitions =
            translation#untranslated_definitions
          in
          let formatted_untranslated_definitions =
            PP.(paragraphs @@ List.map ~f:(Fn.uncurry NanosailToMicrosail.Untranslated.generate) untranslated_definitions)
          in
          EC.return @@ string_of_document formatted_untranslated_definitions
        in
        Slang.Helpers.Function.to_string self#identifier implementation
    end;    

    object(self)
      inherit exported_function "untranslated-definitions?"

      method callable =
        let implementation () =
          EC.return @@ not @@ List.is_empty translation#untranslated_definitions
        in
        Slang.Helpers.Function.to_bool self#identifier implementation
    end;    
  ]
  in
  let* () =
    EC.iter
      exported_functions
      ~f:(fun exported_function -> EC.add_binding exported_function#identifier exported_function#callable)
  in
  EC.return fetch_generated
