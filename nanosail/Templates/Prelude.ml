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


class virtual exported_function (identifier : string) = object
  method identifier = identifier
  method virtual callable : Slang.Value.t
end

class virtual exported_nullary_string_function (identifier : string) = object(self)
  inherit exported_function identifier

  method callable =
    Slang.Helpers.Function.to_string self#identifier (fun () -> self#generate_string)

  method virtual generate_string : string EC.t
end

class virtual string_of_document_exported_function (identifier : string) (program : Ast.program) = object(self)
  inherit exported_nullary_string_function identifier

  method generate_string =
    EC.return @@ string_of_document @@ GC.generate program self#document

  method virtual document : PP.t GC.t
end

class virtual html_of_document_exported_function (identifier : string) (program : Ast.program) = object(self)
  inherit exported_nullary_string_function identifier

  method generate_string =
    EC.return @@ Html.to_string @@ html_of_document @@ GC.generate program self#document

  method virtual document : PP.t GC.t
end

class virtual exported_nullary_boolean_function (identifier : string) = object(self)
  inherit exported_function identifier

  method callable =
    Slang.Helpers.Function.to_bool self#identifier (fun () -> self#implementation)

  method virtual implementation : bool EC.t
end


(* helper function to easily cast from a subtype of exported_function to exported_function *)
let export (f : #exported_function) : exported_function = (f :> exported_function)

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
    export object(self)
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

    export object
      inherit string_of_document_exported_function "base-translation" translation#program
          
      method document =
        translation#pp_base
    end;

    export object
      inherit html_of_document_exported_function "base-html-translation" translation#program
          
      method document =
        translation#pp_base
    end;

    export object
      inherit string_of_document_exported_function "program-translation" translation#program

      method document =
        translation#pp_program
    end;

    export object
      inherit html_of_document_exported_function "program-html-translation" translation#program

      method document =
        translation#pp_program
    end;

    export object
      inherit string_of_document_exported_function "ignored-definitions" translation#program

      method document =
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
        GC.return formatted_ignored_definitions
    end;

    export object
      inherit string_of_document_exported_function "untranslated-definitions" translation#program

      method document =
        let untranslated_definitions =
          translation#untranslated_definitions
        in
        let formatted_untranslated_definitions =
          PP.(paragraphs @@ List.map ~f:(Fn.uncurry NanosailToMicrosail.Untranslated.generate) untranslated_definitions)
        in
        GC.return formatted_untranslated_definitions
    end;    

    export object
      inherit exported_nullary_boolean_function "untranslated-definitions?"

      method implementation =
        EC.return @@ not @@ List.is_empty translation#untranslated_definitions
    end;

    export object
      inherit exported_nullary_string_function "argument-types-of-polymorphic-function-calls"

      method generate_string =
        EC.return @@ Html.to_string @@ html_of_document @@ GC.generate translation#program translation#pp_argument_types_of_polymorphic_function_calls
    end
  ]
  in
  let* () =
    EC.iter
      exported_functions
      ~f:(fun exported_function -> EC.add_binding exported_function#identifier exported_function#callable)
  in
  EC.return fetch_generated
