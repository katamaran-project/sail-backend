open Base
open Monads.Notations.Star(Slang.EvaluationContext)
open Slang.Prelude.Shared

module EC = Slang.EvaluationContext
module GC = NanosailToMicrosail.GenerationContext


let string_of_document document =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer


let template_prelude (translation : Ast.program) =
  (* Allocate a refcell that holds a list of generated strings (in reverse order, for efficiency purposes) *)
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

  let nullary_string_function id func =
    (id, Slang.Helpers.Function.to_string id func)
  and nullary_boolean_function id func =
    (id, Slang.Helpers.Function.to_bool id func)
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

  let exported_full_translation =
    let id = "full-translation"
    in
    let f () =
      EC.return @@ string_of_document @@ NanosailToMicrosail.Katamaran.full_translation translation
    in
    nullary_string_function id f
  in

  let exported_ignored_definitions =
    let id = "ignored-definitions"
    in
    let f () =
      let ignored_definitions =
        List.map ~f:fst @@ Ast.(select Extract.ignored_definition translation.definitions)
      in
      let formatted_ignored_definitions =
        (* todo improve this *)
        let open Monads.Notations.Star(GC)
        in
        let result =
          let* ignored_definitions' =
            GC.map ~f:NanosailToMicrosail.Ignored.generate ignored_definitions
          in
          GC.return @@ PP.vertical ~spacing:2 ignored_definitions'
        in
        GC.generate result
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
        Ast.(select Extract.untranslated_definition translation.definitions)
      in
      let formatted_untranslated_definitions =
        PPrint.(separate (twice hardline) @@ List.map ~f:(Auxlib.uncurry NanosailToMicrosail.Untranslated.generate) untranslated_definitions)
      in
      EC.return @@ string_of_document formatted_untranslated_definitions
    in
    nullary_string_function id f
  in

  let exported_untranslated_definitions_predicate =
    let id = "untranslated-definitions?"
    in
    let f () =
      EC.return @@ not @@ List.is_empty @@ Ast.(select Extract.untranslated_definition translation.definitions)
    in
    nullary_boolean_function id f
  in

  let exported : (string * Slang.Value.t) list = [
    exported_generate;
    exported_full_translation;
    exported_ignored_definitions;
    exported_untranslated_definitions;
    exported_untranslated_definitions_predicate;
  ]
  in
  let* () = EC.iter exported ~f:(fun (id, callable) -> EC.add_binding id callable)
  in
  EC.return fetch_generated


let is_template_block_start line =
  let left_delimiter =
    Configuration.(get template_block_left_delimiter)
  in
  String.equal (String.rstrip line) left_delimiter


let is_template_block_end line =
  let right_delimiter =
    Configuration.(get template_block_right_delimiter)
  in
  String.equal (String.rstrip line) right_delimiter


let run_code
      (translation : Ast.program)
      (source      : string     ) : string =
  let program =
    let* () = Slang.Prelude.initialize
    and* fetch_generated = template_prelude translation
    in
    let* _ = Slang.Evaluation.evaluate_string source
    in
    fetch_generated
  in
  let value, _state = Slang.EvaluationContext.run program
  in
  match value with
   | Success string -> string
   | _              -> failwith "Code should produce string"


(* Processes a single template, given the input and output as channels *)
let process_template_streams
    (translation    : Ast.program        )
    (input_channel  : Stdio.In_channel.t )
    (output_channel : Stdio.Out_channel.t)
  =
  let inside_template_block = ref false
  and block_acc = ref []
  in
  let accumulate_line line =
    block_acc := line :: !block_acc
  and output_line line =
    Stdio.Out_channel.output_lines output_channel [line]
  in

  let process_block () =
    let code = String.concat ~sep:"\n" @@ List.rev !block_acc
    in
    let generated_output = run_code translation code
    in
    output_line generated_output
  in

  let start_new_block () =
    if !inside_template_block
    then failwith "Nested template blocks are not allowed"
    else inside_template_block := true

  and finish_block () =
    if !inside_template_block
    then begin
      process_block ();
      inside_template_block := false;
      block_acc := []
    end
    else failwith "Unexpected end of template block"

  and process_line line =
    if !inside_template_block
    then accumulate_line line
    else output_line line
  in

  Stdio.In_channel.iter_lines input_channel ~f:begin fun line ->
    if is_template_block_start line
    then start_new_block ()
    else if is_template_block_end line
    then finish_block ()
    else process_line line
  end


(* Processes a single template, given the names of the input and output files *)
let process_template
    (translation : Ast.program)
    (input_file  : string     )
    (output_file : string     )
  =
  Stdio.In_channel.with_file ~binary:false input_file ~f:begin fun input_stream ->
    Stdio.Out_channel.with_file output_file ~f:begin fun output_stream ->
      process_template_streams translation input_stream output_stream
    end
  end


(* Processes all templates defined in the configuration *)
let process (translation : Ast.program) =
  let sanitized_translation = SailToNanosail.coqify_identifiers translation
  in
  let templates = Configuration.(get template_files)
  in
  List.iter ~f:(fun (i, o) -> process_template sanitized_translation i o) templates
