open Base
open Monads.Notations.Star(Slang.EvaluationContext)

module EC = Slang.EvaluationContext
module PP = PPrint


let string_of_document document =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer


let template_prelude (translation : Ast.program) =
  let nullary_function id func =
    let f (arguments : Slang.Value.t list) =
      match arguments with
      | [] -> begin
          let string = func ()
          in
          EC.return @@ Slang.Value.Mk.string string
        end
      | _  -> failwith @@ Printf.sprintf "%s does not expect arguments" id
    in
    (id, Slang.Functions.mk_strict_function f)
  in

  let full_translation =
    let id = "full-translation"
    in
    let f () =
      string_of_document @@ NanosailToMicrosail.Katamaran.pretty_print translation
    in
    nullary_function id f
  in

  let ignored_definitions =
    let id = "ignored-definitions"
    in
    let f () =
      let ignored_definitions =
        List.map ~f:fst @@ Ast.(select Extract.ignored_definition translation.definitions)
      in
      let formatted_ignored_definitions =
        PPrint.(separate (twice hardline) @@ List.map ~f:NanosailToMicrosail.Ignored.generate ignored_definitions)
      in
      string_of_document formatted_ignored_definitions
    in
    nullary_function id f
  in

  let untranslated_definitions =
    let id = "untranslated-definitions"
    in
    let f () =
      let untranslated_definitions =
        Ast.(select Extract.untranslated_definition translation.definitions)
      in
      let formatted_untranslated_definitions =
        PPrint.(separate (twice hardline) @@ List.map ~f:(Auxlib.uncurry NanosailToMicrosail.Untranslated.generate) untranslated_definitions)
      in
      string_of_document formatted_untranslated_definitions
    in
    nullary_function id f
  in

  let exported = [
    full_translation;
    ignored_definitions;
    untranslated_definitions;
  ]
  in
  EC.iter exported ~f:(fun (id, callable) -> EC.add_binding id callable)


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
    and* () = template_prelude translation
    in
    Slang.Evaluation.evaluate_string source
  in
  let value, _state = Slang.EvaluationContext.run program
  in
  match value with
   | Slang.Value.String string -> string
   | _                         -> failwith "Code should produce string"


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
    let generated_output =
      run_code translation code
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
