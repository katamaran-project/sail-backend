open Base
open Monads.Notations.Star(Slang.EvaluationContext)

module Blocks = Blocks
module Prelude = Prelude

module EC = Slang.EvaluationContext

module GC = struct
  include NanosailToMicrosail.GenerationContext
  include Monads.Util.Make(NanosailToMicrosail.GenerationContext)
end


let string_of_document document =
  let text_width = Configuration.(get output_width)
  and buffer     = Stdlib.Buffer.create 10000
  in
  PPrint.ToBuffer.pretty 1.0 text_width buffer document;
  Stdlib.Buffer.contents buffer


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
    and* fetch_generated = Prelude.prelude translation
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
  let output_line line =
    Stdio.Out_channel.output_lines output_channel [line]
  in
  let next_line () =
    Stdio.In_channel.input_line input_channel
  and is_block_entry line =
    is_template_block_start line
  and is_block_exit line =
    is_template_block_end line
  and process_out_of_block_line line =
    output_line line
  and process_block lines =
    let code = String.concat ~sep:"\n" lines
    in
    let generated_output = run_code translation code
    in
    output_line generated_output
  in
  Blocks.process_lines ~next_line ~is_block_entry ~is_block_exit ~process_out_of_block_line ~process_block


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
  let templates = Configuration.(get template_translations)
  in
  List.iter ~f:(fun { template_filename; output_filename } -> process_template sanitized_translation template_filename output_filename) templates
