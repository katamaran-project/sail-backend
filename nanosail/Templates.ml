open Base

module EC = Slang.EvaluationContext



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


let run_code (_source : string) =
  "TODO"


(* Processes a single template, given the input and output as channels *)
let process_template_streams
    (_translation   : Ast.program        )
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
      run_code code
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
  let templates = Configuration.(get template_files)
  in
  List.iter ~f:(fun (i, o) -> process_template translation i o) templates
