open ExtBase

module Blocks  = Blocks
module Outputs = Outputs

type katamaran = NanosailToMicrosail.Katamaran.katamaran


(* Processes a single template, given the input and output as channels *)
let process_template_streams
    (translation    : katamaran          )
    ~(template_name : string             )
    (input_channel  : Stdio.In_channel.t )
    (output_channel : Stdio.Out_channel.t) : unit
  =
  let module Input = struct
    let next_line () =
      Stdio.In_channel.input_line input_channel

    let is_block_entry line =
      let left_delimiter =
        Configuration.(get template_block_left_delimiter)
      in
      String.equal (String.rstrip line) left_delimiter

    let is_block_exit line =
      let right_delimiter =
        Configuration.(get template_block_right_delimiter)
      in
      String.equal (String.rstrip line) right_delimiter
  end
  in
  let output_line line =
    Stdio.Out_channel.output_lines output_channel [line]
  in
  let process_out_of_block_line line =
    output_line line
  and process_block lines =
    let contents = String.concat ~sep:"\n" lines
    in
    output_line @@ Outputs.render translation ~file:template_name contents
  in
  Blocks.process_lines (module Input) ~process_out_of_block_line ~process_block


(* Processes a single template, given the names of the input and output files *)
let process_template
    (translation : katamaran)
    (input_file  : string   )
    (output_file : string   ) : unit
  =
  Logging.info [%here] @@ lazy (PP.format "Processing template %s -> %s" input_file output_file);
  Stdio.In_channel.with_file ~binary:false input_file ~f:begin fun input_stream ->
    Stdio.Out_channel.with_file output_file ~f:begin fun output_stream ->
      process_template_streams translation ~template_name:input_file input_stream output_stream
    end
  end


(* Processes all templates defined in the configuration *)
let process (translation : Ast.Program.t) =
  let sanitized_translation =
    new NanosailToMicrosail.Katamaran.katamaran translation
  in
  let templates = Configuration.(get template_translations)
  in
  List.iter ~f:(fun { template_filename; output_filename } -> process_template sanitized_translation template_filename output_filename) templates
