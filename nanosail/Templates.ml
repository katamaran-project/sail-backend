open Base

module EC = Slang.EvaluationContext


let process_template input_file output_file =
  Stdio.printf "%s => %s\n" input_file output_file


let process
      (_translation : Ast.program)
  =
  let templates = Configuration.(get template_files)
  in
  List.iter ~f:(Auxlib.uncurry process_template) templates
