open Base

module EC = Slang.EvaluationContext


let process
      (_translation : Ast.program)
  =
  let templates = Configuration.(get template_files)
  in
  Stdio.printf "PROCESSING TEMPLATES\n";
  List.iter ~f:(fun (x, y) -> Stdio.printf "**************** %s -> %s\n" x y) templates
