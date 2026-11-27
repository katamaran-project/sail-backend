val generate_program_prelude : unit -> PP.t GenerationContext.t

val pp_program_module : (Sail.sail_definition * Ast.Definition.Function.t) list -> (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list -> PP.t GenerationContext.t

(* The program split over several files, for preserve-file-structure (see ProgramModule.ml) *)
type files =
  {
    declarations         : PP.t;
    sail_files_directory : string;
    sail_files           : (FileStructure.ModulePath.t * PP.t) list;
    assembly             : PP.t;
  }

val declarations_module_name : unit -> string

val generate_files :
  declarations_library : string ->
  library_directories  : string list ->
  current_directory    : string ->
  sail_files_directory : string ->
  root                 : string option ->
  reserved_names       : string list ->
  (Sail.sail_definition * Ast.Definition.Type.t) list ->
  (Sail.sail_definition * Ast.Definition.Function.t) list ->
  (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list ->
  files GenerationContext.t
