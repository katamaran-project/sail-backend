type monomorphization_request = {
  monomorphization_identifier : Ast.Identifier.t;
  substitutions : (Ast.Identifier.t * int) list;
}
type template_translation = {
  template_filename : string;
  output_filename : string;
}
module Implementation :
  sig
    val configuration_interpreter_state : Slang.State.t ref
    val run : 'a Slang.EvaluationContext.t -> 'a
    val export : string -> Slang.Value.t -> unit
    val export_callable : string -> Slang.Value.callable -> unit
    val read_file_contents : string -> string
    val load_configuration : string -> unit
    val export_strict_function :
      string -> (Slang.Value.t list -> unit) -> unit
    val generic_strict :
      string ->
      init:'a -> (Slang.Value.t list -> 'a) -> 'a ConfigLib.Setting.t
    val bool : string -> bool -> bool ConfigLib.Setting.t
    val integer : string -> int -> int ConfigLib.Setting.t
    val string : string -> string -> string ConfigLib.Setting.t
    val strings : string -> string list ConfigLib.Setting.t
    val string_to_string :
      string -> (string * string) list ConfigLib.Setting.t
    val callable :
      string ->
      Slang.Value.callable -> Slang.Value.callable ConfigLib.Setting.t
    val export_string_predicate_setter :
      string -> ((string -> bool) -> unit) -> unit
    val string_predicate :
      string -> (string -> bool) -> (string -> bool) ConfigLib.Setting.t
    val environment_variable :
      string -> 'a -> (string -> 'a) -> 'a ConfigLib.Setting.t
    val constant_function :
      arity:int -> return_value:Slang.Value.t -> Slang.Value.callable
    module EC = Slang.EvaluationContext
    module Exported :
      sig
        val base_name : string ConfigLib.Setting.t
        val program_name : string ConfigLib.Setting.t
        val verbosity_level_environment_variable : string
        val verbosity_level : LogLib.VerbosityLevel.t ConfigLib.Setting.t
        val pattern_tree_maximum_permutations : int ConfigLib.Setting.t
        val include_original_code : bool ConfigLib.Setting.t
        val ignore_overloads : bool ConfigLib.Setting.t
        val ignore_default_order : bool ConfigLib.Setting.t
        val pretty_print_lists : bool ConfigLib.Setting.t
        val pretty_print_let : bool ConfigLib.Setting.t
        val pretty_print_match_enum : bool ConfigLib.Setting.t
        val pretty_print_binary_operators : bool ConfigLib.Setting.t
        val pretty_print_function_calls : bool ConfigLib.Setting.t
        val show_generation_blocks : bool ConfigLib.Setting.t
        val bitvectors_zeros_ones_as_literal : bool ConfigLib.Setting.t
        val inline_definitions_in_notations : bool ConfigLib.Setting.t
        val annotate_functions_with_ast : bool ConfigLib.Setting.t
        val dump_nanosail_ast : bool ConfigLib.Setting.t
        val ignore_pragma_predicate : (string -> bool) ConfigLib.Setting.t
        val ignore_type_definition_predicate :
          (string -> bool) ConfigLib.Setting.t
        val ignore_value_definition_predicate :
          (string -> bool) ConfigLib.Setting.t
        val ignore_function_definition_predicate :
          (string -> bool) ConfigLib.Setting.t
        val ignore_top_level_type_constraint_predicate :
          (string -> bool) ConfigLib.Setting.t
        val template_block_left_delimiter : string ConfigLib.Setting.t
        val template_block_right_delimiter : string ConfigLib.Setting.t
        val template_translations :
          template_translation list ConfigLib.Setting.t
      end
    val monomorphization_requests :
      monomorphization_request list Ast.Identifier.Map.t ref
    val register_monomorphization_request :
      Ast.Identifier.t -> monomorphization_request -> unit
  end
val base_name : string ConfigLib.Setting.t
val program_name : string ConfigLib.Setting.t
val verbosity_level_environment_variable : string
val verbosity_level : LogLib.VerbosityLevel.t ConfigLib.Setting.t
val pattern_tree_maximum_permutations : int ConfigLib.Setting.t
val include_original_code : bool ConfigLib.Setting.t
val ignore_overloads : bool ConfigLib.Setting.t
val ignore_default_order : bool ConfigLib.Setting.t
val pretty_print_lists : bool ConfigLib.Setting.t
val pretty_print_let : bool ConfigLib.Setting.t
val pretty_print_match_enum : bool ConfigLib.Setting.t
val pretty_print_binary_operators : bool ConfigLib.Setting.t
val pretty_print_function_calls : bool ConfigLib.Setting.t
val show_generation_blocks : bool ConfigLib.Setting.t
val bitvectors_zeros_ones_as_literal : bool ConfigLib.Setting.t
val inline_definitions_in_notations : bool ConfigLib.Setting.t
val annotate_functions_with_ast : bool ConfigLib.Setting.t
val dump_nanosail_ast : bool ConfigLib.Setting.t
val ignore_pragma_predicate : (string -> bool) ConfigLib.Setting.t
val ignore_type_definition_predicate : (string -> bool) ConfigLib.Setting.t
val ignore_value_definition_predicate : (string -> bool) ConfigLib.Setting.t
val ignore_function_definition_predicate :
  (string -> bool) ConfigLib.Setting.t
val ignore_top_level_type_constraint_predicate :
  (string -> bool) ConfigLib.Setting.t
val template_block_left_delimiter : string ConfigLib.Setting.t
val template_block_right_delimiter : string ConfigLib.Setting.t
val template_translations : template_translation list ConfigLib.Setting.t
val requested_monomorphizations_for :
  Ast.Identifier.t -> monomorphization_request list option
val load_configuration : string -> unit
val get : 'a ConfigLib.Setting.t -> 'a
val set : 'a ConfigLib.Setting.t -> 'a -> unit
