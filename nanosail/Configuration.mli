module Setting : sig
  type 'a t

  val mk     : 'a -> 'a t
  val get    : 'a t -> 'a
  val set    : 'a t -> 'a -> unit
  val update : 'a t -> f:('a -> 'a) -> unit
end

type monomorphization_request = {
  monomorphization_identifier : Ast.Identifier.t;
  substitutions : (Ast.Identifier.t * int) list;
}
type template_translation = {
  template_filename : string;
  output_filename : string;
}
type sum_union = {
  union_identifier : Ast.Identifier.t;
  left_constructor : Ast.Identifier.t;
  right_constructor : Ast.Identifier.t;
}
module FunctionExclusion :
  sig
    type t = NoOp | Failure | Unconstrained | Value of string
    val equal : t -> t -> bool
    val configuration_name : t -> string
  end
module ForeignFunctionSemantics :
  sig
    type t = Given of string | Unconstrained
    val equal : t -> t -> bool
  end

val base_name : string Setting.t
val program_name : string Setting.t
val preserve_file_structure : bool Setting.t
val preserve_file_structure_root : string Setting.t
val preserve_file_structure_directory : string Setting.t
val verbosity_level_environment_variable : string
val verbosity_level : LogLib.VerbosityLevel.t Setting.t
val pattern_tree_maximum_permutations : int Setting.t
val include_original_code : bool Setting.t
val ignore_overloads : bool Setting.t
val ignore_default_order : bool Setting.t
val pretty_print_lists : bool Setting.t
val pretty_print_let : bool Setting.t
val pretty_print_match_enum : bool Setting.t
val pretty_print_binary_operators : bool Setting.t
val pretty_print_function_calls : bool Setting.t
val show_generation_blocks : bool Setting.t
val bitvectors_zeros_ones_as_literal : bool Setting.t
val inline_definitions_in_notations : bool Setting.t
val annotate_functions_with_ast : bool Setting.t
val dump_nanosail_ast : bool Setting.t
val excluded_functions : FunctionExclusion.t Base.Map.M(Base.String).t Setting.t
val excluded_registers : Base.Set.M(Base.String).t Setting.t
val foreign_function_semantics : ForeignFunctionSemantics.t Base.Map.M(Base.String).t Setting.t
val renamed_identifiers : string Base.Map.M(Base.String).t Setting.t
val template_block_left_delimiter : string Setting.t
val template_block_right_delimiter : string Setting.t
val template_translations : template_translation list Setting.t
val sum_unions : sum_union list Setting.t

val requested_monomorphizations_for : Ast.Identifier.t -> monomorphization_request list option
val sum_union_for : Ast.Identifier.t -> sum_union option

(* Reads the configuration file (docs/usage.md#configuration-file); raises Failure on an error, with its location *)
val load_configuration : string -> unit

val get : 'a Setting.t -> 'a
val set : 'a Setting.t -> 'a -> unit

val should_ignore_pragma : string -> bool
val should_ignore_type_definition : string -> bool
val should_ignore_function_definition : string -> bool
val should_ignore_top_level_type_constraint : string -> bool
val should_ignore_value_definition : string -> bool
val function_exclusion : string -> FunctionExclusion.t option
val is_excluded_register : string -> bool
val renamed : string -> string
val foreign_function_semantics_for : string -> ForeignFunctionSemantics.t option
