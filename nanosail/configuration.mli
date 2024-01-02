type 'a setting

type sail_definition_predicate = Ast.sail_definition -> bool

val use_list_notations               : bool setting
val include_untranslated_definitions : bool setting
val include_original_code            : bool setting
val include_ignored_definitions      : bool setting
val ignore_definition                : (Ast.sail_definition -> bool) setting

val get : 'a setting -> 'a
val set : 'a setting -> 'a -> unit
