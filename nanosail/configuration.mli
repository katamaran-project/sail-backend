type 'a setting

val use_list_notations               : bool setting
val include_untranslated_definitions : bool setting
val include_original_code            : bool setting
val include_ignored_definitions      : bool setting

val get : 'a setting -> 'a
val set : 'a setting -> 'a -> unit
