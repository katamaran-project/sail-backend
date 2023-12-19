include Auxlib.Settings

type 'a setting = 'a t

let use_list_notations               = create false
let include_untranslated_definitions = create false
let include_original_code            = create false
let include_ignored_definitions      = create false
