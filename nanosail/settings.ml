let configure_list_notations enabled =
  Gen.Katamaran.opt_list_notations := enabled

let set_include_untranslated_definitions enabled =
  Gen.Katamaran.opt_include_untranslated := enabled

let set_include_original_code enabled =
  Gen.Coq.include_original_sail_code := enabled
