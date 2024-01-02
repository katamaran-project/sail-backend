include Auxlib.Settings

type 'a setting = 'a t

(* Use list notations *)
let use_list_notations               = create false

(* Output definitions for which no translation is available yet *)
let include_untranslated_definitions = create false

(* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_original_code            = create false

(* Output ignored definitions *)
let include_ignored_definitions      = create false

(* Predicate that determines which Sail definitions to ignore *)
let ignore_definition = create (fun (_ : Ast.sail_definition) -> false)
