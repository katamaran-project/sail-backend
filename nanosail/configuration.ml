open Auxlib
include Auxlib.Settings

type 'a setting = 'a t

type sail_definition = Ast.sail_definition

type sail_definition_predicate = sail_definition -> bool

(* Use list notations *)
let use_list_notations               = create false

(* Output definitions for which no translation is available yet *)
let include_untranslated_definitions = create false

(* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_original_code            = create false

(* Output ignored definitions *)
let include_ignored_definitions      = create false

(* Predicate that determines which Sail definitions to ignore *)
let ignore_definition                = create (fun (_ : Ast.sail_definition) -> false)


let ignore_pragma pragma (Libsail.Ast.DEF_aux (definition, _annotation)) =
  match definition with
  | Libsail.Ast.DEF_pragma (p, _argument, _location) -> p = pragma
  | _                                                -> false

let ignore_pragmas pragmas =
  List.fold_left (fun acc p -> acc |.| ignore_pragma p) (fun _ -> false) pragmas
