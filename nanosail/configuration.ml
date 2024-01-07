open Base
open Auxlib
include Auxlib.Settings

type 'a setting = 'a t

type sail_definition = Ast.sail_definition

type sail_definition_predicate = sail_definition -> bool

let use_list_notations               = create false               (* Use list notations *)
let include_untranslated_definitions = create false               (* Output definitions for which no translation is available yet *)
let include_original_code            = create false               (* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_ignored_definitions      = create false               (* Output ignored definitions *)
let ignore_definition                = create (Fn.const false)    (* Predicate that determines which Sail definitions to ignore *)


let ignore_pragma pragma (Libsail.Ast.DEF_aux (definition, _annotation)) =
  match definition with
  | Libsail.Ast.DEF_pragma (p, _argument, _location) -> String.equal p pragma
  | _                                                -> false

let ignore_pragmas pragmas =
  List.fold_left ~f:(fun acc p -> acc |.| ignore_pragma p) ~init:(Fn.const false) pragmas
  
let load_configuration_file path =
  let open Slang in
  let open Slang.Evaluation_context
  in
  let contents = Stdio.In_channel.read_all path
  in
  let environment = extend_environment prelude @@ fun { native_function; _ } -> begin
      native_function "use-list-notations" @@ fun _ -> (set use_list_notations true; return Value.Nil)
    end
  in
  ignore @@ run_string environment contents
