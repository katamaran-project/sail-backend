open Base


type sail_definition = Ast.sail_definition
type sail_definition_predicate = sail_definition -> bool


let use_list_notations               = Configlib.bool "use-list-notations"               (* Use list notations                                                          *)
let include_untranslated_definitions = Configlib.bool "include-untranslated-definitions" (* Output definitions for which no translation is available yet                *)
let include_original_code            = Configlib.bool "include-original-code"            (* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_ignored_definitions      = Configlib.bool "include-ignored-definitions"      (* Output ignored definitions                                                  *)
(* let ignore_definition                = create (Fn.const false)    (\* Predicate that determines which Sail definitions to ignore                  *\) *)


(* let ignore_pragma pragma (Libsail.Ast.DEF_aux (definition, _annotation)) = *)
(*   match definition with *)
(*   | Libsail.Ast.DEF_pragma (p, _argument, _location) -> String.equal p pragma *)
(*   | _                                                -> false *)

(* let ignore_pragmas pragmas = *)
(*   List.fold_left ~f:(fun acc p -> acc |.| ignore_pragma p) ~init:(Fn.const false) pragmas *)


(* module Script = struct *)
(*   open Slang *)
(*   open Slang.Evaluation_context *)
  
(*   let boolean_setter setting args = *)
(*     if List.is_empty args *)
(*     then begin *)
(*       set setting true; *)
(*       return Value.Nil *)
(*     end else *)
(*       failwith "No arguments expected" *)
(* end *)

include Configlib.Exported

(* let load_configuration_file = Configlib.load_configuration_file *)
  (* let open Slang *)
  (* in *)
  (* let contents = Stdio.In_channel.read_all path *)
  (* in *)
  (* let environment = extend_environment prelude @@ fun { native_function; _ } -> begin *)
  (*     native_function "use-list-notations"               @@ Script.boolean_setter use_list_notations; *)
  (*     native_function "include-untranslated-definitions" @@ Script.boolean_setter include_untranslated_definitions; *)
  (*     native_function "include-original-code"            @@ Script.boolean_setter include_original_code; *)
  (*     native_function "include-ignored-definitions"      @@ Script.boolean_setter include_ignored_definitions; *)
  (*   end *)
  (* in *)
  (* ignore @@ run_string environment contents *)
