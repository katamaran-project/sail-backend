open Base
open Auxlib
open Slang.EvaluationContext
open Monads.Notations.Star(Slang.EvaluationContext)


(* List of native functions to be made accessible in configuration script *)
let exported_functions = ref []

type 'a getter  = unit -> 'a
type 'a setter  = 'a -> unit
type 'a setting = Setting of 'a getter * 'a setter


let register_script_function
      (identifier : string              )
      (callable   : Slang.Value.callable) =
  exported_functions := (identifier, callable) :: !exported_functions


let create_setting_cell initial_value =
  let cell = ref initial_value
  in
  let get () = !cell
  and set b  = cell := b
  in
  (get, set)


(*
  Creates a Slang function named <export_as> that takes a boolean argument.
  Calling this function causes a refcell to be set with this argument.
*)
let bool ?(init=false) export_as =
  let get, set = create_setting_cell init
  in
  let script_function _values =
    set true;
    return Slang.Value.Nil
  in
  register_script_function export_as script_function;
  Setting (get, set)


(*
  Creates a Slang function named <export_as> that takes a list of strings as argument.
  Calling this function causes a ref cell to be set to this list of strings.
  Strings are not appended: the list overwrites the previously stored list.
*)
let strings export_as =
  let get, set = create_setting_cell []
  in
  let script_function values =
    let open Slang in
    let open Slang.Prelude.Shared
    in
    let=!! strings = List.map ~f:Multimethods.string values
    in
    set strings;
    return @@ Value.Nil
  in
  register_script_function export_as script_function;
  Setting (get, set)



module Exported = struct
  let get (Setting (getter, _)) =
    getter ()

  let set (Setting (_, setter)) value =
    setter value

  let load_configuration_file path =
    let open Slang
    in
    let contents = Stdio.In_channel.read_all path
    in
    let environment =
      extend_environment prelude @@ fun { callable; _ } -> List.iter ~f:(uncurry callable) !exported_functions
    in
    ignore @@ run_string environment contents
end
