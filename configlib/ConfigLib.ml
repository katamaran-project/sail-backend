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
      (identifier : string)
      (native_function : Slang.Value.native_function) =
  exported_functions := (identifier, native_function) :: !exported_functions

let bool ?(init=false) export_as =
  let cell = ref init
  in
  let get () = !cell
  and set b  = cell := b
  in
  let script_function _values =
    set true;
    return Slang.Value.Nil
  in
  register_script_function export_as script_function;
  Setting (get, set)

let strings export_as =
  let cell = ref []
  in
  let get () = !cell
  and set xs = cell := xs
  in
  let script_function values =
    let open Slang.Types in
    let open Slang.Types.Notations
    in
    let=! strings = map string values
    in
    set strings;
    return Slang.Value.Nil
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
      extend_environment prelude @@ fun { native_function; _ } -> List.iter ~f:(uncurry native_function) !exported_functions
    in
    ignore @@ run_string environment contents
end
