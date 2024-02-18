open Base
open Slang.Evaluation
open Monads.Notations.Star(Slang.EvaluationContext)

module EC = Slang.EvaluationContext


(* List of native functions to be made accessible in configuration script *)
let exported_functions : (string * Slang.Value.callable) list ref = ref []

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
    EC.return Slang.Value.Nil
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
  let script_function arguments =
    let open Slang in
    let open Slang.Prelude.Shared
    in
    let* evaluated_arguments = EC.map ~f:evaluate arguments
    in
    let=!! strings = List.map ~f:Converters.string evaluated_arguments
    in
    set strings;
    EC.return @@ Value.Nil
  in
  register_script_function export_as script_function;
  Setting (get, set)


let callable ?(error_message = "missing setting") export_as =
  let get, set = create_setting_cell (fun _ -> failwith error_message)
  in
  let script_function arguments =
    let open Slang in
    let open Slang.Prelude.Shared
    in
    let* evaluated_arguments = EC.map ~f:evaluate arguments
    in
    let=! callable = Converters.(map1 callable) evaluated_arguments
    in
    set callable;
    EC.return @@ Value.Nil
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
    let program =
      let* () = Prelude.initialize
      in
      let* () =
        EC.iter !exported_functions ~f:(fun (id, callable) -> begin
          EC.add_binding id (Slang.Value.Callable callable)
        end)
      in
      let* _ = Evaluation.evaluate_string contents
      in
      EC.return ()
    in
    ignore @@ EvaluationContext.run program
end
