open Base
open Slang.Evaluation
open Monads.Notations.Star(Slang.EvaluationContext)

module BuildContext = BuildContext
module Setting      = Setting

module EC = Slang.EvaluationContext
module C  = Slang.Converters



module M (_ : sig end) = struct
  let exported_functions : (string * Slang.Value.callable) list ref =
    ref []


  let export_callable
      (identifier : string              )
      (callable   : Slang.Value.callable)
    =
    exported_functions := (identifier, callable) :: !exported_functions


  let load_configuration path =
    let open Slang
    in
    let contents = Stdio.In_channel.read_all path
    in
    let program =
      let* () = Prelude.initialize
      in
      let* () =
        EC.iter !exported_functions ~f:(fun (id, callable) ->
            EC.add_binding id @@ Slang.Value.Mk.callable callable
          )
      in
      let* _ = Evaluation.evaluate_string contents
      in
      EC.return ()
    in
    ignore @@ EvaluationContext.run program


  let export_strict_function
      (export_as : string                  )
      (process   : Slang.Value.t list -> 'a) : unit
    =
    let script_function arguments =
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      process evaluated_arguments;
      EC.return Slang.Value.Nil
    in
    export_callable export_as script_function


  let generic_strict
      (export_as : string                  )
      ~init
      (translate : Slang.Value.t list -> 'a)
    =
    let setting = Setting.mk init
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let strings = translate evaluated_arguments
      in
      Setting.set setting strings;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting

  (*
    Exports a Slang function named <export_as> that takes a boolean argument.
    Calling this function causes a refcell to be set with this argument.
   *)
  let bool export_as init =
    let setting = Setting.mk init
    in
    let script_function _values =
      Setting.set setting true;
      EC.return Slang.Value.Nil
    in
    export_callable export_as script_function;
    setting


  (*
    Exports a Slang function named <export_as> that takes a integer argument.
    Calling this function causes a refcell to be set with this argument.
   *)
  let integer export_as init =
    let setting = Setting.mk init
    in
    let script_function evaluated_arguments =
      let open Slang.Prelude.Shared
      in
      let=! value = C.(map1 integer) evaluated_arguments
      in
      Setting.set setting value
    in
    export_strict_function export_as script_function;
    setting


  (*
    Exports a Slang function named <export_as> that takes a integer argument.
    Calling this function causes a refcell to be set with this argument.
   *)
  let string export_as init =
    let setting = Setting.mk init
    in
    let script_function evaluated_arguments =
      let open Slang.Prelude.Shared
      in
      let=! value = C.(map1 string) evaluated_arguments
      in
      Setting.set setting value
    in
    export_strict_function export_as script_function;
    setting


  (*
    Creates a Slang function named <export_as> that takes a list of strings as argument.
    Calling this function causes a ref cell to be set to this list of strings.
    Strings are not appended: the list overwrites the previously stored list.
   *)
  let strings export_as =
    let setting = Setting.mk []
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let=!! strings = List.map ~f:C.string evaluated_arguments
      in
      Setting.set setting strings;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting


  (*
    Creates a Slang function named <export_as> that takes
    two arguments. Every time it is called, it adds this
    pair of values to a map.
   *)
  let string_to_string export_as =
    let setting = Setting.mk []
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let=! pair = C.map2 C.string C.string evaluated_arguments
      in
      let updated_map =
        let key, data = pair
        in
        let map = Setting.get setting
        in
        map @ [ (key, data) ]
      in
      Setting.set setting updated_map;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting


  let callable ?(error_message = "missing setting") export_as =
    let setting = Setting.mk (fun _ -> failwith error_message)
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let=! callable = Converters.(map1 callable) evaluated_arguments
      in
      Setting.set setting callable;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting


  let callable'
      (export_as : string              )
      (default   : Slang.Value.callable)
    =
    let setting = Setting.mk default
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let=! callable = Converters.(map1 callable) evaluated_arguments
      in
      Setting.set setting callable;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting


  let constant_function
      ~(arity        : int          )
      ~(return_value : Slang.Value.t) : Slang.Value.callable
    =
    let script_function arguments =
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      if
        not @@ Int.equal (List.length evaluated_arguments) arity
      then
        failwith "Invalid arity" (* todo better error reporting *)
      else
        EC.return return_value
    in
    script_function
end
