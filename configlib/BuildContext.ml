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


  let mk_cell = Setting.create_setting_cell

  
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
        EC.iter !exported_functions ~f:(fun (id, callable) -> begin
          EC.add_binding id (Slang.Value.Callable callable)
        end)
      in
      let* _ = Evaluation.evaluate_string contents
      in
      EC.return ()
    in
    ignore @@ EvaluationContext.run program


  let export_strict_function
      (process   : Slang.Value.t list -> 'a)
      (export_as : string                  )
    =
    let script_function arguments =
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      EC.return @@ process evaluated_arguments
    in
    export_callable export_as script_function
  

  let generic_strict
      ~init
      (translate : Slang.Value.t list -> 'a)
      (export_as : string                  )
    =
    let get, set = mk_cell init
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let strings = translate evaluated_arguments
      in
      set strings;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    Setting.mk get set

    (*
      Exports a Slang function named <export_as> that takes a boolean argument.
      Calling this function causes a refcell to be set with this argument.
    *)
  let bool ?(init=false) export_as =
    let get, set = mk_cell init
    in
    let script_function _values =
      set true;
      EC.return Slang.Value.Nil
    in
    export_callable export_as script_function;
    Setting.mk get set


    (*
      Creates a Slang function named <export_as> that takes a list of strings as argument.
      Calling this function causes a ref cell to be set to this list of strings.
      Strings are not appended: the list overwrites the previously stored list.
    *)
  let strings export_as =
    let get, set = mk_cell []
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = EC.map ~f:evaluate arguments
      in
      let=!! strings = List.map ~f:C.string evaluated_arguments
      in
      set strings;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    Setting.mk get set


    (*
      Creates a Slang function named <export_as> that takes
      two arguments. Every time it is called, it adds this
      pair of values to a map.
     *)
  let string_to_string export_as =
    let get, set = mk_cell []
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
        let map = get ()
        in
        map @ [ (key, data) ]
      in
      set updated_map;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    Setting.mk get set


  let callable ?(error_message = "missing setting") export_as =
    let get, set = mk_cell (fun _ -> failwith error_message)
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
    export_callable export_as script_function;
    Setting.mk get set
end
