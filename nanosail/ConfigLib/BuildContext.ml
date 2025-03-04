open ExtBase
open Monads.Notations.Star(Slang.EvaluationContext)

module BuildContext = BuildContext
module Setting      = Setting

module EC = Slang.EvaluationContext
module C  = Slang.Converters


module Make (_ : sig end) = struct
  (*
     State of Slang interpreter used to interpret configuration commands
  *)
  let configuration_interpreter_state : EC.state ref =
    let _, state = EC.run Slang.Prelude.initialize
    in
    ref state
  

  (*
     Run f using the configuration interpreter
  *)
  let run (f : 'a EC.t) : 'a =
    let result, new_slang_state = Slang.EvaluationContext.run_with_state f !configuration_interpreter_state
    in
    match result with
    | Success result -> begin
        configuration_interpreter_state := new_slang_state;
        result
      end
    | Failure error -> failwith error


  (*
     Adds an extra binding to the environment of the configuration interpreter.
     Used to make configuration functions available to the user.
  *)
  let export
      (identifier : string       )
      (value      : Slang.Value.t) : unit
    =
    run (EC.add_binding identifier value)

  
  let export_callable
      (identifier : string              )
      (callable   : Slang.Value.callable) : unit
    =
    export identifier (Slang.Value.Callable callable)


  (*
     Reads the entire contents of the given file as a string.
     This function allows files to include other files using the $include directive.
     Note that no recursion check is made, so a file including itself will cause problems.
  *)
  let rec read_file_contents (path : string) : string =
    let directory =
      Filename.dirname path
    in
    let contents =
      Stdio.In_channel.read_all path
    in
    let lines =
      String.split_lines contents
    in
    let preprocessed_lines =
      let preprocess_line (line : string) : string =
        match String.chop_prefix line ~prefix:"$include " with
        | Some included_path -> begin
            let included_path =
              Filename.concat directory included_path
            in
            read_file_contents included_path
          end
        | None -> line
      in
      List.map ~f:preprocess_line lines
    in
    String.concat_lines preprocessed_lines


  let load_configuration (path : string) : unit =
    let contents =
      read_file_contents path
    in
    let program =
      Slang.Evaluation.parse_and_evaluate_string contents
    in
    ignore @@ run program


  let export_strict_function
      (export_as : string                  )
      (process   : Slang.Value.t list -> 'a) : unit
    =
    let script_function arguments =
      let* evaluated_arguments = Slang.Evaluation.evaluate_sequentially arguments
      in
      process evaluated_arguments;
      EC.return Slang.Value.Nil
    in
    export_callable export_as script_function


  let generic_strict
      (export_as : string                  )
      ~(init     : 'a                      )
      (translate : Slang.Value.t list -> 'a) : 'a Setting.t
    =
    let setting = Setting.mk init
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = Slang.Evaluation.evaluate_sequentially arguments
      in
      let strings = translate evaluated_arguments
      in
      Setting.set setting strings;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting

  (*
     Exports a Slang function named <export_as> that takes an optional boolean argument.
     Calling this function causes the setting to be set with this argument.
     If no argument is given, the setting is set to the negation of <default_value>.
     If the function is not called, the setting is set to <default_value>.
  *)
  let bool export_as default_value =
    let setting = Setting.mk default_value
    in
    let script_function arguments =
      match arguments with
      | [] -> begin
          (* No arguments given; set refcell to (not default_value) *)
          Setting.set setting (not default_value);
          EC.return Slang.Value.Nil
        end
      | [argument] -> begin
          (* Single argument given, evaluate it, determine its truthiness, and assign it to the setting *)
          let* evaluated_argument = Slang.Evaluation.evaluate argument
          in
          let truthiness = Slang.Value.truthy evaluated_argument
          in
          Setting.set setting truthiness;
          EC.return Slang.Value.Nil
        end
      | _ -> failwith @@ Printf.sprintf "Function %s expects at most one argument" export_as
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
      let* evaluated_arguments = Slang.Evaluation.evaluate_sequentially arguments
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
      let* evaluated_arguments = Slang.Evaluation.evaluate_sequentially arguments
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


  let callable
      (export_as : string              )
      (default   : Slang.Value.callable) : Slang.Value.callable Setting.t
    =
    let setting = Setting.mk default
    in
    let script_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let* evaluated_arguments = Slang.Evaluation.evaluate_sequentially arguments
      in
      let=! callable = Converters.(map1 callable) evaluated_arguments
      in
      Setting.set setting callable;
      EC.return @@ Value.Nil
    in
    export_callable export_as script_function;
    setting


  (*
     Creates a constant function that takes <arity> arguments and always returns <return_value>.
  *)
  let constant_function
      ~(arity        : int          )
      ~(return_value : Slang.Value.t) : Slang.Value.callable
    =
    let script_function arguments =
      (* strict evaluation demands we evaluate the arguments *)
      let* _ = Slang.Evaluation.evaluate_sequentially arguments
      in
      let argument_count = List.length arguments
      in
      if
        not @@ Int.equal argument_count arity
      then
        failwith @@ Printf.sprintf "function of arity %d received %d arguments" arity argument_count
      else
        EC.return return_value
    in
    script_function
end
