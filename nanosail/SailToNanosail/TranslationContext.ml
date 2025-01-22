open Base


module Context = struct
  type t =
    {
      definitions   : Ast.Definition.t list;  (* list of definitions                          *)
      next_id_index : int;                    (* counter used to generate unique identifiers  *)
    }

  let empty : t =
    {
      definitions = [];
      next_id_index = 0;
    }

  (*
     Accessors
  *)
  let definitions =
    let get (context : t) : Ast.Definition.t list =
      context.definitions
    and set
        (context     : t                    )
        (definitions : Ast.Definition.t list) : t
      =
      { context with definitions }
    in
    (get, set)

  let next_id_index =
    let get (context : t) : int =
      context.next_id_index
    and set
        (context       : t  )
        (next_id_index : int) : t
      =
      { context with next_id_index }
    in
    (get, set)
end


module Error = struct
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | AssertionFailure  of Lexing.position * string

  let to_string (error : t) =
    match error with
    | NotYetImplemented (ocaml_location, _sail_location, message) -> begin
        match message with
        | None     -> Printf.sprintf "not yet implemented (%s)" (StringOf.OCaml.position ocaml_location)
        | Some msg -> Printf.sprintf "not yet implemented (%s): %s" (StringOf.OCaml.position ocaml_location) msg
      end
    | AssertionFailure (ocaml_location, message) -> begin
        Printf.sprintf "assertion failure (%s): %s" (StringOf.OCaml.position ocaml_location) message
      end
end


module Monad     = Monads.StateResult.Make(Context)(Error)
module MonadUtil = Monads.Util.Make(Monad)

include MonadUtil
open Monads.Notations.Star(Monad)


type 'a t      = 'a Monad.t
type 'a result = 'a Monad.result = Success of 'a      (* explicitly enumerating cases here prevents result from becoming abstract *)
                                 | Failure of Error.t

let return     = Monad.return
let act        = Monad.act
let error      = Monad.fail
let bind       = Monad.bind
let recover    = Monad.recover
let run f      = Monad.run f Context.empty


let log
    (ocaml_position : Lexing.position                         )
    (logger         : Lexing.position -> string lazy_t -> unit)
    (message        : string lazy_t                           ) : unit t
  =
  act (fun () -> logger ocaml_position message)


let with_excursion (block : 'a t) : 'a t =
  let* restore_indentation = act @@ Logging.create_indentation_restorer
  in
  let block' =
    let* block
    and* () = act restore_indentation
    in
    return block
  in
  recover block' (fun error -> let* () = act restore_indentation in Monad.fail error)


let translation_block
    (ocaml_position : Lexing.position)
    (label          : string         )
    (result         : 'a t           ) : 'a t
  =
  let* () = act @@ fun () -> Logging.debug ocaml_position @@ lazy (Printf.sprintf "Entering %s" @@ label)
  in
  let* result = with_excursion result
  in
  let* () = act @@ fun () -> Logging.debug ocaml_position @@ lazy (Printf.sprintf "Exiting %s" label)
  in
  return result


let string_of_error (error : Error.t) : string =
  let string_of_ocaml_location (ocaml_location : Lexing.position) =
    Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
  in
  match error with
  | NotYetImplemented (ocaml_position, sail_location, message) -> begin
      let message =
        match message with
        | Some message -> message
        | None         -> "<no message>"
      in
      Printf.sprintf "NotYetImplemented(%s, ocaml:%s, sail:%s)" message (StringOf.OCaml.position ocaml_position) (StringOf.Sail.location sail_location)
    end
  | AssertionFailure (ocaml_location, message) -> Printf.sprintf "AssertionFailure(%s at %s)" message (string_of_ocaml_location ocaml_location)


(*
  If an error occurs while executing f, print it out and 'reraise' it.
  In other words, this function has no impact on the execution,
  except that it might print out an error.
*)
let debug_error f =
  let show e =
    Stdio.printf "%s\n" (string_of_error e);
    error e
  in
  recover f show


let not_yet_implemented ?(message : string option) ocaml_position sail_position =
  let* () = log [%here] Logging.debug @@ lazy "Not yet implemented"
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let check ocaml_position condition message =
  if not condition
  then Monad.fail @@ AssertionFailure (ocaml_position, Lazy.force message)
  else Monad.return ()


let fail ocaml_position message =
  Monad.fail @@ AssertionFailure (ocaml_position, message)


let definitions : Ast.Definition.t list t =
  Monad.get Context.definitions


let store_definition (definition : Ast.Definition.t) =
  let* old_definitions = definitions
  in
  let new_definitions = definition :: old_definitions
  in
  Monad.put Context.definitions new_definitions


(*
   Returns all definitions satisfying the selector
*)
let select_definitions (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a list t =
  let* definitions
  in
  return @@ Ast.Definition.Select.(select selector definitions)


(*
   Returns definition satisfying selector. Causes failure if there are none or multiple.
*)
let lookup_definition (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a t =
  let* definitions = select_definitions selector
  in
  match definitions with
  | [ definition ] -> return definition
  | []             -> fail [%here] @@ Printf.sprintf "no definition found matching the selector %s" selector#to_string
  | _              -> fail [%here] @@ Printf.sprintf "expected only one match for selector %s" selector#to_string


(*
   Returns definition satisfying selector. Causes failure if there are multiple matches.
*)
let lookup_definition_opt (selector : (Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a option t =
  let* definitions = select_definitions selector
  in
  match definitions with
  | [ definition ] -> return @@ Some definition
  | []             -> return @@ None
  | _              -> fail [%here] "expected only one match"


(*
   Looks up the type of a register with the given name.
*)
let lookup_register_type (identifier : Ast.Identifier.t) : Ast.Type.t option t =
  let* register_definition =
    lookup_definition_opt @@ Ast.Definition.Select.register_definition ~named:identifier ()
  in
  return begin
    Option.map
      register_definition
      ~f:(fun register_definition -> register_definition.typ)
  end


(*
   Checks if there exists a register with the given name.
*)
let is_register (identifier : Ast.Identifier.t) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


(*
  Looks up a variant that has a given constructor.
*)
let lookup_variant_by_constructor (constructor_identifier : Ast.Identifier.t) : Ast.Definition.Type.Variant.t option t =
  let has_constructor (variant_definition : Ast.Definition.Type.Variant.t) : bool =
    List.exists variant_definition.constructors ~f:(fun (id, _) -> Ast.Identifier.equal id constructor_identifier)
  in
  let* variant_definitions =
    select_definitions Ast.Definition.Select.(type_definition @@ of_variant ()) 
  in
  return @@ List.find variant_definitions ~f:has_constructor


let generate_unique_int : int t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  return index


let generate_unique_identifier
    ?(prefix     : string = ""   )
    ?(underscore : bool   = false)
    (_           : unit          ) : Ast.Identifier.t t
  =
  let* index = generate_unique_int
  in
  let result = Configuration.tag_as_generated @@ Ast.Identifier.mk @@ Printf.sprintf "%s%d" prefix index
  in
  if underscore
  then return @@ Ast.Identifier.add_prefix "_" result
  else return result


let rec generate_unique_identifiers
    ?(prefix     : string = "" )
    ?(underscore : bool = false)
    (count       : int         ) : Ast.Identifier.t list t
  =
  if
    Int.equal 0 count
  then
    return []
  else
    let* id  = generate_unique_identifier ~prefix ~underscore ()
    and* ids = generate_unique_identifiers ~prefix ~underscore (count - 1)
    in
    return @@ id :: ids


let rec try_multiple (fs : 'a t list) : 'a t =
  match fs with
  | f::fs -> recover f (fun _error -> try_multiple fs)
  | []    -> fail [%here] "ran out of alternatives"
