open Base
open Ast
open Exception
open Basics
open Monads.OptionNotation


module Context = struct
  type type_map = Ast.type_definition StringMap.t

  type register_map = nanotype StringMap.t

  type t =
    {
      types         : type_map;     (* maps identifiers to type definitions         *)
      next_id_index : int;          (* counter used to generate unique identifiers  *)
      registers     : register_map; (* list of registers                            *)
    }

  let empty : t =
    {
      types         = StringMap.empty;
      next_id_index = 0;
      registers     = StringMap.empty;
    }

  let lookup_type (type_map : type_map) identifier =
    StringMap.find type_map identifier

  (*
     Accessors
  *)
  let types =
    let get (context : t) : type_map = context.types
    and set (context : t) (types : type_map) : t = { context with types }
    in
    (get, set)

  let next_id_index =
    let get (context : t) : int = context.next_id_index
    and set (context : t) (next_id_index : int) : t = { context with next_id_index }
    in
    (get, set)

  let registers =
    let get (context : t) : register_map = context.registers
    and set (context : t) (registers : register_map) : t = { context with registers }
    in
    (get, set)
end


type error =
  | NotYetImplemented of ocaml_source_location * Libsail.Ast.l * string option
  | AssertionFailure  of ocaml_source_location * string


module Monad     = Monads.StateResult.Make (struct type t = Context.t end) (struct type t = error end)
module MonadUtil = Monads.Util.Make(Monad)

include MonadUtil
open Monads.Notations.Star(Monad)


type 'a t      = 'a Monad.t
type 'a result = 'a Monad.result = Success of 'a     (* explicitly enumerating cases here prevents result from becoming abstract *)
                                 | Failure of error

let return     = Monad.return
let bind       = Monad.bind
let recover    = Monad.recover
let run f      = Monad.run f Context.empty


let not_yet_implemented ?(message = "") ocaml_position sail_position =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let check ocaml_position condition message =
  if not condition
  then Monad.fail @@ AssertionFailure (ocaml_position, Lazy.force message)
  else Monad.return ()


let fail ocaml_position message =
  Monad.fail @@ AssertionFailure (ocaml_position, message)


let register_type (type_definition : type_definition) =
  let* types = Monad.get Context.types
  in
  let updated_types =
    let key = type_identifier type_definition
    and data = type_definition
    in
    match StringMap.add types ~key ~data with
    | `Duplicate -> raise @@ TranslationError (Printf.sprintf "type %s defined multiple times" key)
    | `Ok result -> result
  in
  Monad.put Context.types updated_types


let register_register (register_definition : register_definition) =
  let* register_map = Monad.get Context.registers
  in
  let updated_register_map =
    let key = register_definition.identifier
    and data = register_definition.typ
    in
    match StringMap.add register_map ~key ~data with
    | `Duplicate -> raise @@ TranslationError (Printf.sprintf "register %s defined multiple times" key)
    | `Ok result -> result
  in
  Monad.put Context.registers updated_register_map


let register (definition : definition) =
  match definition with
  | TypeDefinition type_definition         -> register_type type_definition
  | RegisterDefinition register_definition -> register_register register_definition
  | TopLevelTypeConstraintDefinition _     -> return ()
  | FunctionDefinition _                   -> return ()
  | UntranslatedDefinition _               -> return ()
  | IgnoredDefinition                      -> return ()
  | ValueDefinition _                      -> return ()


(*
   Looks up a type definition based on the name of the type.

   The extractor (see Ast.Extract) can be used to get a specific kind of type
*)
let lookup_type
      (extractor  : type_definition -> 'a option)
      (identifier : string                      ) : 'a option t =
  let* types = Monad.get Context.types
  in
  return @@ begin
    let=? type_definition = Context.lookup_type types identifier
    in
    extractor type_definition
  end


let lookup_register_type (identifier : string) : nanotype option t =
  let* registers = Monad.get Context.registers
  in
  return @@ StringMap.find registers identifier


let is_register (identifier : string) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


let generate_unique_identifier prefix : string t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  let id = Printf.sprintf "%s%d" prefix index
  in
  return id
