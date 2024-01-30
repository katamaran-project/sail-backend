open Base
open Ast
open Exception
open Basics


module Context = struct
  type type_map = (string, Ast.type_definition, String.comparator_witness) Map.t

  type t = {
    types : type_map
  }

  let empty : t = { types = Map.empty(module String) }

  let lookup_type (type_map : type_map) identifier =
    Map.find type_map identifier

  (*
     Accessors
  *)
  let types =
    let get (context : t) : type_map = context.types
    and set (_context : t) (types : type_map) : t = { types }
    in
    (get, set)
end

type error =
  | NotYetImplemented of ocaml_source_location * Libsail.Ast.l * string option
  | AssertionFailure of ocaml_source_location * string

module Monad = Monads.StateResult.Make (struct type t = Context.t end) (struct type t = error end)
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
  let identifier = type_identifier type_definition
  in
  let add_result = Map.add types ~key:identifier ~data:type_definition
  in
  match add_result with
  | `Duplicate -> raise @@ TranslationError (Printf.sprintf "type %s defined multiple times" identifier)
  | `Ok types' -> Monad.put Context.types types'

let register (definition : definition) =
  match definition with
  | TypeDefinition type_definition       -> register_type type_definition
  | TopLevelTypeConstraintDefinition _   -> return ()
  | FunctionDefinition _                 -> return ()
  | RegisterDefinition _                 -> return ()
  | UntranslatedDefinition _             -> return ()
  | IgnoredDefinition                    -> return ()
  | ValueDefinition _                    -> return ()

let lookup_type (identifier : string) : type_definition option t =
  let* types = Monad.get Context.types
  in
  return @@ Context.lookup_type types identifier
