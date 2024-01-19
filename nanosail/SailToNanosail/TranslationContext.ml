open Base
open Ast
open Exception

(* TODO cleanup *)

module Context = struct
  type type_map = (string, Ast.type_definition, String.comparator_witness) Map.t
  
  type t = {
    types : type_map
  }

  let types =
    let get (context : t) : type_map = context.types
    and set (_context : t) (types : type_map) : t = { types }
    in
    (get, set)
end

type error =
  | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option


module Monad = Monads.StateResult.Make (struct type t = Context.t end) (struct type t = error end)

open Monads.Notations.Star(Monad)


type 'a t = 'a Monad.t

type 'a result = 'a Monad.result = Success of 'a | Failure of error (* prevents result from becoming abstract *)

let return = Monad.return

let not_yet_implemented ?(message = "") ocaml_position sail_position =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)

let bind   = Monad.bind

let recover = Monad.recover

let empty_context : Context.t = { types = Map.empty(module String) }

let run f = Monad.run f empty_context

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


module MonadUtil = Monads.Util.Make(Monad)

let map = MonadUtil.map
let fold_left = MonadUtil.fold_left
