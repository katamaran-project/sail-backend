open Base
open Ast
open Exception
    

module Context = struct
  type t = {
    types : (string, Ast.type_definition, String.comparator_witness) Map.t
  }
  [@@deriving accessors]
end

module Monad = Monads.ComponentState.Make(struct type t = Context.t end)

open Monads.Notations.Star(Monad)


type 'a t = 'a Monad.t

let return = Monad.return
let bind   = Monad.bind
let run    = Monad.run

let get accessor =
  Monad.get @@ Accessor.get accessor

let put accessor =
  Monad.put (fun state x -> Accessor.set accessor state ~to_:x)

let register_type (type_definition : type_definition) =
  let* types = get Context.types
  in
  let identifier = type_identifier type_definition
  in
  let add_result = Map.add types ~key:identifier ~data:type_definition
  in
  match add_result with
  | `Duplicate -> raise @@ TranslationError (Printf.sprintf "type %s defined multiple times" identifier)
  | `Ok types' -> put Context.types types'
