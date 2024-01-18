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

let empty_context : Context.t = { types = Map.empty(module String) }

let run f = Monad.run f empty_context

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


module MonadUtil = Monads.Util.Make(Monad)

let map = MonadUtil.map
let fold_left = MonadUtil.fold_left
