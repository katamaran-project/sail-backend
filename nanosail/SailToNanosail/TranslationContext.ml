open Base
open Ast
open Exception
    

module Context = struct
  type t = {
    types : (string, Ast.type_definition, String.comparator_witness) Map.t
  }

  let types =
    let get context = context.types
    and set _ types = { types }
    in
    (get, set)
end

module Monad = Monads.ComponentState.Make(struct type t = Context.t end)

open Monads.Notations.Star(Monad)


type 'a t = 'a Monad.t

let return = Monad.return
let bind   = Monad.bind

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
