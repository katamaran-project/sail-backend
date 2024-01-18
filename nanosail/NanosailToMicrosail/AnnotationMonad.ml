open Base


type annotation = PPrint.document

type annotations = annotation list

type state =
  {
    annotations : annotations
  }

module Monad = Monads.State.Make(struct type t = state end)

type 'a t = 'a Monad.t

let return = Monad.return

let bind = Monad.bind

let initial_state = { annotations = [] }

let create_annotation annotation =
  let open Monads.Notations.Star(Monad)
  in
  let* state = Monad.get
  in
  let annotations' = annotation :: state.annotations
  in
  let state' = { annotations = annotations' }
  in
  let* () = Monad.put state'
  in
  Monad.return @@ List.length annotations'

let not_yet_implemented (position : Lexing.position) =
  let open Monads.Notations.Star(Monad)
  in
  let annotation_doc =
    PPrint.string (Printf.sprintf "%s line %d" position.pos_fname position.pos_lnum)
  in
  let* id = create_annotation annotation_doc
  in
  return @@ PPrint.string (Printf.sprintf "NYI[%d]" id)

let collect_annotations f =
  let result, state = Monad.run f initial_state
  in
  (result, List.rev state.annotations)

include Monads.Util.Make(Monad)
