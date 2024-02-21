open Base
open Basics


type   annotation     = PPrint.document
type   annotations    = annotation list
type   state          = { annotations : annotations }

module Monad          = Monads.State.Make(struct type t = state end)
type   'a t           = 'a Monad.t

let    return         = Monad.return
let    bind           = Monad.bind
let    initial_state  = { annotations = [] }


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


let not_yet_implemented ?(message = "") (position : ocaml_source_location) =
  let open Monads.Notations.Star(Monad)
  in
  let annotation_doc =
    let message_suffix =
      if String.is_empty message
      then ""
      else Printf.sprintf " (%s)" message
    in
    PPrint.string (Printf.sprintf "%s line %d%s" position.pos_fname position.pos_lnum message_suffix)
  in
  let* id = create_annotation annotation_doc
  in
  return @@ PPrint.string (Printf.sprintf "NYI[%d]" id)

let collect_annotations f =
  let result, state = Monad.run f initial_state
  in
  (result, List.rev state.annotations)


include Monads.Util.Make(Monad)
