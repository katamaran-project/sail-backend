open Base


type   annotation     = Annotation of PPrint.document
type   annotations    = annotation list
type   state          = { annotations : annotations }

module Monad          = Monads.State.Make(struct type t = state end)
type   'a t           = 'a Monad.t

let    return         = Monad.return
let    bind           = Monad.bind
let    initial_state  = { annotations = [] }


let create_annotation_from_document (annotation_document : PP.document) : int t =
  let open Monads.Notations.Star(Monad)
  in
  let  annotation       = Annotation annotation_document  in
  let* state            = Monad.get                       in
  let  annotation_index = List.length state.annotations   in
  let  annotations'     = annotation :: state.annotations in
  let  state'           = { annotations = annotations' }  in
  let* ()               = Monad.put state'
  in
  return annotation_index


let create_annotation_from_string (annotation_string : string) : int t =
  create_annotation_from_document (PP.string annotation_string)


let not_yet_implemented ?(message = "") (position : Lexing.position) =
  let open Monads.Notations.Star(Monad)
  in
  let annotation_doc =
    let message_suffix =
      if String.is_empty message
      then ""
      else Printf.sprintf " (%s)" message
    in
    PPrint.string @@ Printf.sprintf "Not yet implemented; see %s line %d%s" position.pos_fname position.pos_lnum message_suffix
  in
  let* id = create_annotation_from_document annotation_doc
  in
  return @@ PPrint.string @@ Printf.sprintf "NYI[%d]" id


let collect_annotations (f : 'a t) : 'a * annotation list =
  let result, state = Monad.run f initial_state
  in
  (result, List.rev state.annotations)


let drop_annotations (f : 'a t) : 'a =
  fst @@ collect_annotations f


include Monads.Util.Make(Monad)


let document_of_annotation (Annotation d) = d
