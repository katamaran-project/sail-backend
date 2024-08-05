open Base


module Error = struct
  type t = string
end


type annotation     = Annotation of PP.document
type comment        = Comment of PP.document
type frame          = annotation list * comment list
type state          = frame list * int

module Monad          = Monads.StateResult.Make (struct type t = state end) (Error)
type   'a t           = 'a Monad.t

let return  = Monad.return
let bind    = Monad.bind
let fail    = Monad.fail

let get     = Monad.get
let put     = Monad.put
let update  = Monad.update                

let frames      : (state, frame list     ) Monads.Accessors.accessor = Monads.Accessors.(Pair.first id)
let top_frame   : (state, frame          ) Monads.Accessors.accessor = Monads.Accessors.(List.head @@ Pair.first id)
let annotations : (state, annotation list) Monads.Accessors.accessor = Monads.Accessors.(Pair.first  top_frame)
let comments    : (state, comment    list) Monads.Accessors.accessor = Monads.Accessors.(Pair.second top_frame)
let index       : (state, int            ) Monads.Accessors.accessor = Monads.Accessors.(Pair.second id)


open Monads.Notations.Star(Monad)


let initial_state = ([], 0)


let new_frame =
  Monad.update frames (fun frames -> ([], []) :: frames)


let pop_frame =
  let* frame = get top_frame
  in
  let pop frames =
    match frames with
    | _::xs -> xs
    | []    -> failwith "Bug: pop_frame called on empty frame stack"
  in
  let* () = Monad.update frames pop
  in
  return frame


let with_fresh_frame f =
  let* ()     = new_frame
  and* result = f
  and* frame  = pop_frame
  in
  return (result, frame)


let is_empty_frame frame =
  match frame with
  | ([], []) -> true
  | _        -> false


let next_index =
  let* i  = get index
  in
  let* () = put index @@ i + 1
  in
  return i
    

let convert_frame_to_document (frame : frame) =
  let annotations, comments = frame
  in
  let pp_annotations =
    let pp_annotation index (Annotation annotation) =
      PP.(concat [
          brackets @@ string @@ Int.to_string index;
          space;
          colon;
          space;
          align annotation
        ])
    in
    PP.separate PP.hardline @@ List.mapi ~f:pp_annotation annotations
  and pp_comments =
    PP.(separate_map (twice hardline) (fun (Comment c) -> c) comments)
  in
  PP.(separate (twice hardline) [ pp_comments; pp_annotations ])


let block (f : PP.document t) : PP.document t =
  let* (document, frame) = with_fresh_frame f
  in
  if is_empty_frame frame
  then return document
  else begin
    let comments =
      convert_frame_to_document frame
    in
    return @@ Coq.add_comments ~comments ~document
  end


let add_annotation (document : PP.document) : int t =
  let* () = update annotations @@ fun xs -> (Annotation document) :: xs
  in
  next_index


let add_comment (document : PP.document) : unit t =
  let comment = Comment document
  in
  update comments (fun cs -> comment :: cs)


let not_yet_implemented ?(message = "") (position : Lexing.position) =
  let annotation_document =
    let message_suffix =
      if String.is_empty message
      then ""
      else Printf.sprintf " (%s)" message
    in
    PPrint.string @@ Printf.sprintf "Not yet implemented; see %s line %d%s" position.pos_fname position.pos_lnum message_suffix
  in
  let* id = add_annotation annotation_document
  in
  let nyi = PPrint.string @@ Printf.sprintf "NYI[%d]" id
  in
  return nyi


let generation_block
    (position : Lexing.position)
    (label    : PP.document    )
    (contents : PP.document t  ) : PP.document t
  =
  if
    Configuration.(get show_generation_blocks)
  then
    let* contents = contents
    in
    let position_string =
      let filename    = position.pos_fname
      and line_number = position.pos_lnum
      in
      Printf.sprintf "%s:%d" filename line_number
    in
    let entry_block =
      Coq.pp_inline_comment @@ PP.separate PP.space [
        PP.string "<<<<<";
        PP.string position_string;
        label
      ]
    and exit_block =
      Coq.pp_inline_comment @@ PP.separate PP.space [
        PP.string ">>>>>";
        PP.string position_string;
        label
      ]
    in
    return @@ PP.separate PP.hardline [
      entry_block;
      PP.indent' contents;
      exit_block;
    ]
  else
    contents


let generate (f : PP.document t) : PP.document =
  let result, _ = Monad.run f initial_state
  in
  match result with
  | Monad.Success result -> result
  | Monad.Failure error -> failwith @@ "Error occurred during generation: " ^ error


include Monads.Util.Make(Monad)
