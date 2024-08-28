open Base


module Error = struct
  type t = string
end


type annotation     = Annotation of PP.document
type comment        = Comment of PP.document
type frame          = annotation list * comment list
type state          = frame list * int

module Monad        = Monads.StateResult.Make (struct type t = state end) (Error)
type   'a t         = 'a Monad.t

let return          = Monad.return
let bind            = Monad.bind
let fail            = Monad.fail

let get             = Monad.get
let put             = Monad.put
let update          = Monad.update


let frames      : (state, frame list     ) Monads.Accessors.accessor = Monads.Accessors.(Pair.first id             )
let top_frame   : (state, frame          ) Monads.Accessors.accessor = Monads.Accessors.(List.head @@ Pair.first id)
let annotations : (state, annotation list) Monads.Accessors.accessor = Monads.Accessors.(Pair.first  top_frame     )
let comments    : (state, comment    list) Monads.Accessors.accessor = Monads.Accessors.(Pair.second top_frame     )
let index       : (state, int            ) Monads.Accessors.accessor = Monads.Accessors.(Pair.second id            )


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
      PP.(horizontal ~separator:(space ^^ colon ^^ space) [
          brackets @@ string @@ Int.to_string index;
          align annotation
        ])
    in
    List.mapi ~f:pp_annotation annotations
  and pp_comments =
    List.map ~f:(fun (Comment c) -> c) comments
  in
  PP.build_vertical @@ fun { addall; _ } -> begin
    addall pp_comments;
    addall pp_annotations
  end


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


let add_annotation (annotation : PP.document) : int t =
  let* () = update annotations @@ fun xs -> (Annotation annotation) :: xs
  in
  next_index


let add_comment (comment : PP.document) : unit t =
  let comment = Comment comment
  in
  update comments (fun cs -> comment :: cs)


let not_yet_implemented ?(message = "") (position : Lexing.position) : PP.document t =
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
    (contents : PP.document    ) : PP.document t
  =
  if
    Configuration.(get show_generation_blocks)
  then
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
      PP.indent contents;
      exit_block;
    ]
  else
    return contents


let generate (f : PP.document t) : PP.document =
  let result, _ = Monad.run f initial_state
  in
  match result with
  | Monad.Success result -> result
  | Monad.Failure error  -> failwith @@ "Error occurred during generation: " ^ error


include Monads.Util.Make(Monad)


let pp_inductive_type
     (identifier : PP.document                          )
    ?(parameters : (PP.document * PP.document) list = [])
     (typ        : PP.document                          )
      constructor_generator                               : PP.document t
  =
  let* constructors =
    let result = ref []
    in
    let generate_case
          ?(parameters  : PP.document = PP.empty)
          ?(typ         : PP.document = PP.empty)
           (identifier  : PP.document           ) =
      result := (identifier, parameters, typ) :: !result;
      return ()
    in
    let* _ = constructor_generator generate_case in
    return @@ List.rev !result
  in
  let first_line =
    let parameters' =
      List.map parameters ~f:(
          fun (identifier, typ) ->
            PP.(parens @@ separate space [ identifier; colon; typ ])
        )
    in
    PP.(
      separate space (
        Auxlib.build_list (fun { add; addall; _ } ->
            add @@ string "Inductive";
            add identifier;
            addall parameters';
            if not @@ is_empty typ
            then
              (
                add colon;
                add typ
              );
            add @@ string ":="
          )
      )
    )
  in
  let constructor_lines =
    let pairs =
      List.map constructors ~f:(fun (id, params, typ) ->
          PP.(
            separate space (
              Auxlib.build_list (fun { add; _ } ->
                  add id;
                  if not @@ is_empty params
                  then add params
                )
            ),
            typ
          )
        )
    in
    let longest_left_part =
      if List.is_empty pairs
      then 0
      else
        Auxlib.maximum (
            List.map ~f:(fun (left, _) -> PP.measure left) pairs
          )
    in
    let make_line (left, right) =
      PP.(
        (twice space) ^^ separate space (
          Auxlib.build_list (fun { add; _ } ->
              add @@ string "|";
              add @@ pad_right longest_left_part left;
              if not @@ is_empty right
              then (
                add colon;
                add right
              )
            )
        )
      )
    in
    List.map ~f:make_line pairs
  in
  let result_lines =
    Auxlib.build_list (fun { add; addall; _ } ->
        add first_line;
        addall constructor_lines
      )
  in
  return @@ Coq.pp_sentence @@ PP.(separate hardline result_lines ^^ hardline)


(* todo move this to PPSail.ml *)
let pp_sail_definition sail_definition =
  let document =
    Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  let str =
    String.rstrip @@ PP.string_of_document ~page_width:200 document
  in
  let lines =
    List.map ~f:String.rstrip @@ String.split_lines str
  in
  PP.vertical_strings lines


let add_original_definitions (originals : Libsail.Type_check.tannot Libsail.Ast.def list) : unit t =
  if
    Configuration.(get include_original_code)
  then
    add_comment begin
      PP.vertical ~separator:PP.(twice hardline) [
        PP.string "Original Sail code";
        PP.indent @@ PP.vertical begin
          List.map ~f:pp_sail_definition originals
        end
      ]
    end
  else
    return ()


let add_original_definition (original : Libsail.Type_check.tannot Libsail.Ast.def) : unit t =
  add_original_definitions [ original ]
