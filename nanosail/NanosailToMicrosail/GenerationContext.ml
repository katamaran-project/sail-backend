open! ExtBase

module Error = struct
  type t = string
end


type annotation     = Annotation of PP.document
type comment        = Comment of PP.document
type frame          = annotation list * comment list
type state          = frame list * int * Ast.program

module Monad        = Monads.StateResult.Make (struct type t = state end) (Error)
type   'a t         = 'a Monad.t

let return          = Monad.return
let bind            = Monad.bind

let fail
      (ocaml_position : Lexing.position)
      (message        : string         ) : 'a t
  =
  Monad.fail @@ Printf.sprintf "(%s:%d) %s" ocaml_position.pos_fname ocaml_position.pos_lnum message

let recover         = Monad.recover

let get             = Monad.get
let put             = Monad.put
let act             = Monad.act
let update          = Monad.update


let frames      : (state, frame list     ) Monads.Accessors.accessor = Monads.Accessors.(Triple.first id             )
let top_frame   : (state, frame          ) Monads.Accessors.accessor = Monads.Accessors.(List.head @@ Triple.first id)
let annotations : (state, annotation list) Monads.Accessors.accessor = Monads.Accessors.(Pair.first  top_frame       )
let comments    : (state, comment    list) Monads.Accessors.accessor = Monads.Accessors.(Pair.second top_frame       )
let index       : (state, int            ) Monads.Accessors.accessor = Monads.Accessors.(Triple.second id            )
let program     : (state, Ast.program    ) Monads.Accessors.accessor = Monads.Accessors.(Triple.third id             )

exception FrameException of string

open Monads.Notations.Star(Monad)

module MonadUtil = Monads.Util.Make(Monad)
include MonadUtil


let mk_initial_state (program : Ast.program) : state = ([], 0, program)


let log
    (ocaml_position : Lexing.position                         )
    (logger         : Lexing.position -> string lazy_t -> unit)
    (message        : string lazy_t                           ) : unit t
  =
  act (fun () -> logger ocaml_position message)


(* Creates a fresh frame and pushes it onto the frame stack *)
let push_new_frame =
  Monad.update frames (fun frames -> ([], []) :: frames)


(* Pops the last frame from the frame stack *)
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


(* Evaluates f inside a fresh frame *)
let with_fresh_frame f =
  let* ()     = push_new_frame
  and* result = f
  and* frame  = pop_frame
  in
  return (result, frame)


let is_empty_frame frame =
  match frame with
  | ([], []) -> true
  | _        -> false


(* Returns a fresh index *)
let fresh_index =
  let* i  = get index
  in
  let* () = put index @@ i + 1
  in
  return i


(* Checks whether there are any frames on the stack *)
let inside_frame : bool t =
  let* frame_stack = get frames
  in
  return @@ not @@ List.is_empty frame_stack


(* Ensures that there is a frame on the frame stack *)
let assert_inside_frame : unit t =
  let* result = inside_frame
  in
  if not result
  then raise @@ FrameException "should only be executed while within a frame"
  else return ()


(* Ensures that there is no frame on the frame stack *)
let assert_outside_frame : unit t =
  let* result = inside_frame
  in
  if result
  then raise @@ FrameException "should only be executed while there are no frames on the frame stack"
  else return ()


let convert_frame_to_document (frame : frame) =
  let annotations, comments = frame
  in
  let pp_annotations =
    let pp_annotation index (Annotation annotation) =
      PP.(separate_horizontally ~separator:(horizontal [ space; colon; space ]) [
          PP.(surround brackets) @@ string @@ Int.to_string index;
          annotation
        ])
    in
    List.mapi ~f:pp_annotation annotations
  and pp_comments =
    List.map ~f:(fun (Comment c) -> c) comments
  in
  PP.paragraphs @@ List.concat [ pp_comments; pp_annotations ]


(*
   Computes f and gathers all comments/annotations during its execution.
   Produces the result of f and prepends all comments/annotations
*)
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


(* Adds annotation to the current frame *)
let add_annotation (annotation : PP.document) : int t =
  let* () = assert_inside_frame
  and* () = update annotations @@ fun xs -> List.append xs [Annotation annotation]
  in
  fresh_index


(* Adds comment to the current frame *)
let add_comment (comment : PP.document) : unit t =
  let* () = assert_inside_frame
  in
  let comment = Comment comment
  in
  update comments (fun cs -> comment :: cs)


let not_yet_implemented ?(message : string option) (position : Lexing.position) : PP.document t =
  let annotation_document =
    let message_suffix =
      match message with
      | None         -> ""
      | Some message -> Printf.sprintf " (%s)" message
    in
    PP.string @@ Printf.sprintf "Not yet implemented; see %s line %d%s" position.pos_fname position.pos_lnum message_suffix
  in
  let* id = add_annotation annotation_document
  in
  let nyi = PP.string @@ Printf.sprintf "NYI[%d]" id
  in
  return nyi


(*
   If enabled (see configuration), surrounds <contents> by a tag and location. Helpful for debugging.

   For example,

     generation_block [%here] (PP.string "Some label") (PP.string "Contents")

   produces

     (* <<<<< source-path:line-number Some label *)
     Contents
     (* >>>>> source-path:line-number Some label *)
*)
let generation_block
    (position : Lexing.position)
    (label    : string         )
    (contents : PP.document t  ) : PP.document t
  =
  let* () = return () (* forces the logging to happen inside the constructor state monad, instead of before *)
  in
  let* contents =
    let* () = act @@ fun () -> Logging.debug position @@ lazy (Printf.sprintf "Entering %s" label)
    and* restore_indentation = act Logging.create_indentation_restorer
    in
    let* () = act @@ fun () -> Logging.increase_indentation ()
    in
    let* contents
    in
    let* () = act restore_indentation (* fix indentation restoration to handle failures correctly *)
    and* () = act @@ fun () -> Logging.debug position @@ lazy (Printf.sprintf "Exiting %s" label)
    in
    return contents
  in
  let contents = PP.annotate position ~label contents
  in
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
      Coq.pp_inline_comment @@ PP.separate_horizontally ~separator:PP.space [
        PP.string "<<<<<";
        PP.string position_string;
        PP.string label
      ]
    and exit_block =
      Coq.pp_inline_comment @@ PP.separate_horizontally ~separator:PP.space [
        PP.string ">>>>>";
        PP.string position_string;
        PP.string label
      ]
    in
    return @@ PP.vertical [
      entry_block;
      PP.indent contents;
      exit_block;
    ]
  else
    return contents



(* Computes the document described by f *)
let generate
      (program : Ast.program  )
      (f       : PP.document t) : PP.document
  =
  let result, _ =
    (* Add an extra check to f to ensure there are no open frames left *)
    let wrapped_f =
      let* result = f
      and* () = assert_outside_frame
      in
      return result
    in
    Monad.run wrapped_f @@ mk_initial_state program
  in
  match result with
  | Monad.Success result -> result
  | Monad.Failure error  -> failwith @@ "Error occurred during generation: " ^ error


include Monads.Util.Make(Monad)


(* todo probably best moved elsewhere *)
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
    let pp_parameters =
      List.map parameters ~f:(
          fun (identifier, typ) ->
          PP.(surround parens @@ separate_horizontally ~separator:space [ identifier; colon; typ ])
        )
    in
    PP.(
      separate_horizontally ~separator:space (
        List.build_list (fun { add; addall; _ } ->
            add @@ string "Inductive";
            add identifier;
            addall pp_parameters;
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
            separate_horizontally ~separator:space (
              List.build_list (fun { add; _ } ->
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
      Option.value
        ~default:0
        (List.max_elt ~compare:Int.compare @@ List.map ~f:(Fn.compose PP.measure_width fst) pairs)
    in
    let make_line (left, right) =
      PP.(
        indent @@ separate_horizontally ~separator:space (
          List.build_list (fun { add; _ } ->
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
    List.build_list (fun { add; addall; _ } ->
        add first_line;
        addall constructor_lines
      )
  in
  return @@ Coq.pp_sentence @@ PP.vertical result_lines


let add_original_definitions (originals : Sail.sail_definition list) : unit t =
  if
    Configuration.(get include_original_code)
  then
    add_comment begin
      PP.paragraphs [
        PP.string "Original Sail code";
        PP.indent @@ PP.vertical begin
          List.map ~f:PPSail.pp_sail_definition originals
        end
      ]
    end
  else
    return ()


let add_original_definition (original : Sail.sail_definition) : unit t =
  add_original_definitions [ original ]


(*
  Adds a PP-level annotation
*)
let pp_annotate
    (location        : Lexing.position)
    (to_be_annotated : PP.document t  ) : PP.document t
  =
  lift ~f:(PP.annotate location) to_be_annotated


let pp_annotate'
    (location  : Lexing.position)
    (label     : string         )
    (annotated : PP.document t  ) : PP.document t
  =
  lift ~f:(PP.annotate location ~label) annotated


let get_program : Ast.program t =
  get program


let select_definitions (selector : (Sail.sail_definition * Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a list t =
  let* program = get program
  in
  return @@ Ast.Definition.Select.select selector program.definitions


let lookup_definition_opt (selector : (Sail.sail_definition * Ast.Definition.t, 'a) Ast.Definition.Select.selector) : 'a option t =
  let* matches = select_definitions selector
  in
  match matches with
  | [ result ] -> return @@ Some result
  | []         -> return None
  | _          -> fail [%here] "expected exactly one matching definition"
