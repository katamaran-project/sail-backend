(*

   CoqGenerationContext is a state monad that extends GenerationContext
   with Coq-specific utility functions.
   
*)

open Base

include GenerationContext

open Monads.Notations.Star(Monad)


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
            if requirement typ > 0
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
                  if requirement params > 0
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
            List.map ~f:(fun (left, _) -> PP.requirement left) pairs
          )
    in
    let make_line (left, right) =
      PP.(
        (twice space) ^^ separate space (
          Auxlib.build_list (fun { add; _ } ->
              add @@ string "|";
              add @@ pad_right longest_left_part left;
              if requirement right > 0
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
  return @@ PP.(separate hardline result_lines ^^ hardline ^^ Coq.eol)


let vertical ?(spacing = 1) documents =
  let* documents =
    sequence documents
  in
  return @@ PP.separate (PP.repeat spacing PP.hardline) documents


let vertical_strings ?(spacing = 1) strings =
  vertical ~spacing @@ List.map ~f:(fun s -> return @@ PP.utf8string s) strings
