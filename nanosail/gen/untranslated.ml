open Base
open PPrint
open Ast
open Auxlib
open Util

module FunDeclKit = Fundeclkit


let generate untranslated_definitions =
  let pp_sail_location (location : Libsail.Parse_ast.l) =
    match location with
    | Libsail.Parse_ast.Range (start, stop) ->
       if String.equal start.pos_fname stop.pos_fname
       then (
         if start.pos_lnum = stop.pos_lnum
         then
           Printf.sprintf "%s line %d chars %d-%d"
             start.pos_fname
             start.pos_lnum
             (start.pos_cnum - start.pos_bol)
             (stop.pos_cnum - stop.pos_bol)
         else
           Printf.sprintf "%s from line %d:%d to line %d:%d"
             start.pos_fname
             start.pos_lnum
             (start.pos_cnum - start.pos_bol)
             stop.pos_lnum
             (stop.pos_cnum - stop.pos_bol)
       )
       else Sail.string_of_location location
    | _ -> Sail.string_of_location location
  in
  let pp_untranslated_definition (original : sail_definition) (untranslated_definition : untranslated_definition) =
    let { filename; line_number; sail_location; message } = untranslated_definition in
    let ocaml_location_string = Printf.sprintf "OCaml location: %s line %d" filename line_number in
    let sail_location_string = Printf.sprintf "Sail location: %s" (pp_sail_location sail_location) in
    let message_string =
      match message with
      | Some message -> Printf.sprintf "Message: %s" message
      | None         -> Printf.sprintf "No message"
    in
    concat [
        Sail.pp_sail_definition original;
        string ocaml_location_string;
        hardline;
        string sail_location_string;
        hardline;
        string message_string
      ]
  in
  Coq.comment (
      separate small_step (
          List.map ~f:(uncurry pp_untranslated_definition) untranslated_definitions
    ))
