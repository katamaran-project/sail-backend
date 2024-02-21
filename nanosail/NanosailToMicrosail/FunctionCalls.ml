open Base
open PP
open Auxlib
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let default_translation
    (function_identifier : string       )
    (arguments           : document list) : document AC.t
  =
  let terms =
    build_list @@ fun { add; addall; _ } -> begin
      add @@ string "call";
      add @@ string function_identifier;
      addall @@ arguments
    end
  in
  AC.return @@ simple_app terms


let translate
    (function_identifier : string       )
    (arguments           : document list) : document AC.t
  =
  match function_identifier with
  | "add_bits_int" -> begin
      match arguments with
      | [x; y] -> AC.return @@ PP.parens (x ^^ string "+" ^^ y)
      | _      -> begin
          let message =
            Printf.sprintf "%s should receive 2 arguments but instead received %d; falling back on default translation for function calls" function_identifier (List.length arguments)
          in
          let* annotation_index = AC.create_annotation @@ string message
          and* translation      = default_translation function_identifier arguments
          in
          AC.return @@ PP.separate space [ translation; Coq.inline_comment (string @@ Int.to_string annotation_index) ]
        end
    end
  | _ -> default_translation function_identifier arguments
