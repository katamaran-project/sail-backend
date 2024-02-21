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
      | _      -> default_translation function_identifier arguments
    end
  | _ -> default_translation function_identifier arguments
