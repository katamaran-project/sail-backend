open! ExtBase


module Annotation = struct
  type t = string list

  let empty = []

  let is_empty = List.is_empty

  let combine s1 s2 =
    List.concat [s1; s2]

  let to_html strings =
    Html.unordered_list @@ List.map ~f:Html.escape_string @@ List.rev strings
end


module Doc = Document.Make(Annotation)


type document  = Doc.t

let undecorate            = Doc.undecorate
let string_of_document    = Doc.to_string
let html_of_document      = Doc.to_html
let is_empty              = Doc.is_empty
let is_single_line        = Doc.is_single_line                              
let measure_width         = Doc.measure_width

include Doc.Aux


let annotate
    (ocaml_location : Lexing.position)
    ?(label         : string option  )
    (document       : document       ) : document
  =
  let { pos_fname; pos_lnum; _ } : Lexing.position = ocaml_location
  in
  let annotation =
    match label with
    | Some label -> Printf.sprintf "%s:%d %s" pos_fname pos_lnum label
    | None       -> Printf.sprintf "%s:%d" pos_fname pos_lnum
  in
  annotate [annotation] document
