open Base


module Annotation = struct
  type t = string

  let empty        = ""
  let is_empty     = String.is_empty 
  let combine _ s2 = s2
  let to_html s    = s
end


module Doc = Document.Make(Annotation)


type document  = Doc.t


let empty      = Doc.empty
let string     = Doc.string
let horizontal = Doc.horizontal
let vertical   = Doc.vertical

let plus       = Doc.plus
let star       = Doc.star
let minus      = Doc.minus
let equals     = Doc.equals

let lparen     = Doc.lparen
let rparen     = Doc.rparen
let langle     = Doc.langle
let rangle     = Doc.rangle
let lbracket   = Doc.lbracket
let rbracket   = Doc.rbracket
let lbrace     = Doc.lbrace
let rbrace     = Doc.rbrace

let squote     = Doc.squote
let dquote     = Doc.dquote

let braces     = Doc.braces
let parens     = Doc.parens
let squotes    = Doc.squotes
let dquotes    = Doc.dquotes
let brackets   = Doc.brackets

let dot        = Doc.dot
let bang       = Doc.bang
let semi       = Doc.semi
let comma      = Doc.comma
let colon      = Doc.colon
let space      = Doc.space

let ampersand  = Doc.ampersand
let percent    = Doc.percent
let bar        = Doc.bar
let underscore = Doc.underscore

let surround  = Doc.surround
let indent    = Doc.indent
                  
let is_empty  = Doc.is_empty


let string_of_document    = Doc.to_string
let html_of_document      = Doc.to_html

let delimited_list        = Doc.delimited_list
let application           = Doc.application
let separate_horizontally = Doc.separate_horizontally
let repeat                = Doc.repeat
let description_list      = Doc.description_list
let is_single_line        = Doc.is_single_line
let hanging               = Doc.hanging
let measure               = Doc.measure
let measure_width         = Doc.measure_width
let measure_height        = Doc.measure_height
let pad_right             = Doc.pad_right
let paragraphs            = Doc.paragraphs
let pp_binary_operation   = Doc.pp_binary_operation
