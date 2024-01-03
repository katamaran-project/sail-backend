module Continuation = Continuation

type token =
  | TLeftParenthesis
  | TRightParenthesis
  | TSymbol           of string
  | TString           of string
  | TTrue
  | TFalse

type ast =
  | Cons    of ast * ast
  | Integer of int
  | Symbol  of string
  | String  of string
  | Bool    of bool
  | Nil


let tokenize (seq : char Seq.t) : token Seq.t =
  let tokenize_string node =
    let rec collect_chars acc node =
      match node with
      | Seq.Nil               -> failwith "Unfinished string"
      | Seq.Cons ('"', rest)  -> (TString (String.of_seq @@ List.to_seq @@ List.rev acc), rest ())
      | Seq.Cons (ch , rest)  -> collect_chars (ch :: acc) (rest ())
    in
    collect_chars [] node
  in

  let tokenize_boolean node =
    match node with
    | Seq.Nil              -> failwith "Unfinished boolean"
    | Seq.Cons ('t', rest) -> (TTrue, rest ())
    | Seq.Cons ('f', rest) -> (TFalse, rest ())
    | Seq.Cons (_  , _   ) -> failwith "Unrecognized #-token"
  in

  let rec aux (node : char Seq.node) =
    match node with
    | Seq.Nil               -> None
    | Seq.Cons (char, rest) -> begin
        match char with
        | '('           -> Some (TLeftParenthesis, rest ())
        | ')'           -> Some (TRightParenthesis, rest ())
        | '"'           -> Some (tokenize_string (rest ()))
        | '#'           -> Some (tokenize_boolean (rest ()))
        | ' '           -> aux @@ rest ()
        | _             -> failwith "Unrecognized input"
      end
  in
  Seq.unfold aux (seq ())
