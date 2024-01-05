open Base

type raw_token = string


let is_whitespace char =
  match char with
  | ' ' | '\n' | '\r' | '\t' -> true
  | _                        -> false

type token =
  | TLeftParenthesis
  | TRightParenthesis
  | TSymbol           of string
  | TString           of string
  | TInteger          of int
  | TTrue
  | TFalse


type node = (char * char Sequence.t) option


let read_boolean (node : node) =
  match node with
  | None     -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '#' -> begin
          match Sequence.next tail with
          | None               -> failwith "unfinished boolean"
          | Some (char, tail) -> begin
              match char with
              | 't'  -> (TTrue, Sequence.next tail)
              | 'f'  -> (TFalse, Sequence.next tail)
              | _    -> failwith "unrecognized boolean"
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end

let read_string (node : node) =
  let rec collect_string_chars acc node =
    match node with
    | None -> failwith "unfinished string"
    | Some (char, tail) -> begin
        match char with
        | '"' -> (TString (String.of_char_list @@ List.rev acc), Sequence.next tail)
        | _   -> collect_string_chars (char :: acc) @@ Sequence.next tail
      end
  in
  
  match node with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '"' -> collect_string_chars [] @@ Sequence.next tail
      | _ -> failwith "expected to find a string token"
    end

let read_symbol_or_integer (node : node) =
  let rec collect_chars acc node =
    match node with
    | None               -> (String.of_char_list @@ List.rev acc, node)
    | Some (char, tail) -> begin
        match char with
        | '('
        | ')'
        | ' '
        | '\t'
        | '\n'
        | '\r' -> (String.of_char_list @@ List.rev acc, node)
        | _    -> collect_chars (char :: acc) @@ Sequence.next tail
      end
  in
  let (chars, tail) = collect_chars [] node
  in
  if String.length chars = 0
  then failwith "invalid input"
  else begin
    match Int.of_string_opt chars with
    | Some n -> (TInteger n, tail)
    | None   -> (TSymbol chars, tail)
  end

let rec read_next_token (node : node) =
  match node with
  | None               -> None
  | Some (char, tail) -> begin
      match char with
      | '('  -> Some (TLeftParenthesis, Sequence.next tail)
      | ')'  -> Some (TRightParenthesis, Sequence.next tail)
      | '#'  -> Some (read_boolean node)
      | '"'  -> Some (read_string node)
      | ' '
      | '\t'
      | '\n'
      | '\r' -> read_next_token @@ Sequence.next tail
      | _    -> Some (read_symbol_or_integer node)
    end

let tokenize (seq : char Sequence.t) =
  Sequence.unfold ~f:read_next_token ~init:(Sequence.next seq)

let tokenize_string (string : string) =
  let seq = Sequence.of_list @@ String.to_list string
  in
  tokenize seq
