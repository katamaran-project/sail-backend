type raw_token = string


let string_of_chars chars =
  String.of_seq @@ List.to_seq chars

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


let read_boolean (node : char Seq.node) =
  match node with
  | Seq.Nil     -> failwith "no more input"
  | Seq.Cons (char, tail) -> begin
      match char with
      | '#' -> begin
          match tail () with
          | Seq.Nil               -> failwith "unfinished boolean"
          | Seq.Cons (char, tail) -> begin
              match char with
              | 't'  -> (TTrue, tail ())
              | 'f'  -> (TFalse, tail ())
              | _    -> failwith "unrecognized boolean"
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end

let read_string (node : char Seq.node) =
  let rec collect_string_chars acc node =
    match node with
    | Seq.Nil -> failwith "unfinished string"
    | Seq.Cons (char, tail) -> begin
        match char with
        | '"' -> (TString (string_of_chars @@ List.rev acc), tail ())
        | _   -> collect_string_chars (char :: acc) @@ tail ()
      end
  in
  
  match node with
  | Seq.Nil -> failwith "no more input"
  | Seq.Cons (char, tail) -> begin
      match char with
      | '"' -> collect_string_chars [] @@ tail ()
      | _ -> failwith "expected to find a string token"
    end

let read_symbol_or_integer (node : char Seq.node) =
  let rec collect_chars acc node =
    match node with
    | Seq.Nil               -> (string_of_chars @@ List.rev acc, node)
    | Seq.Cons (char, tail) -> begin
        match char with
        | '('
        | ')'
        | ' '
        | '\t'
        | '\n'
        | '\r' -> (string_of_chars @@ List.rev acc, node)
        | _    -> collect_chars (char :: acc) @@ tail ()
      end
  in
  let (chars, tail) = collect_chars [] node
  in
  if String.length chars = 0
  then failwith "invalid input"
  else begin
    match int_of_string_opt chars with
    | Some n -> (TInteger n, tail)
    | None   -> (TSymbol chars, tail)
  end

let rec read_next_token (node : char Seq.node) =
  match node with
  | Seq.Nil               -> None
  | Seq.Cons (char, tail) -> begin
      match char with
      | '('  -> Some (TLeftParenthesis, tail ())
      | ')'  -> Some (TRightParenthesis, tail())
      | '#'  -> Some (read_boolean node)
      | '"'  -> Some (read_string node)
      | ' '
      | '\t'
      | '\n'
      | '\r' -> read_next_token @@ tail ()
      | _    -> Some (read_symbol_or_integer node)
    end

let tokenize (seq : char Seq.t) =
  Seq.unfold read_next_token @@ seq ()
