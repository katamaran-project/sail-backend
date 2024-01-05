open Base


type token =
  | TLeftParenthesis
  | TRightParenthesis
  | TSymbol           of string
  | TString           of string
  | TInteger          of int
  | TTrue
  | TFalse


let read_boolean (seq : char Sequence.t) =
  match Sequence.next seq with
  | None     -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '#' -> begin
          match Sequence.next tail with
          | None               -> failwith "unfinished boolean"
          | Some (char, tail) -> begin
              match char with
              | 't'  -> (TTrue, tail)
              | 'f'  -> (TFalse, tail)
              | _    -> failwith "unrecognized boolean"
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end


let read_string (seq : char Sequence.t) =
  let rec collect_string_chars acc seq =
    match Sequence.next seq with
    | None -> failwith "unfinished string"
    | Some (char, tail) -> begin
        match char with
        | '"' -> (TString (String.of_char_list @@ List.rev acc), tail)
        | _   -> collect_string_chars (char :: acc) @@ tail
      end
  in
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '"' -> collect_string_chars [] @@ tail
      | _ -> failwith "expected to find a string token"
    end


let read_symbol_or_integer (seq : char Sequence.t) =
  let rec collect_chars acc seq =
    match Sequence.next seq with
    | None               -> (String.of_char_list @@ List.rev acc, seq)
    | Some (char, tail) -> begin
        match char with
        | '('
        | ')'
        | ' '
        | '\t'
        | '\n'
        | '\r' -> (String.of_char_list @@ List.rev acc, seq)
        | _    -> collect_chars (char :: acc) tail
      end
  in
  let (chars, tail) = collect_chars [] seq
  in
  if String.is_empty chars
  then failwith "invalid input"
  else begin
    match Int.of_string_opt chars with
    | Some n -> (TInteger n, tail)
    | None   -> (TSymbol chars, tail)
  end


let rec read_next_token (seq : char Sequence.t) =
  match Sequence.next seq with
  | None               -> None
  | Some (char, tail) -> begin
      match char with
      | '('  -> Some (TLeftParenthesis, tail)
      | ')'  -> Some (TRightParenthesis, tail)
      | '#'  -> Some (read_boolean seq)
      | '"'  -> Some (read_string seq)
      | ' '
      | '\t'
      | '\n'
      | '\r' -> read_next_token tail
      | _    -> Some (read_symbol_or_integer seq)
    end


let tokenize (seq : char Sequence.t) =
  Sequence.unfold ~f:read_next_token ~init:seq


let tokenize_string (string : string) =
  let seq = Sequence.of_list @@ String.to_list string
  in
  tokenize seq
