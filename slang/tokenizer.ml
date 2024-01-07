open Base
open Sequence.Generator

type token =
  | TLeftParenthesis
  | TRightParenthesis
  | TQuote
  | TSymbol           of string
  | TString           of string
  | TInteger          of int
  | TTrue
  | TFalse


let is_newline = Char.equal '\n'


let rec read_next_token (seq : char Sequence.t) =
  match Sequence.next seq with
  | None              -> return ()
  | Some (char, tail) -> begin
      let continue () = read_next_token tail
      in
      match char with
      | '('  -> yield TLeftParenthesis >>= continue
      | ')'  -> yield TRightParenthesis >>= continue
      | '#'  -> read_boolean seq
      | '"'  -> read_string seq
      | ';'  -> read_comment seq
      | '\'' -> yield TQuote
      | _    -> begin
          if Char.is_whitespace char
          then continue ()
          else read_symbol_or_integer seq
        end
    end

and read_boolean (seq : char Sequence.t) =
  match Sequence.next seq with
  | None     -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '#' -> begin
          match Sequence.next tail with
          | None               -> failwith "unfinished boolean"
          | Some (char, tail) -> begin
              let continue () =
                read_next_token tail
              in
              match char with
              | 't'  -> yield TTrue >>= continue
              | 'f'  -> yield TFalse >>= continue
              | _    -> failwith "unrecognized boolean"
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end

and read_string (seq : char Sequence.t) =
  let rec collect_string_chars acc seq =
    match Sequence.next seq with
    | None -> failwith "unfinished string"
    | Some (char, tail) -> begin
        let continue () =
          read_next_token tail
        in
        match char with
        | '"' -> yield (TString (String.of_char_list @@ List.rev acc)) >>= continue
        | _   -> collect_string_chars (char :: acc) @@ tail
      end
  in
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | '"' -> collect_string_chars [] @@ tail
      | _   -> failwith "expected to find a string token"
    end

and read_symbol_or_integer (seq : char Sequence.t) =
  let rec collect_chars acc seq =
    match Sequence.next seq with
    | None              -> (String.of_char_list @@ List.rev acc, seq)
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
  let continue () =
    read_next_token tail
  in
  if String.is_empty chars
  then failwith "invalid input"
  else begin
    match Int.of_string_opt chars with
    | Some n -> yield (TInteger n) >>= continue
    | None   -> yield (TSymbol chars) >>= continue
  end

and read_comment (seq : char Sequence.t) =
  match Sequence.next seq with
  | Some (';', tail) -> read_next_token @@ Sequence.drop_while ~f:(Fn.non is_newline) tail
  | _                -> failwith "expected to find comment"
  

let tokenize (seq : char Sequence.t) =
  run @@ read_next_token seq


let tokenize_string (string : string) =
  let seq = Sequence.of_list @@ String.to_list string
  in
  tokenize seq
