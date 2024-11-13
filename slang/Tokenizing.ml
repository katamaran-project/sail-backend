open Base
open Sequence.Generator

module T = Token

let is_newline = Char.equal '\n'


let rec read_next_token (seq : (char * 'loc) Sequence.t) =
  match Sequence.next seq with
  | None              -> return ()
  | Some (pair, tail) -> begin
      let continue () = read_next_token tail
      in
      match pair with
      | ('(', _loc)  -> yield T.LeftParenthesis  >>= continue
      | (')', _loc)  -> yield T.RightParenthesis >>= continue
      | ('\'', _loc) -> yield T.Quote            >>= continue
      | ('#', _loc)  -> read_boolean seq
      | ('"', _loc)  -> read_string seq
      | (';', _loc)  -> read_comment seq
      | (char, _loc) -> begin
          if Char.is_whitespace char
          then continue ()
          else read_symbol_or_integer seq
        end
    end

and read_boolean (seq : (char * 'loc) Sequence.t) =
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | ('#', _loc) -> begin
          match Sequence.next tail with
          | None               -> failwith "unfinished boolean"
          | Some (char, tail) -> begin
              let continue () =
                read_next_token tail
              in
              match char with
              | ('t', _loc)  -> yield T.True >>= continue
              | ('f', _loc)  -> yield T.False >>= continue
              | _    -> failwith "unrecognized boolean"
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end

and read_string (seq : (char * 'loc) Sequence.t) =
  let rec collect_string_chars
            (acc : char list               )
            (seq : (char * 'loc) Sequence.t)
    =
    match Sequence.next seq with
    | None -> failwith "unfinished string"
    | Some (pair, tail) -> begin
        let continue () =
          read_next_token tail
        in
        match pair with
        | ('\\', _loc) -> begin
            match Sequence.next tail with
            | None              -> failwith "unfinished string"
            | Some (('"' , _loc), tail) -> collect_string_chars ('"'  :: acc) @@ tail
            | Some (('n' , _loc), tail) -> collect_string_chars ('\n' :: acc) @@ tail
            | Some (('\\', _loc), tail) -> collect_string_chars ('\\' :: acc) @@ tail
            | Some ((_   , _loc), tail) -> collect_string_chars ('\\' :: acc) @@ tail
          end
        | ('"', _loc)  -> yield (T.String (String.of_char_list @@ List.rev acc)) >>= continue
        | (char, _loc)  -> collect_string_chars (char :: acc) @@ tail
      end
  in
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | ('"', _loc) -> collect_string_chars [] @@ tail
      | _   -> failwith "expected to find a string token"
    end

and read_symbol_or_integer (seq : (char * 'loc) Sequence.t) =
  let rec collect_chars (acc : char list) (seq : (char * 'loc) Sequence.t) =
    match Sequence.next seq with
    | None              -> (String.of_char_list @@ List.rev acc, seq)
    | Some (pair, tail) -> begin
        match pair with
        | ('(' , _loc)
        | (')' , _loc)
        | (' ' , _loc)
        | ('\t', _loc)
        | ('\n', _loc)
        | ('\r', _loc) -> (String.of_char_list @@ List.rev acc, seq)
        | (char, _loc)  -> collect_chars (char :: acc) tail
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
    | Some n -> yield (T.Integer n) >>= continue
    | None   -> yield (T.Symbol chars) >>= continue
  end

and read_comment (seq : (char * 'loc) Sequence.t) =
  match Sequence.next seq with
  | Some ((';', _loc), tail) -> read_next_token @@ Sequence.drop_while ~f:(fun (c, _) -> not @@ is_newline c) tail
  | _                        -> failwith "expected to find comment"


let tokenize (seq : (char * 'loc) Sequence.t) =
  run @@ read_next_token seq


let sequence_of_string (string : string) : (char * int) Sequence.t =
  Sequence.of_list @@ List.map ~f:(fun (x, y) -> (y, x)) @@ Auxlib.zip_indices @@ String.to_list string


let tokenize_string (string : string) =
  tokenize @@ sequence_of_string string
