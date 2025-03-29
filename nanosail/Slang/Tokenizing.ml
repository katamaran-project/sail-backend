open ExtBase
open Sequence.Generator
open Location

module T = Token

let is_newline = Char.equal '\n'


let rec read_next_token (seq : (char * location) Sequence.t) : (unit, location * location * Token.t) t =
  match Sequence.next seq with
  | None              -> return ()
  | Some (pair, tail) -> begin
      let continue () = read_next_token tail
      in
      match pair with
      | ('(', loc)  -> yield (loc, loc, T.LeftParenthesis)  >>= continue
      | (')', loc)  -> yield (loc, loc, T.RightParenthesis) >>= continue
      | ('\'', loc) -> yield (loc, loc, T.Quote)            >>= continue
      | ('#', _loc)  -> read_boolean seq
      | ('"', _loc)  -> read_string seq
      | (';', _loc)  -> read_comment seq
      | (char, _loc) -> begin
          if Char.is_whitespace char
          then continue ()
          else read_symbol_or_integer seq
        end
    end

and read_boolean (seq : (char * location) Sequence.t) : (unit, location * location * T.t) t =
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (pair, tail) -> begin
      match pair with
      | ('#', start_loc) -> begin
          match Sequence.next tail with
          | None              -> failwith "unfinished boolean"
          | Some (pair, tail) -> begin
              let continue () =
                read_next_token tail
              in
              match pair with
              | ('t', end_loc)  -> yield (start_loc, end_loc, T.True) >>= continue
              | ('f', end_loc)  -> yield (start_loc, end_loc, T.False) >>= continue
              | (_  , end_loc)  -> failwith @@ Printf.sprintf "unrecognized boolean at %s" end_loc#to_string
            end
        end
      | _ -> failwith "expected to find a boolean token"
    end

and read_string (seq : (char * location) Sequence.t) =
  let rec collect_string_chars
            (start_loc : location                    )
            (acc       : char list               )
            (seq       : (char * location) Sequence.t)
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
            | None                      -> failwith "unfinished string"
            | Some (('"' , _loc), tail) -> collect_string_chars start_loc ('"'  :: acc) @@ tail
            | Some (('n' , _loc), tail) -> collect_string_chars start_loc ('\n' :: acc) @@ tail
            | Some (('\\', _loc), tail) -> collect_string_chars start_loc ('\\' :: acc) @@ tail
            | Some ((_   , _loc), tail) -> collect_string_chars start_loc ('\\' :: acc) @@ tail
          end
        | ('"', end_loc)  -> yield (start_loc, end_loc, T.String (String.of_char_list @@ List.rev acc)) >>= continue
        | (char, _loc) -> collect_string_chars start_loc (char :: acc) @@ tail
      end
  in
  match Sequence.next seq with
  | None -> failwith "no more input"
  | Some (char, tail) -> begin
      match char with
      | ('"', start_loc) -> collect_string_chars start_loc [] @@ tail
      | _                -> failwith "internal error: expected to find a string token"
    end

and read_symbol_or_integer (seq : (char * location) Sequence.t) =
  let rec collect_chars
            (acc : (char * location) list)
            (seq : (char * location) Sequence.t) : (char * location) list * (char * location) Sequence.t
    =
    match Sequence.next seq with
    | None              -> (List.rev acc, seq)
    | Some (pair, tail) -> begin
        match pair with
        | ('(' , _loc)
        | (')' , _loc)
        | (' ' , _loc)
        | ('\t', _loc)
        | ('\n', _loc)
        | ('\r', _loc) -> (List.rev acc, seq)
        | (char, loc)  -> collect_chars ((char, loc) :: acc) tail
      end
  in
  let (pairs, tail) = collect_chars [] seq
  in
  let chars : string =
    String.of_char_list @@ List.map ~f:fst pairs
  and start_loc : location =
    match pairs with
    | (_, loc) :: _ -> loc
    | []            -> failwith "zero-length symbol/integer"
  and end_loc : location =
    snd @@ List.last_exn pairs
  in
  let continue () =
    read_next_token tail
  in
  match Int.of_string_opt chars with
  | Some n -> yield (start_loc, end_loc, T.Integer n) >>= continue
  | None   -> yield (start_loc, end_loc, T.Symbol chars) >>= continue

and read_comment (seq : (char * location) Sequence.t) =
  match Sequence.next seq with
  | Some ((';', _loc), tail) -> read_next_token @@ Sequence.drop_while ~f:(fun (c, _) -> not @@ is_newline c) tail
  | _                        -> failwith "internal error: expected to find comment"


let tokenize (seq : (char * location) Sequence.t) : (location * location * Token.t) Sequence.t =
  run @@ read_next_token seq


let sequence_of_string (string : string) : (char * string_location) Sequence.t =
  Sequence.of_list @@ List.map ~f:(fun (index, char) -> (char, new string_location index)) @@ List.zip_indices @@ String.to_list string


let tokenize_string (string : string) =
  tokenize @@ sequence_of_string string
