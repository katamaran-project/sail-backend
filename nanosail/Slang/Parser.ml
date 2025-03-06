open! ExtBase


exception ParseError of string


module TokenSource    = Monads.Dispenser.MakeSequenceSource(struct type t = Token.t end)
module TokenDispenser = Monads.Dispenser.Make(TokenSource)


let parse_tokens tokens =
  let open TokenDispenser in
  let open Monads.Notations.Star(TokenDispenser)
  in
  let rec parse_single () : Value.t t =
    let* c = current
    in
    match c with
    | None       -> raise @@ ParseError "ran out of tokens to parse"
    | Some token -> begin
        match token with
        | LeftParenthesis  -> parse_list ()
        | RightParenthesis -> raise @@ ParseError "unexpected right parenthesis"
        | Quote            -> parse_quote ()
        | Symbol s         -> let* () = next in return @@ Value.Symbol s
        | String s         -> let* () = next in return @@ Value.String s
        | Integer n        -> let* () = next in return @@ Value.Integer n
        | True             -> let* () = next in return @@ Value.Bool true
        | False            -> let* () = next in return @@ Value.Bool false
      end

  and parse_list () =
    let* opening = current
    in
    match opening with
    | Some LeftParenthesis -> begin
        let* () = next
        and* list_items = parse_multiple ()
        and* closing = current
        in
        match closing with
        | None                  -> raise @@ ParseError "unclosed list"
        | Some RightParenthesis -> let* () = next in return @@ Value.list_to_cons list_items
        | _                     -> failwith "BUG: parse_multiple should only stop processing when encountering EOF or a right parenthesis"
      end
    | _                         -> failwith "BUG: parse_list should only be called when encountering a left parenthesis"

  and parse_quote () =
    let* quote = current
    in
    match quote with
    | Some Quote                -> let* () = next in let* quoted = parse_single () in return @@ Value.list_to_cons [ Symbol "quote"; quoted ]
    | _                         -> failwith "BUG: parse_quote should only be called when encountering a quote"

  and parse_multiple () : Value.t list t =
    let* c = current
    in
    match c with
    | None
    | Some RightParenthesis     -> return @@ []
    | _                         -> let* x  = parse_single ()
                                   and* xs = parse_multiple ()
                                   in
                                   return @@ x::xs
  in
  let (result, rest) = run (parse_multiple ()) tokens
  in
  if Sequence.is_empty rest
  then result
  else failwith "invalid input"


let parse_string string =
  let triples =
    Tokenizing.tokenize_string string
  in
  let tokens =
    Sequence.map ~f:Tuple.Triple.third triples
  in
  parse_tokens tokens
