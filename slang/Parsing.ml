open Base



exception ParseError of string


(* let parse_tokens (tokens : Token.t Sequence.t) = *)
(*   let stack = ref [[]] *)
(*   in *)
(*   let add_level () = *)
(*     stack := [] :: !stack *)
(*   and pop_level () = *)
(*     match !stack with *)
(*     | []    -> raise (ParseError "too many )") *)
(*     | x::xs -> begin *)
(*         stack := xs; *)
(*         x *)
(*       end *)
(*   and push_value x = *)
(*     match !stack with *)
(*     | []      -> failwith "too many )" *)
(*     | xs::xss -> begin *)
(*         stack := (x :: xs) :: xss *)
(*       end *)
(*   in *)
(*   let make_list xs = *)
(*     let rec aux xs = *)
(*       match xs with *)
(*       | []    -> Value.Nil *)
(*       | x::xs -> Value.Cons (x, aux xs) *)
(*     in *)
(*     if List.is_empty xs *)
(*     then Value.Nil *)
(*     else aux xs *)
(*   in *)
(*   let create_new_list () = *)
(*     add_level () *)
(*   and pop_list () = *)
(*     let elts = pop_level () *)
(*     in *)
(*     make_list @@ List.rev elts *)
(*   in *)
(*   let process_token (token : Token.t)  = *)
(*     match token with *)
(*     | Token.LeftParenthesis  -> create_new_list () *)
(*     | Token.RightParenthesis -> push_value @@ pop_list () *)
(*     | Token.Symbol symbol    -> push_value @@ Value.Symbol symbol *)
(*     | Token.String str       -> push_value @@ Value.String str *)
(*     | Token.Integer n        -> push_value @@ Value.Integer n *)
(*     | Token.True             -> push_value @@ Value.Bool true *)
(*     | Token.False            -> push_value @@ Value.Bool false *)
(*     | Token.Quote            -> raise @@ ParseError "quote not supported yet :-(" *)
(*   in *)
(*   Sequence.iter ~f:process_token tokens; *)
(*   match !stack with *)
(*   | []  -> failwith "too many )" *)
(*   | [x] -> List.rev x *)
(*   | _   -> failwith "open lists left" *)


module TokenSource = Monads.Reader.MakeSequenceSource(struct type t = Token.t end)
module TokenReader = Monads.Reader.Make(TokenSource)


let parse_tokens tokens =
  let open TokenReader in
  let open Monads.Notations.Star(TokenReader)
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
    | _                         -> let* x = parse_single ()
                                   and* xs = parse_multiple ()
                                   in
                                   return @@ x::xs
  in
  let (result, rest) = run (parse_multiple ()) tokens
  in
  if Sequence.is_empty rest
  then result
  else failwith "BUG"    


let parse_string string =
  let tokens = Tokenizing.tokenize @@ Sequence.of_list @@ String.to_list string
  in
  parse_tokens tokens
