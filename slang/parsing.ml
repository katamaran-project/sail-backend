open Base



exception ParseError of string
  

let parse_tokens (tokens : Token.t Sequence.t) =
  let stack = ref [[]]
  in
  let add_level () =
    stack := [] :: !stack
  and pop_level () =
    match !stack with
    | []    -> raise (ParseError "too many )")
    | x::xs -> begin
        stack := xs;
        x
      end
  and push_value x =
    match !stack with
    | []      -> failwith "too many )"
    | xs::xss -> begin
        stack := (x :: xs) :: xss
      end
  in
  let make_list xs =
    let rec aux xs =
      match xs with
      | []    -> Value.Nil
      | x::xs -> Value.Cons (x, aux xs)
    in
    if List.is_empty xs
    then Value.Nil
    else aux xs
  in
  let create_new_list () =
    add_level ()
  and pop_list () =
    let elts = pop_level ()
    in
    make_list @@ List.rev elts
  in
  let process_token (token : Token.t)  =
    match token with
    | Token.LeftParenthesis  -> create_new_list ()
    | Token.RightParenthesis -> push_value @@ pop_list ()
    | Token.Symbol symbol    -> push_value @@ Value.Symbol symbol
    | Token.String str       -> push_value @@ Value.String str
    | Token.Integer n        -> push_value @@ Value.Integer n
    | Token.True             -> push_value @@ Value.Bool true
    | Token.False            -> push_value @@ Value.Bool false
    | Token.Quote            -> raise @@ ParseError "quote not supported yet :-("
  in
  Sequence.iter ~f:process_token tokens;
  match !stack with
  | []  -> failwith "too many )"
  | [x] -> List.rev x
  | _   -> failwith "open lists left"

 

let parse_string string =
  let tokens = Tokenizer.tokenize @@ Sequence.of_list @@ String.to_list string
  in
  parse_tokens tokens
