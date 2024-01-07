open Base


module T = Tokenizer


exception ParseError of string
  

let parse_tokens (tokens : Tokenizer.token Sequence.t) =
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
  let process_token (token : T.token)  =
    match token with
    | T.TLeftParenthesis  -> create_new_list ()
    | T.TRightParenthesis -> push_value @@ pop_list ()
    | T.TSymbol symbol    -> push_value @@ Value.Symbol symbol
    | T.TString str       -> push_value @@ Value.String str
    | T.TInteger n        -> push_value @@ Value.Integer n
    | T.TTrue             -> push_value @@ Value.Bool true
    | T.TFalse            -> push_value @@ Value.Bool false
    | T.TQuote            -> raise @@ ParseError "quote not supported yet :-("
  in
  Sequence.iter ~f:process_token tokens;
  match !stack with
  | []  -> failwith "too many )"
  | [x] -> List.rev x
  | _   -> failwith "open lists left"

 

let parse_string string =
  let tokens = T.tokenize @@ Sequence.of_list @@ String.to_list string
  in
  parse_tokens tokens
