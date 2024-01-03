let parse_tokens (tokens : Tokenizer.token Seq.t) =
  let stack = ref [[]]
  in
  let add_level () =
    stack := [] :: !stack
  and pop_level () =
    match !stack with
    | []    -> failwith "too many )"
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
  let rec make_list xs =
    match xs with
    | []    -> Ast.Nil
    | x::xs -> Ast.Cons (x, make_list xs)

  in
  let process_token (token : Tokenizer.token)  =
    match token with
    | Tokenizer.TLeftParenthesis  -> add_level ()
    | Tokenizer.TRightParenthesis -> let elts = pop_level () in push_value @@ make_list @@ List.rev elts
    | Tokenizer.TSymbol symbol    -> push_value @@ Symbol symbol
    | Tokenizer.TString str       -> push_value @@ String str
    | Tokenizer.TInteger n        -> push_value @@ Integer n
    | Tokenizer.TTrue             -> push_value @@ Bool true
    | Tokenizer.TFalse            -> push_value @@ Bool false
  in
  Seq.iter process_token tokens;
  match !stack with
  | []  -> failwith "too many )"
  | [x] -> List.rev x
  | _   -> failwith "open lists left"


let parse_string string =
  let tokens = Tokenizer.tokenize (String.to_seq string)
  in
  parse_tokens tokens
