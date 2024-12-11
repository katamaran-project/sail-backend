open Base


module type ValueFactory = sig
  type t
    
  val mk_symbol  : string -> t
  val mk_string  : string -> t
  val mk_integer : int -> t
  val mk_bool    : bool -> t
  val mk_list    : t list -> t
end

module Make(VF : ValueFactory) = struct

  exception ParseError of string


  module TokenSource = Monads.Reader.MakeSequenceSource(struct type t = Token.t end)
  module TokenReader = Monads.Reader.Make(TokenSource)

  
  let parse_tokens (tokens : TokenSource.t) : VF.t list =
    let open TokenReader in
    let open Monads.Notations.Star(TokenReader)
    in
    let rec parse_single () : VF.t t =
      let* c = current
      in
      match c with
      | None       -> raise @@ ParseError "ran out of tokens to parse"
      | Some token -> begin
          match token with
          | LeftParenthesis  -> parse_list ()
          | RightParenthesis -> raise @@ ParseError "unexpected right parenthesis"
          | Quote            -> parse_quote ()
          | Symbol s         -> let* () = next in return @@ VF.mk_symbol s
          | String s         -> let* () = next in return @@ VF.mk_string s
          | Integer n        -> let* () = next in return @@ VF.mk_integer n
          | True             -> let* () = next in return @@ VF.mk_bool true
          | False            -> let* () = next in return @@ VF.mk_bool false
        end

    and parse_list () : VF.t t =
      let* opening = current
      in
      match opening with
      | Some LeftParenthesis -> begin
          let* ()         = next
          and* list_items = parse_multiple ()
          and* closing    = current
          in
          match closing with
          | None                  -> raise @@ ParseError "unclosed list"
          | Some RightParenthesis -> let* () = next in return @@ VF.mk_list list_items
          | _                     -> failwith "BUG: parse_multiple should only stop processing when encountering EOF or a right parenthesis"
        end
      | _                         -> failwith "BUG: parse_list should only be called when encountering a left parenthesis"

    and parse_quote () : VF.t t =
      let* quote = current
      in
      match quote with
      | Some Quote                -> let* () = next in let* quoted = parse_single () in return @@ VF.mk_list [ VF.mk_symbol "quote"; quoted ]
      | _                         -> failwith "BUG: parse_quote should only be called when encountering a quote"

    and parse_multiple () : VF.t list t =
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


  let parse_string (string : string) : VF.t list =
    let triples = Tokenizing.tokenize_string string
    in
    let tokens  = Sequence.map ~f:Auxlib.Triple.third triples
    in
    parse_tokens tokens
end
