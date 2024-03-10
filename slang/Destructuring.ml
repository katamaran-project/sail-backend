open Base
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters


type positional = { target : string }
type optional   = { target : string; default_value : Value.t }
type keyword    = { target : string; keyword : string; default_value : Value.t }


let rec process_pattern (pattern : Value.t list) =
  let rec gather_positional
      ~(positional : positional list)
      (pattern     : Value.t list   )
    =
    match pattern with
    | first :: rest -> begin
        match C.(symbol first) with
        | Some target -> gather_positional ~positional:({ target } :: positional) rest
        | None        -> gather_optional ~positional ~optional:[] pattern
      end
    | [] -> (positional, [], [])
            
  and gather_optional
      ~(positional : positional list)
      ~(optional   : optional list  )
       (pattern    : Value.t list   )
    =
    match pattern with
    | first :: rest -> begin
        match C.(tuple2 symbol value first) with
        | Some (target, default_value) -> gather_optional ~positional ~optional:({ target; default_value } :: optional) rest
        | None -> gather_keyword ~positional ~optional ~keyword:[] pattern
      end
    | [] -> (positional, optional, [])
            
  and gather_keyword
      ~(positional : positional list)
      ~(optional   : optional list  )
      ~(keyword    : keyword list   )
       (pattern    : Value.t list   )
    =
    match pattern with
    | first :: rest -> begin
        match C.(tuple3 symbol symbol value first) with
        | Some (keyword, target, default_value) -> gather_keyword ~positional ~optional ~keyword:({ target; keyword; default_value } :: keyword) rest
        | None -> raise @@ Exception.SlangError "invalid pattern in destructuring bind"
      end
    | [] -> (positional, optional, keyword)
  in
  let positional, optional, keyword = gather_positional ~positional:[] pattern
  in
  (
    List.rev positional,
    List.rev optional,
    List.rev keyword
  )


let destructure
    (pattern : Value.t list)
    (values  : Value.t list) : unit EC.t
  =
  let positional, optional, keyword = process_pattern pattern
  in
  let rec bind_positional (positional : positional list) values =
    match positional with
    | { target } :: rest_positional -> begin
        match values with
        | first :: rest_values -> begin
            let* () = EC.add_binding target first
            in
            bind_positional rest_positional rest_values
          end
        | [] -> raise @@ Exception.SlangError "insufficient positionals"
      end
    | [] -> bind_optional optional values

  and bind_optional optional values =
    match optional with
    | { target; default_value } :: rest -> begin
        match values with
        | first ::
      end
    | [] -> bind_keyword keyword values
