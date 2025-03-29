open ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters


type positional = { target : string }
type optional   = { target : string; default_value : Value.t }
type keyword    = { target : string; keyword : string; default_value : Value.t }


let gather_patterns (patterns : Value.t list) =
  let rec gather_positionals
      ~(positionals : positional list)
      (patterns     : Value.t list   ) : (positional list * optional list * keyword list)
    =
    match patterns with
    | first :: rest -> begin
        match C.(symbol first) with
        | Some target -> gather_positionals ~positionals:({ target } :: positionals) rest
        | None        -> gather_optionals ~positionals ~optionals:[] patterns
      end
    | [] -> (positionals, [], [])

  and gather_optionals
      ~(positionals : positional list)
      ~(optionals   : optional list  )
       (patterns    : Value.t list   ) : (positional list * optional list * keyword list)
    =
    match patterns with
    | first :: rest -> begin
        match C.(tuple2 symbol anything first) with
        | Some (target, default_value) -> gather_optionals ~positionals ~optionals:({ target; default_value } :: optionals) rest
        | None -> gather_keywords ~positionals ~optionals ~keywords:[] patterns
      end
    | [] -> (positionals, optionals, [])

  and gather_keywords
      ~(positionals : positional list)
      ~(optionals   : optional list  )
      ~(keywords    : keyword list   )
       (patterns    : Value.t list   ) : (positional list * optional list * keyword list)
    =
    match patterns with
    | first :: rest -> begin
        match C.(tuple3 symbol symbol anything first) with
        | Some (keyword, target, default_value) -> begin
            if Value.is_keyword keyword
            then gather_keywords ~positionals ~optionals ~keywords:({ target; keyword; default_value } :: keywords) rest
            else raise @@ Exception.SlangError "keyword symbol expected"
          end
        | None -> raise @@ Exception.SlangError "invalid pattern in destructuring bind"
      end
    | [] -> (positionals, optionals, keywords)
  in
  let positional, optional, keyword = gather_positionals ~positionals:[] patterns
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
  let positionals, optionals, keywords = gather_patterns pattern
  in
  let rec bind_positionals (positionals : positional list) values =
    match positionals with
    | { target } :: rest_positional -> begin
        match values with
        | first :: rest_values -> begin
            let* () = EC.add_binding target first
            in
            bind_positionals rest_positional rest_values
          end
        | [] -> raise @@ Exception.SlangError "insufficient positionals"
      end
    | [] -> bind_optionals optionals values

  and bind_optionals optionals values =
    match optionals with
    | { target; default_value } :: rest_optionals -> begin
        match values with
        | value :: rest_values -> begin
            let* () = EC.add_binding target value
            in
            bind_optionals rest_optionals rest_values
          end
        | [] -> begin
            let* () = EC.add_binding target default_value
            in
            bind_optionals rest_optionals []
          end
      end
    | [] -> bind_keywords keywords values

  and bind_keywords keywords values =
    match keywords with
    | { target; keyword; default_value } :: rest_keywords -> begin
        match values with
        | v1 :: v2 :: rest_values -> begin
            if
              Value.equal (Value.Symbol keyword) v1
            then
              let* () = EC.add_binding target v2
              in
              bind_keywords rest_keywords rest_values
            else
              let* () = EC.add_binding target default_value
              in
              bind_keywords rest_keywords values
          end
        | _ -> begin
            let* () = EC.add_binding target default_value
            in
            bind_keywords rest_keywords values
          end
      end
    | [] -> begin
        (* add this point, we don't expect any more values *)
        match values with
        | [] -> EC.return ()
        | _  -> raise @@ Exception.SlangError "too many values for given pattern"
      end
  in
  bind_positionals positionals values
