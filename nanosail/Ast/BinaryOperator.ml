module Signedness = struct
  type t =
    | Signed
    | Unsigned

  let to_fexpr (signedness : t) : FExpr.t =
    match signedness with
    | Signed   -> FExpr.mk_symbol "Signed"
    | Unsigned -> FExpr.mk_symbol "Unsigned"

  let equal
      (signedness_1 : t)
      (signedness_2 : t) : bool
    =
    match signedness_1, signedness_2 with
    | Signed  , Signed   -> true
    | Unsigned, Unsigned -> true
    | Signed  , _        -> false
    | Unsigned, _        -> false
end


module Comparison = struct
  type t =
    | LessThanOrEqualTo
    | LessThan
    | GreaterThanOrEqualTo
    | GreaterThan

  let to_fexpr (comparison : t) : FExpr.t =
    match comparison with
    | LessThanOrEqualTo    -> FExpr.mk_symbol "LessThanOrEqualTo"
    | LessThan             -> FExpr.mk_symbol "LessThan"
    | GreaterThanOrEqualTo -> FExpr.mk_symbol "GreaterThanOrEqualTo"
    | GreaterThan          -> FExpr.mk_symbol "GreaterThan"

  let equal
      (comparison_1 : t)
      (comparison_2 : t) : bool
    =
    match comparison_1, comparison_2 with
    | LessThanOrEqualTo   , LessThanOrEqualTo    -> true
    | LessThan            , LessThan             -> true
    | GreaterThanOrEqualTo, GreaterThanOrEqualTo -> true
    | GreaterThan         , GreaterThan          -> true
    | LessThanOrEqualTo   , _                    -> false
    | LessThan            , _                    -> false
    | GreaterThanOrEqualTo, _                    -> false
    | GreaterThan         , _                    -> false
end


type t =
  | Plus
  | Times
  | Minus
  | And
  | Or
  | Pair
  | Cons
  | Append
  | EqualTo
  | NotEqualTo
  | StandardComparison of Comparison.t
  | BitvectorComparison of Signedness.t * Comparison.t


let to_fexpr (operator : t) : FExpr.t =
  match operator with
   | Plus                 -> FExpr.mk_symbol "Plus"
   | Times                -> FExpr.mk_symbol "Times"
   | Minus                -> FExpr.mk_symbol "Minus"
   | And                  -> FExpr.mk_symbol "And"
   | Or                   -> FExpr.mk_symbol "Or"
   | Pair                 -> FExpr.mk_symbol "Pair"
   | Cons                 -> FExpr.mk_symbol "Cons"
   | Append               -> FExpr.mk_symbol "Append"
   | EqualTo              -> FExpr.mk_symbol "EqualTo"
   | NotEqualTo           -> FExpr.mk_symbol "NotEqualTo"
   | StandardComparison c -> begin
       let positional = [
         Comparison.to_fexpr c
       ]
       in
       FExpr.mk_application ~positional "StandardComparison"
     end
   | BitvectorComparison (signedness, comparison) -> begin
       let positional = [
         Signedness.to_fexpr signedness;
         Comparison.to_fexpr comparison
       ]
       in
       FExpr.mk_application ~positional "BitvectorComparison"
     end


let equal
    (operator_1 : t)
    (operator_2 : t) : bool
  =
  match operator_1, operator_2 with
   | Plus      , Plus       -> true
   | Times     , Times      -> true
   | Minus     , Minus      -> true
   | And       , And        -> true
   | Or        , Or         -> true
   | Pair      , Pair       -> true
   | Cons      , Cons       -> true
   | Append    , Append     -> true
   | EqualTo   , EqualTo    -> true
   | NotEqualTo, NotEqualTo -> true
   | StandardComparison comparison_1, StandardComparison comparison_2 -> begin
       Comparison.equal
         comparison_1
         comparison_2
     end
   | BitvectorComparison (signedness_1, comparison_1),
     BitvectorComparison (signedness_2, comparison_2)             -> begin
       Signedness.equal
         signedness_1
         signedness_2
       &&
       Comparison.equal
         comparison_1
         comparison_2
     end
   (* remainder of the cases still handled explicitly, so as to get a compiler warning/error in case we add more operators later on *)
   | Plus                                            , _          -> false
   | Times                                           , _          -> false
   | Minus                                           , _          -> false
   | And                                             , _          -> false
   | Or                                              , _          -> false
   | Pair                                            , _          -> false
   | Cons                                            , _          -> false
   | Append                                          , _          -> false
   | EqualTo                                         , _          -> false
   | NotEqualTo                                      , _          -> false
   | StandardComparison _                            , _          -> false
   | BitvectorComparison _                           , _          -> false
