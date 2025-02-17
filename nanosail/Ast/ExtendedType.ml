open! ExtBase


module Parameter = struct
  type t =
    | Tuple      of t list
    | Int        of int
    | Bool       of int
    | Identifier of string
    | Bitvector  of Numeric.Expression.t   (* todo not used anywhere yet; update translation functionality *)
    | Unknown    of { ocaml_location : Lexing.position;
                      sail_location  : Libsail.Ast.l  ;
                      sail_type      : string         }

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts       -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k          -> Printf.sprintf "int(#%d)" k
    | Bool k         -> Printf.sprintf "bool(#%d)" k
    | Identifier s   -> s
    | Bitvector nexp -> Printf.sprintf "bitvector(%s)" (Numeric.Expression.to_string nexp)
    | Unknown _      -> Printf.sprintf "unknown"

  let rec to_fexpr (extended_parameter_type : t) : FExpr.t =
    let prefix str =
      "ExtType:Param:" ^ str
    in
    let tuple_to_fexpr (ts : t list) : FExpr.t =
      let ts' =
        List.map ~f:to_fexpr ts
      in
      FExpr.mk_application ~positional:ts' @@ prefix "Tuple"

    and int_to_fexpr (n : int) : FExpr.t =
      let n' =
        FExpr.mk_int n
      in
      FExpr.mk_application ~positional:[n'] @@ prefix "Int"

    and bool_to_fexpr (n : int) : FExpr.t =
      let b' =
        FExpr.mk_int n
      in
      FExpr.mk_application ~positional:[b'] @@ prefix "Bool"

    and unknown_to_fexpr
          (ocaml_location : Lexing.position    )
          (sail_location  : Libsail.Parse_ast.l)
          (sail_type      : string             ) : FExpr.t
      =
      let ocaml_location' =
        match ocaml_location with
        | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
           FExpr.mk_string @@ Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

      and sail_location' =
        FExpr.mk_string @@ Sail.string_of_location sail_location

      and sail_type' =
        FExpr.mk_string sail_type

      in
      let keyword =
        [
          ("ocaml_location", ocaml_location');
          ("sail_location", sail_location');
          ("sail_type", sail_type');
        ]
      in
      FExpr.mk_application ~keyword @@ prefix "Unknown"

    in
    match extended_parameter_type with
     | Tuple ts                                             -> tuple_to_fexpr ts
     | Int n                                                -> int_to_fexpr n
     | Bool n                                               -> bool_to_fexpr n
     | Identifier s                                         -> FExpr.mk_application ~positional:[FExpr.String s] "ExtType:Param:Id"
     | Bitvector nexp                                       -> FExpr.mk_application ~positional:[Numeric.Expression.to_fexpr nexp] "ExtType:Param:Bitvector"
     | Unknown { ocaml_location; sail_location; sail_type } -> unknown_to_fexpr ocaml_location sail_location sail_type
end

(* OCaml requires this duplication for recursive modules *)
module rec IntExpression : sig

  type t =
    | Var      of int
    | Constant of Z.t
    | Add      of t * t
    | Sub      of t * t
    | Mul      of t * t
    | Neg      of t
    | Unknown  of { ocaml_location : Lexing.position;
                    sail_location  : Libsail.Ast.l  ;
                    sail_type      : string         }

  val to_fexpr : t -> FExpr.t

end = struct

  type t =
    | Var      of int
    | Constant of Z.t
    | Add      of t * t
    | Sub      of t * t
    | Mul      of t * t
    | Neg      of t
    | Unknown  of { ocaml_location : Lexing.position;
                    sail_location  : Libsail.Ast.l  ;
                    sail_type      : string         }

  let rec to_fexpr (int_expression : t) : FExpr.t =
    let prefix str =
      "IntExpr:" ^ str
    in
    match int_expression with
    | Var n        -> FExpr.mk_application ~positional:[FExpr.mk_int n]             @@ prefix "Var"
    | Constant n   -> FExpr.mk_application ~positional:[FExpr.mk_int @@ Z.to_int n] @@ prefix "Constant"
    | Add (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]   @@ prefix "Add"
    | Sub (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]   @@ prefix "Sub"
    | Mul (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2]   @@ prefix "Mul"
    | Neg e        -> FExpr.mk_application ~positional:[to_fexpr e]                 @@ prefix "Neg"
    | Unknown { ocaml_location; sail_location; sail_type } -> begin
        let keyword = [
            ("OCamlLocation", FExpr.mk_ocaml_location ocaml_location);
            ("SailLocation", FExpr.mk_sail_location sail_location);
            ("SailType", FExpr.String sail_type);
          ]
        in
        FExpr.mk_application ~keyword @@ prefix "Unknown"
      end


end and BoolExpression : sig

  type t =
    | Bool                 of bool
    | Var                  of int
    | And                  of t * t
    | Or                   of t * t
    | Equal                of IntExpression.t * IntExpression.t
    | NotEqual             of IntExpression.t * IntExpression.t
    | LessThan             of IntExpression.t * IntExpression.t
    | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
    | GreaterThan          of IntExpression.t * IntExpression.t
    | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
    | Unknown              of { ocaml_location : Lexing.position;
                                sail_location  : Libsail.Ast.l  ;
                                sail_type      : string         }

  val to_fexpr : t -> FExpr.t

end = struct

  type t =
    | Bool                 of bool
    | Var                  of int
    | And                  of t * t
    | Or                   of t * t
    | Equal                of IntExpression.t * IntExpression.t
    | NotEqual             of IntExpression.t * IntExpression.t
    | LessThan             of IntExpression.t * IntExpression.t
    | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
    | GreaterThan          of IntExpression.t * IntExpression.t
    | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
    | Unknown              of { ocaml_location : Lexing.position;
                                sail_location  : Libsail.Ast.l  ;
                                sail_type      : string         }

  let rec to_fexpr (bool_expression : t) : FExpr.t =
    let prefix str =
      "BoolExpr:" ^ str
    in
    match bool_expression with
    | Bool b                        -> FExpr.mk_bool b
    | Var n                         -> FExpr.mk_application ~positional:[FExpr.mk_int n] @@ prefix "Var"
    | And (e1, e2)                  -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] @@ prefix "And"
    | Or (e1, e2)                   -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] @@ prefix "Or"
    | Equal (e1, e2)                -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "Equal"
    | NotEqual (e1, e2)             -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "NotEqual"
    | LessThan (e1, e2)             -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "LessThan"
    | LessThanOrEqualTo (e1, e2)    -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "LessThanOrEqualTo"
    | GreaterThan (e1, e2)          -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "GreaterThan"
    | GreaterThanOrEqualTo (e1, e2) -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] @@ prefix "GreaterThanOrEqualTo"
    | Unknown { ocaml_location; sail_location; sail_type } -> begin
        let keyword = [
            ("OCamlLocation", FExpr.mk_ocaml_location ocaml_location);
            ("SailLocation", FExpr.mk_sail_location sail_location);
            ("SailType", FExpr.String sail_type);
          ]
        in
        FExpr.mk_application ~keyword @@ prefix "Unknown"
      end
end

module ReturnValue = struct
  type t =
    | Int     of IntExpression.t
    | Bool    of BoolExpression.t
    | Other   of string
    | Tuple   of t list
    | Unknown of { ocaml_location : Lexing.position;
                   sail_location  : Libsail.Ast.l  ;
                   sail_type      : string         }

  let rec to_fexpr (return_type : t) : FExpr.t =
    let prefix str =
      "Return:" ^ str
    in
    match return_type with
    | Int e        -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e]  @@ prefix "Int"
    | Bool e       -> FExpr.mk_application ~positional:[BoolExpression.to_fexpr e] @@ prefix "Bool"
    | Other s      -> FExpr.mk_application ~positional:[FExpr.mk_string s]         @@ prefix "Other"
    | Tuple ts     -> FExpr.mk_application ~positional:(List.map ~f:to_fexpr ts)   @@ prefix "Tuple"
    | Unknown { ocaml_location; sail_location; sail_type } -> begin
        let ocaml_location' =
          match ocaml_location with
          | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
             FExpr.mk_string @@ Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

        and sail_location' =
          FExpr.mk_string @@ Sail.string_of_location sail_location

        and sail_type' =
          FExpr.mk_string sail_type

        in
        let keyword =
          [
            ("ocaml_location", ocaml_location');
            ("sail_location" , sail_location' );
            ("sail_type"     , sail_type'     );
          ]
        in
        FExpr.mk_application ~keyword @@ prefix "Unknown"
      end
end
