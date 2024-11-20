open Base


module Parameter = struct
  type t =
    | Tuple      of t list
    | Int        of int
    | Bool       of int
    | Identifier of string
    | Unknown    of { ocaml_location : Lexing.position;
                      sail_location  : Libsail.Ast.l  ;
                      sail_type      : string         }

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts     -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k        -> Printf.sprintf "int(#%d)" k
    | Bool k       -> Printf.sprintf "bool(#%d)" k
    | Identifier s -> s
    | Unknown _    -> Printf.sprintf "unknown"

  let rec to_fexpr (extended_parameter_type : t) : FExpr.t =
    let tuple_to_fexpr (ts : t list) : FExpr.t =
      let ts' =
        List.map ~f:to_fexpr ts
      in
      FExpr.mk_application ~positional:ts' "ExtType:Param:Tuple"

    and int_to_fexpr (n : int) : FExpr.t =
      let n' =
        FExpr.mk_int n
      in
      FExpr.mk_application ~positional:[n'] "ExtType:Param:Int"

    and bool_to_fexpr (n : int) : FExpr.t =
      let b' =
        FExpr.mk_int n
      in
      FExpr.mk_application ~positional:[b'] "ExtType:Param:Bool"

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
      FExpr.mk_application ~keyword "ExtType:Param:Unknown"

    in
    match extended_parameter_type with
     | Tuple ts                                             -> tuple_to_fexpr ts
     | Int n                                                -> int_to_fexpr n
     | Bool n                                               -> bool_to_fexpr n
     | Identifier s                                         -> FExpr.mk_application ~positional:[FExpr.String s] "ExtType:Param:Id"
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
    match int_expression with
    | Var n        -> FExpr.mk_application ~positional:[FExpr.mk_int n] "IntExpr:Var"
    | Constant n   -> FExpr.mk_application ~positional:[FExpr.mk_int @@ Z.to_int n] "IntExpr:Constant"
    | Add (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "IntExpr:Add"
    | Sub (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "IntExpr:Sub"
    | Mul (e1, e2) -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "IntExpr:Mul"
    | Neg e        -> FExpr.mk_application ~positional:[to_fexpr e] "IntExpr:Neg"
    | Unknown { ocaml_location; sail_location; sail_type } -> begin
        let keyword = [
            ("OCamlLocation", FExpr.mk_ocaml_location ocaml_location);
            ("SailLocation", FExpr.mk_sail_location sail_location);
            ("SailType", FExpr.String sail_type);
          ]
        in
        FExpr.mk_application ~keyword "BoolExpr:Unknown"
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
    match bool_expression with
    | Bool b                        -> FExpr.mk_bool b
    | Var n                         -> FExpr.mk_application ~positional:[FExpr.mk_int n] "BoolExpr:Var"
    | And (e1, e2)                  -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "BoolExpr:And"
    | Or (e1, e2)                   -> FExpr.mk_application ~positional:[to_fexpr e1; to_fexpr e2] "BoolExpr:Or"
    | Equal (e1, e2)                -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:Equal"
    | NotEqual (e1, e2)             -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:NotEqual"
    | LessThan (e1, e2)             -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:LessThan"
    | LessThanOrEqualTo (e1, e2)    -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:LessThanOrEqualTo"
    | GreaterThan (e1, e2)          -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:GreaterThan"
    | GreaterThanOrEqualTo (e1, e2) -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e1; IntExpression.to_fexpr e2] "BoolExpr:GreaterThanOrEqualTo"
    | Unknown { ocaml_location; sail_location; sail_type } -> begin
        let keyword = [
            ("OCamlLocation", FExpr.mk_ocaml_location ocaml_location);
            ("SailLocation", FExpr.mk_sail_location sail_location);
            ("SailType", FExpr.String sail_type);
          ]
        in
        FExpr.mk_application ~keyword "BoolExpr:Unknown"
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
                   annotation     : string         }

  let rec to_fexpr (return_type : t) : FExpr.t =
    match return_type with
    | Int e        -> FExpr.mk_application ~positional:[IntExpression.to_fexpr e] "Return:Int"
    | Bool e       -> FExpr.mk_application ~positional:[BoolExpression.to_fexpr e] "Return:Bool"
    | Other s      -> FExpr.mk_application ~positional:[FExpr.mk_string s] "Return:Other"
    | Tuple ts     -> FExpr.mk_application ~positional:(List.map ~f:to_fexpr ts) "Return:Tuple"
    | Unknown { ocaml_location; sail_location; annotation } -> begin
        let ocaml_location' =
          match ocaml_location with
          | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
             FExpr.mk_string @@ Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

        and sail_location' =
          FExpr.mk_string @@ Sail.string_of_location sail_location

        and annotation' =
          FExpr.mk_string annotation

        in
        let keyword =
          [
            ("ocaml_location", ocaml_location');
            ("sail_location", sail_location');
            ("annotation", annotation');
          ]
        in
        FExpr.mk_application ~keyword "Return:Unknown"
      end
end
