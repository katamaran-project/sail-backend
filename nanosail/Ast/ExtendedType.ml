open Base


type unknown_data = {
    ocaml_location : Lexing.position;
    sail_location  : Libsail.Ast.l;
    annotation     : string;
  }

module Parameter = struct
  type t =
    | Tuple   of t list
    | Int     of int
    | Bool    of int
    | Other   of string
    | Unknown of unknown_data

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts   -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k      -> Printf.sprintf "int(#%d)" k
    | Bool k     -> Printf.sprintf "bool(#%d)" k
    | Other s    -> s
    | Unknown _  -> Printf.sprintf "unknown"

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

    and other_to_fexpr (x : string) : FExpr.t =
      FExpr.mk_application ~positional:[FExpr.mk_string x] "ExtType:Param:Other"

    and unknown_to_fexpr
          (ocaml_location : Lexing.position    )
          (sail_location  : Libsail.Parse_ast.l)
          (annotation     : string             ) : FExpr.t
      =
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
      FExpr.mk_application ~keyword "ExtType:Param:Unknown"
  
    in  
    match extended_parameter_type with
     | Tuple ts -> tuple_to_fexpr ts
     | Int n -> int_to_fexpr n
     | Bool n -> bool_to_fexpr n
     | Other x -> other_to_fexpr x
     | Unknown { ocaml_location; sail_location; annotation } -> unknown_to_fexpr ocaml_location sail_location annotation
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
end = struct
  type t =
    | Var      of int
    | Constant of Z.t
    | Add      of t * t
    | Sub      of t * t
    | Mul      of t * t
    | Neg      of t
end and BoolExpression : sig
  type t =
    | Var                  of int
    | And                  of t * t
    | Or                   of t * t
    | Equal                of IntExpression.t * IntExpression.t
    | NotEqual             of IntExpression.t * IntExpression.t
    | LessThan             of IntExpression.t * IntExpression.t
    | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
    | GreaterThan          of IntExpression.t * IntExpression.t
    | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
end = struct
  type t =
    | Var                  of int
    | And                  of t * t
    | Or                   of t * t
    | Equal                of IntExpression.t * IntExpression.t
    | NotEqual             of IntExpression.t * IntExpression.t
    | LessThan             of IntExpression.t * IntExpression.t
    | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
    | GreaterThan          of IntExpression.t * IntExpression.t
    | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
    end

module ReturnValue = struct
  type t =
    | Int     of IntExpression.t
    | Bool    of BoolExpression.t
    | Other   of string
    | Tuple   of t list
    | Unknown of unknown_data
end
