open Base


let identifier = Ast.Identifier.string_of


let rec nanotype (t : Ast.Type.t) =
  match t with
  | Int              -> "Type.Int"
  | Bool             -> "Type.Bool"
  | String           -> "Type.String"
  | List _           -> "Type.List"
  | Product (t1, t2) -> Printf.sprintf "(%s * %s)" (nanotype t1) (nanotype t2)
  | Sum (t1, t2)     -> Printf.sprintf "(%s + %s)" (nanotype t1) (nanotype t2)
  | Unit             -> "Type.Unit"
  | Bitvector numexp -> Printf.sprintf "Type.Bitvector(%s)" (numeric_expression numexp)
  | Enum id          -> Printf.sprintf "Type.Enum(%s)" (Ast.Identifier.string_of id)
  | Record           -> "Type.Record"
  | Nat              -> "Type.Nat"
  | Atom             -> "Type.Atom"
  | Custom id        -> Printf.sprintf "Type.Custom(%s)" (identifier id)
  | Application (constructor, targs) -> begin
      let constructor' = nanotype constructor
      and targs' = List.map ~f:type_argument targs
      in
      Printf.sprintf "%s(%s)" constructor' (String.concat ~sep:"," targs')
    end
  | Tuple ts -> begin
      let ts' = List.map ~f:nanotype ts
      in
      Printf.sprintf "(%s)" (String.concat ~sep:"," ts')
    end

and type_argument (targ : Ast.TypeArgument.t) =
  match targ with
   | Type t                   -> nanotype t
   | NumericExpression numexp -> numeric_expression numexp
   | Bool nc                  -> numeric_constraint nc

and numeric_expression (numexp : Ast.NumericExpression.t) =

and numeric_constraint = Ast.NumericConstraint.to_string
