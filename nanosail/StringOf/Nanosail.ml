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
  match numexp with
  | Constant n     -> Z.to_string n
  | Add (e1, e2)   -> Printf.sprintf "(%s + %s)" (numeric_expression e1) (numeric_expression e2)
  | Minus (e1, e2) -> Printf.sprintf "(%s - %s)" (numeric_expression e1) (numeric_expression e2)
  | Times (e1, e2) -> Printf.sprintf "(%s * %s)" (numeric_expression e1) (numeric_expression e2)
  | Neg e          -> Printf.sprintf "-%s" (numeric_expression e)
  | Id id          -> identifier id
  | Var id         -> identifier id

and numeric_constraint (nc : Ast.NumericConstraint.t) =
  match nc with
  | Equal (e1, e2)      -> Printf.sprintf "(%s == %s)" (numeric_expression e1) (numeric_expression e2)
  | BoundedGE (e1, e2)  -> Printf.sprintf "(%s >= %s)" (numeric_expression e1) (numeric_expression e2)
  | BoundedGT (e1, e2)  -> Printf.sprintf "(%s > %s)"  (numeric_expression e1) (numeric_expression e2)
  | BoundedLE (e1, e2)  -> Printf.sprintf "(%s <= %s)" (numeric_expression e1) (numeric_expression e2)
  | BoundedLT (e1, e2)  -> Printf.sprintf "(%s < %s)"  (numeric_expression e1) (numeric_expression e2)
  | Not_equal (e1, e2)  -> Printf.sprintf "(%s != %s)" (numeric_expression e1) (numeric_expression e2)
  | Var id              -> identifier id
  | True                -> "NC_true"
  | False               -> "NC_false"
  | Set (_, _)          -> failwith "Not yet imnplemented"
  | Or (_, _)           -> failwith "Not yet imnplemented"
  | And (_, _)          -> failwith "Not yet imnplemented"
  | App (_, _)          -> failwith "Not yet imnplemented"
