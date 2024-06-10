open Base


let identifier = Ast.Identifier.string_of


let rec nanotype (t : Ast.nanotype) =
  match t with
  | Ty_int              -> "Ty_int"
  | Ty_bool             -> "Ty_bool"
  | Ty_string           -> "Ty_string"
  | Ty_list _           -> "Ty_list"
  | Ty_prod (t1, t2)    -> Printf.sprintf "(%s * %s)" (nanotype t1) (nanotype t2)
  | Ty_sum (t1, t2)     -> Printf.sprintf "(%s + %s)" (nanotype t1) (nanotype t2)
  | Ty_unit             -> "Ty_unit"
  | Ty_bitvector numexp -> Printf.sprintf "Ty_bitvector(%s)" (numeric_expression numexp)
  | Ty_record           -> "Ty_record"
  | Ty_nat              -> "Ty_nat"
  | Ty_atom             -> "Ty_atom"
  | Ty_custom id        -> Printf.sprintf "Ty_custom(%s)" (identifier id)
  | Ty_app (constructor, targs) -> begin
      let constructor' = nanotype constructor
      and targs' = List.map ~f:type_argument targs
      in
      Printf.sprintf "%s(%s)" constructor' (String.concat ~sep:"," targs')
    end
  | Ty_tuple ts -> begin
      let ts' = List.map ~f:nanotype ts
      in
      Printf.sprintf "(%s)" (String.concat ~sep:"," ts')
    end
                          
and type_argument (targ : Ast.type_argument) =
  match targ with
   | Ast.TA_type t        -> nanotype t
   | Ast.TA_numexp numexp -> numeric_expression numexp
   | Ast.TA_bool nc       -> numeric_constraint nc

and numeric_expression (numexp : Ast.NumericExpression.t) =
  match numexp with
  | Constant n     -> Z.to_string n
  | Add (e1, e2)   -> Printf.sprintf "(%s + %s)" (numeric_expression e1) (numeric_expression e2)
  | Minus (e1, e2) -> Printf.sprintf "(%s - %s)" (numeric_expression e1) (numeric_expression e2)
  | Times (e1, e2) -> Printf.sprintf "(%s * %s)" (numeric_expression e1) (numeric_expression e2)
  | Neg e          -> Printf.sprintf "-%s" (numeric_expression e)
  | Id id          -> identifier id
  | Var id         -> identifier id

and numeric_constraint (nc : Ast.numeric_constraint) =
  match nc with
  | NC_equal (e1, e2)      -> Printf.sprintf "(%s == %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_bounded_ge (e1, e2) -> Printf.sprintf "(%s >= %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_bounded_gt (e1, e2) -> Printf.sprintf "(%s > %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_bounded_le (e1, e2) -> Printf.sprintf "(%s <= %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_bounded_lt (e1, e2) -> Printf.sprintf "(%s < %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_not_equal (e1, e2)  -> Printf.sprintf "(%s != %s)" (numeric_expression e1) (numeric_expression e2)
  | NC_var id              -> identifier id
  | NC_true                -> "NC_true"
  | NC_false               -> "NC_false"
  | NC_set (_, _)          -> failwith "Not yet imnplemented"
  | NC_or (_, _)           -> failwith "Not yet imnplemented"
  | NC_and (_, _)          -> failwith "Not yet imnplemented"
  | NC_app (_, _)          -> failwith "Not yet imnplemented"
