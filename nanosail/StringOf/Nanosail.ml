open Base


let identifier = Id.string_of


let rec nanotype (t : Ast.nanotype) =
  match t with
  | Ast.Ty_int              -> "Ty_int"
  | Ast.Ty_bool             -> "Ty_bool"
  | Ast.Ty_string           -> "Ty_string"
  | Ast.Ty_list _           -> "Ty_list"
  | Ast.Ty_prod (t1, t2)    -> Printf.sprintf "(%s * %s)" (nanotype t1) (nanotype t2)
  | Ast.Ty_sum (t1, t2)     -> Printf.sprintf "(%s + %s)" (nanotype t1) (nanotype t2)
  | Ast.Ty_unit             -> "Ty_unit"
  | Ast.Ty_bitvector numexp -> Printf.sprintf "Ty_bitvector(%s)" (numeric_expression numexp)
  | Ast.Ty_record           -> "Ty_record"
  | Ast.Ty_nat              -> "Ty_nat"
  | Ast.Ty_atom             -> "Ty_atom"
  | Ast.Ty_custom id        -> Printf.sprintf "Ty_custom(%s)" (identifier id)
  | Ast.Ty_app (constructor, targs) -> begin
      let constructor' = nanotype constructor
      and targs' = List.map ~f:type_argument targs
      in
      Printf.sprintf "%s(%s)" constructor' (String.concat ~sep:"," targs')
    end
  | Ast.Ty_tuple ts -> begin
      let ts' = List.map ~f:nanotype ts
      in
      Printf.sprintf "(%s)" (String.concat ~sep:"," ts')
    end
                          
and type_argument (targ : Ast.type_argument) =
  match targ with
   | Ast.TA_type t        -> nanotype t
   | Ast.TA_numexp numexp -> numeric_expression numexp
   | Ast.TA_bool nc       -> numeric_constraint nc

and numeric_expression (numexp : Ast.numeric_expression) =
  match numexp with
  | Ast.NE_constant n     -> Z.to_string n
  | Ast.NE_add (e1, e2)   -> Printf.sprintf "(%s + %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NE_minus (e1, e2) -> Printf.sprintf "(%s - %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NE_times (e1, e2) -> Printf.sprintf "(%s * %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NE_neg e          -> Printf.sprintf "-%s" (numeric_expression e)
  | Ast.NE_id id          -> identifier id
  | Ast.NE_var id         -> identifier id

and numeric_constraint (nc : Ast.numeric_constraint) =
  match nc with
  | Ast.NC_equal (e1, e2)      -> Printf.sprintf "(%s == %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_bounded_ge (e1, e2) -> Printf.sprintf "(%s >= %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_bounded_gt (e1, e2) -> Printf.sprintf "(%s > %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_bounded_le (e1, e2) -> Printf.sprintf "(%s <= %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_bounded_lt (e1, e2) -> Printf.sprintf "(%s < %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_not_equal (e1, e2)  -> Printf.sprintf "(%s != %s)" (numeric_expression e1) (numeric_expression e2)
  | Ast.NC_var id              -> identifier id
  | Ast.NC_true                -> "NC_true"
  | Ast.NC_false               -> "NC_false"
  | Ast.NC_set (_, _)          -> failwith "Not yet imnplemented"
  | Ast.NC_or (_, _)           -> failwith "Not yet imnplemented"
  | Ast.NC_and (_, _)          -> failwith "Not yet imnplemented"
  | Ast.NC_app (_, _)          -> failwith "Not yet imnplemented"
