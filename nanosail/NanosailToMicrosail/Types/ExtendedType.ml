open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let rec pp_extended_type (extended_type : Ast.ExtendedType.t) : PP.document AC.t =
  match extended_type with
  | Ast.ExtendedType.Int k    -> AC.return @@ PP.(string "int" ^^ space ^^ string k)
  | Ast.ExtendedType.Bool k   -> AC.return @@ PP.(string "bool" ^^ space ^^ string k)
  | Ast.ExtendedType.Other s  -> AC.return @@ PP.string s
  | Ast.ExtendedType.Tuple ts -> begin
      let* ts' = AC.map ~f:pp_extended_type ts (* add parentheses around each t of ts *)
      in
      AC.return @@ PP.(separate (string " * ") ts')
    end


let pp_extended_function_type
      (ft  : Ast.function_type         )
      (eft : Ast.ExtendedFunctionType.t) : PP.document AC.t
  =
  let parameter_names =
    List.map ~f:(fun (id, _) -> Id.string_of id) ft.parameters
  and parameter_extended_types =
    eft.extended_parameter_types
  and return_extended_type =
    eft.extended_return_type
  in
  let* pp_parameter_names =
    AC.return @@ List.map ~f:(fun name -> PP.(string "parameter " ^^ PP.string name)) parameter_names
  and* pp_parameter_extended_types =
    AC.map ~f:pp_extended_type parameter_extended_types
  in
  let pp_parameter_pairs =
    List.zip_exn pp_parameter_names pp_parameter_extended_types
  in
  let* pp_return_value_pair =
    let* ret = pp_extended_type return_extended_type
    in
    AC.return (PP.string "return value", ret)
  in
  let pairs =
    List.append pp_parameter_pairs [pp_return_value_pair]
  in
  AC.return @@ PP.description_list pairs
