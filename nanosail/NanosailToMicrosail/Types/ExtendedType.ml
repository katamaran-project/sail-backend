open Base
open Monads.Notations.Star(AnnotationContext)

module PP = PPrint
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
  let named_extended_parameter_types =
    let pairs = List.zip_exn ft.parameters eft.extended_parameter_types
    in
    List.map ~f:(fun ((id, _), ext_type) -> (Id.string_of id, ext_type)) pairs
  in
  let* parameter_types =
    let pp_pair id ext_type =
      let* ext_type' = pp_extended_type ext_type
      in
      AC.return @@ PP.(string id ^^ colon ^^ space ^^ ext_type')
    in
    AC.map ~f:(Auxlib.uncurry pp_pair) named_extended_parameter_types
  and* return_type =
    let* ert' = pp_extended_type eft.extended_return_type
    in
    AC.return PP.(string "RV" ^^ colon ^^ space ^^ ert')
  in
  let types = List.append parameter_types [return_type]
  in
  AC.return @@ PP.(separate hardline types)
