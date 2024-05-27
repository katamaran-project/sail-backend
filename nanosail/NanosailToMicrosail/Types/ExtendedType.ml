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


let pp_extended_function_type (eft : Ast.ExtendedFunctionType.t) : PP.document AC.t =
  let* parameter_types = AC.map ~f:pp_extended_type eft.extended_parameter_types
  and* return_type     = pp_extended_type eft.extended_return_type
  in
  let types = List.append parameter_types [return_type]
  in
  AC.return @@ PP.(separate hardline types)

