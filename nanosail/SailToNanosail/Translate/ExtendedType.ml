module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module N        = Ast
module TC       = TranslationContext

open Base
open Monads.Notations.Star(TC)


module ExtendedType = struct
  type extended_type =
    | Tuple of extended_type list
    | Int   of int
    | Bool  of int

  let rec string_of (t : extended_type) =
    match t with
    | Tuple ts -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k    -> Printf.sprintf "Int(#%d)" k
    | Bool k   -> Printf.sprintf "Bool(#%d)" k
end


let collect_extended_parameter_types (pattern : N.type_annotation Libsail.Ast.pat) =
  let rec process_type (typ : Libsail.Ast.typ) =
    let Typ_aux (unwrapped_typ, typ_location) = typ
    in
    match unwrapped_typ with
    | Libsail.Ast.Typ_internal_unknown -> TC.not_yet_implemented [%here] typ_location
    | Libsail.Ast.Typ_var _            -> TC.not_yet_implemented [%here] typ_location
    | Libsail.Ast.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] typ_location
    | Libsail.Ast.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] typ_location
    | Libsail.Ast.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] typ_location
    | Libsail.Ast.Typ_tuple elts       -> begin
        let* elts' = TC.map ~f:process_type elts
        in
        TC.return @@ ExtendedType.Tuple elts'
      end
    | Libsail.Ast.Typ_id id -> begin
        let Id_aux (unwrapped_id, id_location) = id
        in
        match unwrapped_id with
        | Libsail.Ast.Operator _    -> TC.not_yet_implemented [%here] id_location
        | Libsail.Ast.Id type_name -> begin
            match type_name with
            | "bool" -> let* metavar = TC.generate_unique_int in TC.return @@ ExtendedType.Bool metavar
            | "int"  -> let* metavar = TC.generate_unique_int in TC.return @@ ExtendedType.Int metavar
            | _      -> TC.not_yet_implemented [%here] id_location
          end
      end
    | Libsail.Ast.Typ_app (receiver_id, _argument) -> begin
        let S.Id_aux (unwrapped_receiver_id, unwrapped_receiver_id_location) = receiver_id
        in
        match unwrapped_receiver_id with
        | S.Operator _ -> TC.not_yet_implemented [%here] unwrapped_receiver_id_location
        | S.Id id -> begin
            match id with
            | "atom" -> begin
                let* metavar = TC.generate_unique_int
                in
                TC.return @@ ExtendedType.Int metavar
              end
            | "bool" -> begin
                let* metavar = TC.generate_unique_int
                in
                TC.return @@ ExtendedType.Bool metavar
              end
            | _ -> begin
                let message =
                  Printf.sprintf "Unknown type %s" id
                in
                TC.not_yet_implemented ~message [%here] unwrapped_receiver_id_location
              end
          end
      end
  in
  
  let P_aux (_unwrapped_pattern, pattern_annotation) = pattern
  in
  let typ = Libsail.Type_check.typ_of_annot pattern_annotation
  in
  let* extended_parameter_types = TC.debug_error @@ process_type typ
  in
  Stdio.printf "%s\n" (ExtendedType.string_of extended_parameter_types);
  TC.return ()
