module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module N  = Ast
module TC = TranslationContext

open Base
open Basics
open Monads.Notations.Star(TC)



(* module Context = struct *)
(*   type t = *)
(*     { *)
(*       equalities : S.typ_arg list; *)
(*     } *)
(* end *)

(* module Error = struct *)
(*   type t = *)
(*     | NotYetImplemented of ocaml_source_location * Libsail.Ast.l *)
(* end *)


(* module ExtendedTypeMonad = Monads.StateResult.Make (struct type t = Context.t end) (struct type t = Error.t end) *)
(* module ExtendedTypeMonadUtil = Monads.Util.Make(ExtendedTypeMonad) *)
(* open Monads.Notations.Plus(ExtendedTypeMonad) *)


(* let not_yet_implemented ocaml_position sail_position = *)
(*   ExtendedTypeMonad.fail @@ NotYetImplemented (ocaml_position, sail_position) *)



(* module ExtendedType = struct *)
(*   type extended_type = *)
(*     | Tuple of extended_type list *)
(*     | Int   of int *)
(*     | Bool  of int *)

(*   let rec string_of (t : extended_type) = *)
(*     match t with *)
(*     | Tuple ts -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts *)
(*     | Int k    -> Printf.sprintf "Int(#%d)" k *)
(*     | Bool k   -> Printf.sprintf "Bool(#%d)" k *)
(* end *)




(* let collect_extended_parameter_types (pattern : N.type_annotation Libsail.Ast.pat) = *)
(*   let rec process_type (typ : Libsail.Ast.typ) = *)
(*     let Typ_aux (unwrapped_typ, typ_location) = typ *)
(*     in *)
(*     match unwrapped_typ with *)
(*     | Libsail.Ast.Typ_internal_unknown -> TC.not_yet_implemented [%here] typ_location *)
(*     | Libsail.Ast.Typ_var _            -> TC.not_yet_implemented [%here] typ_location *)
(*     | Libsail.Ast.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] typ_location *)
(*     | Libsail.Ast.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] typ_location *)
(*     | Libsail.Ast.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] typ_location *)
(*     | Libsail.Ast.Typ_tuple elts       -> begin *)
(*         let* elts' = TC.map ~f:process_type elts *)
(*         in *)
(*         TC.return @@ ExtendedType.Tuple elts' *)
(*       end *)
(*     | Libsail.Ast.Typ_id id -> begin *)
(*         let Id_aux (unwrapped_id, id_location) = id *)
(*         in *)
(*         match unwrapped_id with *)
(*         | Libsail.Ast.Operator _    -> TC.not_yet_implemented [%here] id_location *)
(*         | Libsail.Ast.Id type_name -> begin *)
(*             match type_name with *)
(*             | "bool" -> let* metavar = TC.generate_unique_int in TC.return @@ ExtendedType.Bool metavar *)
(*             | "int"  -> let* metavar = TC.generate_unique_int in TC.return @@ ExtendedType.Int metavar *)
(*             | _      -> TC.not_yet_implemented [%here] id_location *)
(*           end *)
(*       end *)
(*     | Libsail.Ast.Typ_app (receiver_id, _argument) -> begin *)
(*         let S.Id_aux (unwrapped_receiver_id, unwrapped_receiver_id_location) = receiver_id *)
(*         in *)
(*         match unwrapped_receiver_id with *)
(*         | S.Operator _ -> TC.not_yet_implemented [%here] unwrapped_receiver_id_location *)
(*         | S.Id id -> begin *)
(*             match id with *)
(*             | "atom" -> begin *)
(*                 let* metavar = TC.generate_unique_int *)
(*                 in *)
(*                 TC.return @@ ExtendedType.Int metavar *)
(*               end *)
(*             | "atom_bool" -> begin *)
(*                 let* metavar = TC.generate_unique_int *)
(*                 in *)
(*                 TC.return @@ ExtendedType.Bool metavar *)
(*               end *)
(*             | _ -> begin *)
(*                 let message = *)
(*                   Printf.sprintf "Unknown type %s" id *)
(*                 in *)
(*                 TC.not_yet_implemented ~message [%here] unwrapped_receiver_id_location *)
(*               end *)
(*           end *)
(*       end *)
(*   in   *)
(*   let P_aux (_unwrapped_pattern, pattern_annotation) = pattern *)
(*   in *)
(*   let typ = Libsail.Type_check.typ_of_annot pattern_annotation *)
(*   in *)
(*   let* extended_parameter_types = TC.debug_error @@ process_type typ *)
(*   in *)
(*   Stdio.printf "%s\n" (ExtendedType.string_of extended_parameter_types); *)
(*   TC.return () *)



module ExtendedType = struct
  type t =
    | Tuple of t list
    | Int   of int
    | Bool  of int
    | Other of string

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k    -> Printf.sprintf "Int(#%d)" k
    | Bool k   -> Printf.sprintf "Bool(#%d)" k
    | Other s  -> s
end


let determine_parameter_types (parameter_bindings : N.type_annotation Libsail.Ast.pat) : S.typ =
  let P_aux (_unwrapped_pattern, pattern_annotation) = parameter_bindings
  in
  Libsail.Type_check.typ_of_annot pattern_annotation


let translate_to_extended_type (sail_type : S.typ) : ExtendedType.t TC.t =
  let translate_parameter_type (sail_type : S.typ) (mappings : int StringMap.t) : (ExtendedType.t * int StringMap.t) TC.t =
    let S.Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
    in
    match unwrapped_sail_type with
    | S.Typ_internal_unknown -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_id _             -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_var _            -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_tuple _          -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] sail_type_location
    | S.Typ_app (id, targs)  -> begin
        let S.Id_aux (unwrapped_id, id_location) = id
        in
        match unwrapped_id with
        | S.Operator _ -> TC.not_yet_implemented [%here] id_location
        | S.Id str -> begin
            match str with
            | "atom" -> begin
                match targs with
                | [targ] ->
                   begin
                     let S.A_aux (unwrapped_targ, targ_location) = targ
                     in
                     match unwrapped_targ with
                      | S.A_typ _                   -> TC.not_yet_implemented [%here] targ_location
                      | S.A_bool _                  -> TC.not_yet_implemented [%here] targ_location
                      | S.A_nexp numeric_expression -> begin
                          let S.Nexp_aux (unwrapped_numeric_expression, numeric_expression_location) = numeric_expression
                          in
                          match unwrapped_numeric_expression with
                           | S.Nexp_id _         -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_var _        -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_constant _   -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_app (_, _)   -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_times (_, _) -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_sum (_, _)   -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_minus (_, _) -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_exp _        -> TC.not_yet_implemented [%here] numeric_expression_location
                           | S.Nexp_neg _        -> TC.not_yet_implemented [%here] numeric_expression_location
                        end
                   end
                | _ -> TC.not_yet_implemented ~message:"Only one type argument supported" [%here] sail_type_location
              end
          end
      end
  
  let S.Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
  in
  match unwrapped_sail_type with
   | S.Typ_internal_unknown -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_id _             -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_var _            -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_tuple _          -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_app (_, _)       -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] sail_type_location


let determine_extended_type
      (parameter_bindings : N.type_annotation Libsail.Ast.pat)
      (_return_type        : Libsail.Ast.typ                  )
  =
  let _parameter_types = determine_parameter_types parameter_bindings
  in
  TC.return ()
