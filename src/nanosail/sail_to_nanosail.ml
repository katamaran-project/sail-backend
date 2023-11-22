open Ast
open Libsail.Ast
open Libsail.Ast_defs
open Libsail.Anf
open Libsail

module Big_int = Nat_big_num


type source_position = string * int * int * int

exception NotYetImplemented of source_position * l * string option

let not_yet_implemented source_position sail_location =
  raise (NotYetImplemented (source_position, sail_location, None))

let not_yet_implemented_msg source_position sail_location message =
  raise (NotYetImplemented (source_position, sail_location, Some message))


(******************************************************************************)

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | _ -> " NOT YET SUPPORTED "

(******************************************************************************)

let ty_id_of_typ_id (Id_aux (aux, location)) =
  match aux with
  | Id "bool"      -> Bool
  | Id "int"       -> Int
  | Id "list"      -> List
  | Id "prod"      -> Prod
  | Id "unit"      -> Unit
  | Id "string"    -> String
  | Id "bitvector" -> Bitvector
  | Id id          -> not_yet_implemented_msg __POS__ location (Printf.sprintf "Missing case Id \"%s\"" id)
  | Operator _     -> not_yet_implemented __POS__ location


let rec ty_of_typ (Typ_aux (typ, location)) =
  let ty_of_arg (A_aux (aux, arg_location)) =
    match aux with
    | A_nexp _  -> not_yet_implemented __POS__ arg_location
    | A_typ typ -> ty_of_typ typ
    | A_bool _  -> not_yet_implemented __POS__ arg_location
  in
  match typ with
  | Typ_internal_unknown -> not_yet_implemented __POS__ location
  | Typ_var _            -> not_yet_implemented __POS__ location
  | Typ_fn (_, _)        -> not_yet_implemented __POS__ location
  | Typ_bidir (_, _)     -> not_yet_implemented __POS__ location
  | Typ_exist (_, _, _)  -> not_yet_implemented __POS__ location
  | Typ_id id            -> Ty_id (ty_id_of_typ_id id)
  | Typ_tuple items ->
     (
       match items with
       | []                -> not_yet_implemented_msg __POS__ location "Should not occur"
       | h_typ :: typs     ->
          let h_ty = ty_of_typ h_typ in
          let f ty1 typ2 =
            let ty2 = ty_of_typ typ2 in
            Ty_app (Prod, [ty1; ty2]) in
          List.fold_left f h_ty typs
     )
  | Typ_app (id, args) ->
     (
       match string_of_id id with
       | "atom"      -> Ty_id Int
       | "atom_bool" -> Ty_id Bool
       | _           -> Ty_app (ty_id_of_typ_id id, List.map ty_of_arg args)
     )


(******************************************************************************)

let ty_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> ty_of_typ (Type_check.typ_of exp)
  | Pat_when _ -> Ty_nys


(******************************************************************************)

let rec binds_of_pat (P_aux (aux, a)) =
  match aux with
  | P_lit (L_aux (L_unit, _)) -> [("()", Ty_id Unit)]
  | P_id id ->
      let x = string_of_id id in
      let ty = ty_of_typ (Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_tuple pats ->
      List.concat (List.map binds_of_pat pats)
  | _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]

let binds_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]


(******************************************************************************)

let value_of_lit (L_aux (aux, _)) =
  match aux with
  | L_true     -> Val_bool true
  | L_false    -> Val_bool false
  | L_num n    -> Val_int n
  | L_unit     -> Val_unit
  | L_string s -> Val_string s
  | _ -> Val_nys

let rec expression_of_aval = function
  | AV_lit (lit, _) ->
      Exp_val (value_of_lit lit)
  | AV_id (id, _) ->
      Exp_var (string_of_id id)
  | AV_tuple (h :: t) ->
      let e_h = expression_of_aval h in
      let f e1 aval2 =
        let e2 = expression_of_aval aval2 in
        Exp_binop (Pair, e1, e2) in
      List.fold_left f e_h t
  | AV_list (l, _) ->
      Exp_list (List.map expression_of_aval l)
  | _ -> Exp_nys

let rec statement_of_aexp (AE_aux (aux, _, _)) =
  match aux with
  | AE_val aval ->
      Stm_exp (expression_of_aval aval)
  | AE_app (id, avals, _) ->
      let x = string_of_id id in (
        match avals with
        | [aval1; aval2] when x = "sail_cons" ->
            let e1 = expression_of_aval aval1 in
            let e2 = expression_of_aval aval2 in
            Stm_exp (Exp_binop (Cons, e1, e2))
        | _ ->
            Stm_call (x, List.map expression_of_aval avals)
      )
  | AE_let (_, id, _, aexp1, aexp2, _) ->
      let x = string_of_id id in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_let (x, s1, s2)
  | AE_if (aval, aexp1, aexp2, _) ->
      let s = Stm_exp (expression_of_aval aval) in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_if (s, s1, s2)
  | AE_match (aval, cases, _) ->
      statement_of_match aval cases
  | _ -> Stm_nys

and statement_of_match aval = function
  | [ (AP_aux (AP_nil _, _, _), _, aexp1);
      (AP_aux (AP_cons (
        AP_aux ((AP_id (id_h, _)), _, _),
        AP_aux ((AP_id (id_t, _)), _, _)
      ), _, _), _, aexp2)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval aval);
        alt_nil  = statement_of_aexp aexp1;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_aexp aexp2;
      }
  | [ (AP_aux (AP_cons (
        AP_aux ((AP_id (id_h, _)), _, _),
        AP_aux ((AP_id (id_t, _)), _, _)
      ), _, _), _, aexp1);
      (AP_aux (AP_nil _, _, _), _, aexp2)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval aval);
        alt_nil  = statement_of_aexp aexp2;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_aexp aexp1;
      }
  | [ (AP_aux (AP_tuple [
        AP_aux ((AP_id (id_l, _)), _, _);
        AP_aux ((AP_id (id_r, _)), _, _);
      ], _, _),_ , aexp)
    ] ->
      Stm_match_prod {
        s   = Stm_exp (expression_of_aval aval);
        xl  = string_of_id id_l;
        xr  = string_of_id id_r;
        rhs = statement_of_aexp aexp;
      }
  | _ -> Stm_nys

let body_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (anf exp)
  | Pat_when _       -> Stm_nys


(******************************************************************************)

let ir_funcl (FCL_aux (FCL_funcl (id, pexp), _)) = {
  funName = string_of_id(id);
  funType = {
    arg_types = binds_of_pexp pexp;
    ret_type  = ty_of_pexp pexp
  };
  funBody = body_of_pexp pexp
}

let ir_fundef (FD_aux ((FD_function (_, _, funcls)), _)) =
  match funcls with
  | [funcl] -> Some (ir_funcl funcl)
  | _       -> None

let translate_type_abbreviation _definition_annotation _type_annotation (Id_aux (identifier, identifier_location)) quantifier type_arg : definition =
  let TypQ_aux (quantifier, quantifier_location) = quantifier
  and A_aux (arg, arg_location) = type_arg
  in
  let translate_numeric_expression (Nexp_aux (numeric_expression, numexp_location)) =
    match quantifier with
    | TypQ_tq _ -> not_yet_implemented __POS__ quantifier_location
    | TypQ_no_forall ->
       (
         match numeric_expression with
         | Nexp_id _ -> not_yet_implemented __POS__ numexp_location
         | Nexp_var _ -> not_yet_implemented __POS__ numexp_location
         | Nexp_constant constant ->
            (
              match identifier with
              | Id id -> TypeDefinition (TD_abbreviation (id, TA_numeric_expression (Nexp_constant constant)))
              | Operator _ -> not_yet_implemented __POS__ identifier_location
            )

         | Nexp_app (_, _) -> not_yet_implemented __POS__ numexp_location
         | Nexp_times (_, _) -> not_yet_implemented __POS__ numexp_location
         | Nexp_sum (_, _) -> not_yet_implemented __POS__ numexp_location
         | Nexp_minus (_, _) -> not_yet_implemented __POS__ numexp_location
         | Nexp_exp _ -> not_yet_implemented __POS__ numexp_location
         | Nexp_neg _ -> not_yet_implemented __POS__ numexp_location
       )
  in
  match arg with
  | A_nexp numeric_expression ->
     translate_numeric_expression numeric_expression
  | A_typ _ -> not_yet_implemented __POS__ arg_location
  | A_bool _ -> not_yet_implemented __POS__ arg_location

let translate_type_definition (definition_annotation : def_annot) (TD_aux (type_definition, type_annotation)) : definition =
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg) ->
     translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_record (_, _, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_variant (_, _, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_enum (_, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_bitfield (_, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc

let translate_top_level_constant (definition_annotation : def_annot) (VS_aux (value_specification, _vspec_annotation)) : definition =
  let VS_val_spec (TypSchm_aux (TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)), _type_scheme_location), _identifier, _extern) = value_specification
  in
  not_yet_implemented __POS__ definition_annotation.loc

let translate_register (_definition_annotation : def_annot) (DEC_aux (DEC_reg (sail_type, Id_aux (identifier, identifier_location), expression), (_spec_location, _spec_annotation))) : definition =
  let identifier_string =
    match identifier with
    | Id string -> string
    | Operator _ -> not_yet_implemented __POS__ identifier_location
  in
  (
    match expression with
    | None -> ()
    | Some (E_aux (_expr, (location, _annotation))) ->
       not_yet_implemented __POS__ location
  );
  let nano_type = ty_of_typ sail_type
  in
  RegisterDefinition { identifier = identifier_string; typ = nano_type }

let translate_definition (DEF_aux (def, annotation) as sail_definition) : (sail_definition * definition) =
  try
    match def with
    | DEF_fundef fd ->
       (
         match ir_fundef fd with
         | Some translation -> (sail_definition, FunctionDefinition translation)
         | None             -> not_yet_implemented __POS__ annotation.loc
       )
    | DEF_type type_definition  -> (sail_definition, translate_type_definition annotation type_definition)
    | DEF_mapdef _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_impl _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_let _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_val value_specification ->
      (sail_definition, translate_top_level_constant annotation value_specification)
    | DEF_outcome (_, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_instantiation (_, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_fixity (_, _, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_overload (_, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_default _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_scattered _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_measure (_, _, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_loop_measures (_, _) ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_register specification ->
      (sail_definition, translate_register annotation specification)
    | DEF_internal_mutrec _ ->
       not_yet_implemented __POS__ annotation.loc
    | DEF_pragma (_, _, _) ->
       not_yet_implemented __POS__ annotation.loc
  with NotYetImplemented (source_position, sail_location, message) ->
    let (file, line_number, _, _) = source_position
    in
    (sail_definition, UntranslatedDefinition { filename=file; line_number = line_number; sail_location = sail_location; message = message })

let sail_to_nanosail ast name =
  let lift f (original, translation) =
    Option.map (fun x -> (original, x)) (f translation)
  in
  let sail_definitions = ast.defs in
  let nano_definitions = List.map translate_definition sail_definitions in
  let collect f =
    List.filter_map (lift f) nano_definitions
  in
  {
    program_name             = name;
    function_definitions     = List.filter_map (fun (_original, translation) -> extract_function_definition translation) nano_definitions;
    type_definitions         = collect extract_type_definition;
    register_definitions     = collect extract_register_definition;
    untranslated_definitions = collect extract_untranslated_definition;
  }
