module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast



type source_position = string * int * int * int

exception NotYetImplemented of source_position * S.l * string option

let not_yet_implemented ?(message = "") source_position sail_location =
  let message =
    if message == ""
    then None
    else Some message
  in
  raise (NotYetImplemented (source_position, sail_location, message))


(******************************************************************************)

let string_of_id (S.Id_aux (aux, location)) =
  match aux with
  | Id x -> x
  | Operator _ -> not_yet_implemented __POS__ location

(******************************************************************************)

let rec translate_numeric_expression (S.Nexp_aux (numeric_expression, numexp_location)) : N.numeric_expression =
  match numeric_expression with
  | Nexp_constant constant     -> NE_constant constant
  | Nexp_times (x, y)          -> NE_times (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_sum (x, y)            -> NE_add (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_minus (x, y)          -> NE_minus (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_neg x                 -> NE_neg (translate_numeric_expression x)
  | Nexp_id (Id_aux (id, loc)) ->
    (
      match id with
      | S.Id s                 -> NE_id s
      | S.Operator _           -> not_yet_implemented __POS__ loc
    )
  | Nexp_exp _                 -> not_yet_implemented __POS__ numexp_location
  | Nexp_var _                 -> not_yet_implemented __POS__ numexp_location
  | Nexp_app (_, _)            -> not_yet_implemented __POS__ numexp_location


let ty_id_of_typ_id (S.Id_aux (aux, location)) =
  match aux with
  | Id "bool"      -> N.Bool
  | Id "int"       -> N.Int
  | Id "list"      -> N.List
  | Id "prod"      -> N.Prod
  | Id "unit"      -> N.Unit
  | Id "string"    -> N.String
  | Id "bitvector" -> N.Bitvector
  | Id id          -> not_yet_implemented ~message:(Printf.sprintf "Missing case Id \"%s\"" id) __POS__ location
  | Operator _     -> not_yet_implemented __POS__ location


let rec ty_of_typ (S.Typ_aux (typ, location)) =
  let ty_of_arg (S.A_aux (aux, arg_location)) : N.type_argument =
    match aux with
    | A_nexp e  -> N.TA_numexp (translate_numeric_expression e)
    | A_typ typ -> N.TA_type (ty_of_typ typ)
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
       | []                -> not_yet_implemented ~message:"Should not occur" __POS__ location
       | h_typ :: typs     ->
          let h_ty = ty_of_typ h_typ in
          let f ty1 typ2 =
            let ty2 = ty_of_typ typ2 in
            N.Ty_app (Prod, [TA_type ty1; TA_type ty2]) in
          List.fold_left f h_ty typs
     )
  | Typ_app (Id_aux (id, id_loc) as sail_id, args) ->
     match id, args with
     | Id "atom", []      -> Ty_id Int
     | Id "atom_bool", [] -> Ty_id Bool
     | Id _, _            -> Ty_app (ty_id_of_typ_id sail_id, List.map ty_of_arg args)
     | Operator _, _      -> not_yet_implemented __POS__ id_loc


let _translate_type_argument (S.A_aux (type_argument, location)) : N.type_argument =
  match type_argument with
  | A_nexp e -> TA_numexp (translate_numeric_expression e) 
  | A_typ t  -> TA_type (ty_of_typ t)
  | A_bool _ -> not_yet_implemented __POS__ location


(******************************************************************************)

let ty_of_pexp (S.Pat_aux (aux, (loc, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> ty_of_typ (Libsail.Type_check.typ_of exp)
  | Pat_when _       -> not_yet_implemented __POS__ loc


(******************************************************************************)

let rec binds_of_pat (S.P_aux (aux, a)) =
  match aux with
  | P_lit (L_aux (L_unit, _)) -> [("()", N.Ty_id Unit)]
  | P_id id ->
      let x = string_of_id id in
      let ty = ty_of_typ (Libsail.Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_tuple pats ->
      List.concat (List.map binds_of_pat pats)
  | _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]

let binds_of_pexp (S.Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]


(******************************************************************************)

let value_of_lit (S.L_aux (aux, location)) =
  match aux with
  | L_true     -> N.Val_bool true
  | L_false    -> N.Val_bool false
  | L_num n    -> N.Val_int n
  | L_unit     -> N.Val_unit
  | L_string s -> N.Val_string s
  | S.L_zero   -> not_yet_implemented __POS__ location
  | S.L_one    -> not_yet_implemented __POS__ location
  | S.L_hex _  -> not_yet_implemented __POS__ location
  | S.L_bin _  -> not_yet_implemented __POS__ location
  | S.L_undef  -> not_yet_implemented __POS__ location
  | S.L_real _ -> not_yet_implemented __POS__ location

let rec expression_of_aval location (value : 'a S.aval) =
  match value with
  | AV_lit (lit, _) ->
     N.Exp_val (value_of_lit lit)
  | AV_id (id, _) ->
     N.Exp_var (string_of_id id)
  | AV_tuple elts ->
     (
       match elts with
       | [] -> not_yet_implemented ~message:"Should not occur" __POS__ location
       | h::t ->
          let e_h = expression_of_aval location h in
          let f e1 aval2 =
            let e2 = expression_of_aval location aval2 in
            N.Exp_binop (Pair, e1, e2) in
          List.fold_left f e_h t
     )
  | AV_list (lst, _) ->
     Exp_list (List.map (expression_of_aval location) lst)
  | S.AV_ref (_, _)    -> not_yet_implemented __POS__ location
  | S.AV_vector (_, _) -> not_yet_implemented __POS__ location
  | S.AV_record (_, _) -> not_yet_implemented __POS__ location
  | S.AV_cval (_, _)   -> not_yet_implemented __POS__ location


let rec statement_of_aexp (S.AE_aux (aux, _, location)) =
  match aux with
  | AE_val aval ->
      N.Stm_exp (expression_of_aval location aval)
  | AE_app (id, avals, _) ->
      let x = string_of_id id in (
        match avals with
        | [aval1; aval2] when x = "sail_cons" ->
            let e1 = expression_of_aval location aval1 in
            let e2 = expression_of_aval location aval2 in
            Stm_exp (Exp_binop (Cons, e1, e2))
        | _ ->
            Stm_call (x, List.map (expression_of_aval location) avals)
      )
  | AE_let (_, id, _, aexp1, aexp2, _) ->
      let x = string_of_id id in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_let (x, s1, s2)
  | AE_if (aval, aexp1, aexp2, _) ->
      let s = N.Stm_exp (expression_of_aval location aval) in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_if (s, s1, s2)
  | AE_match (aval, cases, _) ->
      statement_of_match location aval cases
  | S.AE_typ (_, _)              -> not_yet_implemented __POS__ location
  | S.AE_assign (_, _)           -> not_yet_implemented __POS__ location
  | S.AE_block (_, _, _)         -> not_yet_implemented __POS__ location
  | S.AE_return (_, _)           -> not_yet_implemented __POS__ location
  | S.AE_exit (_, _)             -> not_yet_implemented __POS__ location
  | S.AE_throw (_, _)            -> not_yet_implemented __POS__ location
  | S.AE_field (_, _, _)         -> not_yet_implemented __POS__ location
  | S.AE_try (_, _, _)           -> not_yet_implemented __POS__ location
  | S.AE_struct_update (_, _, _) -> not_yet_implemented __POS__ location
  | S.AE_for (_, _, _, _, _, _)  -> not_yet_implemented __POS__ location
  | S.AE_loop (_, _, _)          -> not_yet_implemented __POS__ location
  | S.AE_short_circuit (_, _, _) -> not_yet_implemented __POS__ location

and statement_of_match location matched cases =
  match cases with
  | [ (AP_aux (AP_nil _, _, _), _, aexp1);
      (AP_aux (AP_cons (
        AP_aux ((AP_id (id_h, _)), _, _),
        AP_aux ((AP_id (id_t, _)), _, _)
      ), _, _), _, aexp2)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval location matched);
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
        s        = Stm_exp (expression_of_aval location matched);
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
        s   = Stm_exp (expression_of_aval location matched);
        xl  = string_of_id id_l;
        xr  = string_of_id id_r;
        rhs = statement_of_aexp aexp;
      }
  | _ -> not_yet_implemented __POS__ location

let body_of_pexp (S.Pat_aux (aux, (location, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (S.anf exp)
  | Pat_when _       -> not_yet_implemented __POS__ location


(******************************************************************************)

let ir_funcl (S.FCL_aux (S.FCL_funcl (id, pexp), _)) =
  {
    N.funName = string_of_id(id);
    N.funType = {
      arg_types = binds_of_pexp pexp;
      ret_type  = ty_of_pexp pexp
    };
    N.funBody = body_of_pexp pexp
  }

let ir_fundef (S.FD_aux ((FD_function (_, _, funcls)), _)) =
  match funcls with
  | [funcl] -> Some (ir_funcl funcl)
  | _       -> None

let translate_type_abbreviation
      _definition_annotation
      _type_annotation
      (S.Id_aux (identifier, identifier_location))
      (S.TypQ_aux (quantifier, quantifier_location))
      (S.A_aux (arg, arg_location)) : N.definition =
  match quantifier with
  | TypQ_tq _ -> not_yet_implemented __POS__ quantifier_location
  | TypQ_no_forall ->
     (
       match identifier with
       | Id id_string ->
          (
            match arg with
            | A_nexp numeric_expression ->
               let nano_numeric_expression = translate_numeric_expression numeric_expression
               in
               TypeDefinition (TD_abbreviation (id_string, TA_numeric_expression nano_numeric_expression))
            | A_typ _  -> not_yet_implemented __POS__ arg_location
            | A_bool _ -> not_yet_implemented __POS__ arg_location
          )
       | Operator _ -> not_yet_implemented __POS__ identifier_location
     )

let translate_enum
      (_definition_annotation : S.def_annot)
      (_type_annotation : 'a S.annot)
      (S.Id_aux (identifier, identifier_location))
      (cases : S.id list) : N.definition =
  match identifier with
  | S.Id identifier ->
     (
       let cases =
         let string_of_case (S.Id_aux (case, case_location)) =
           match case with
           | S.Id string  -> string
           | S.Operator _ -> not_yet_implemented __POS__ case_location
         in
         List.map string_of_case cases
       in
       EnumDefinition { identifier = identifier; cases = cases }
     )
  | S.Operator _ -> not_yet_implemented __POS__ identifier_location


let translate_type_definition (definition_annotation : S.def_annot) (S.TD_aux (type_definition, type_annotation)) : N.definition =
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg) ->
     translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_record (_, _, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_variant (_, _, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_enum (identifier, cases, _) ->
     translate_enum definition_annotation type_annotation identifier cases
  | TD_bitfield (_, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc

let translate_top_level_type_constraint
      (definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : N.definition =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          _identifier, _extern) = value_specification
  in
  not_yet_implemented __POS__ definition_annotation.loc

let translate_register
      (_definition_annotation : S.def_annot)
      (S.DEC_aux
         (DEC_reg (sail_type, Id_aux (identifier, identifier_location), expression),
          (_spec_location, _spec_annotation))) : N.definition =
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

let translate_mapping_definition
      (_definition_annotation : S.def_annot)
      (S.MD_aux (_definition, (location, _mapping_annotation))) =
  not_yet_implemented __POS__ location

let translate_impl_definition
      (_definition_annotation : S.def_annot)
      (S.FCL_aux (_definition, (annot, _))) =
  not_yet_implemented __POS__ annot.loc

let translate_value_definition
      (_definition_annotation : S.def_annot)
      (S.LB_aux (_definition, (location, _value_def_annotation))) =
  not_yet_implemented __POS__ location

let translate_top_level_outcome_definition
      (_definition_annotation : S.def_annot)
      (S.OV_aux (_outcome, location))
      (_definitions : ('a S.def) list) =
  not_yet_implemented __POS__ location

let translate_definition (S.DEF_aux (def, annotation) as sail_definition) : (N.sail_definition * N.definition) =
  try
    let translation =
      match def with
      | DEF_fundef fd ->
         (
           match ir_fundef fd with
           | Some translation -> N.FunctionDefinition translation
           | None             -> not_yet_implemented __POS__ annotation.loc
         )
      | DEF_type type_definition  ->
         translate_type_definition annotation type_definition
      | DEF_mapdef definition ->
         translate_mapping_definition annotation definition
      | DEF_impl impl_definition ->
         translate_impl_definition annotation impl_definition
      | DEF_let let_definition ->
         translate_value_definition annotation let_definition
      | DEF_val value_specification ->
         translate_top_level_type_constraint annotation value_specification
      | DEF_outcome (outcome_spec, definitions) ->
         translate_top_level_outcome_definition annotation outcome_spec definitions
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
         translate_register annotation specification
      | DEF_internal_mutrec _ ->
         not_yet_implemented __POS__ annotation.loc
      | DEF_pragma (pragma, _argument, location) ->
         match pragma with
         | "include_start" -> N.IgnoredDefinition
         | "include_end"   -> N.IgnoredDefinition
         | "file_start"    -> N.IgnoredDefinition
         | "file_end"      -> N.IgnoredDefinition
         | _               -> not_yet_implemented __POS__ location
    in
    (sail_definition, translation)
  with NotYetImplemented (source_position, sail_location, message) ->
    let file, line_number, _, _ = source_position
    in
    (
      sail_definition,
      UntranslatedDefinition {
          filename = file;
          line_number = line_number;
          sail_location = sail_location;
          message = message
        }
    )

let sail_to_nanosail (ast : Libsail.Type_check.tannot Libsail.Ast_defs.ast) name : N.ir_t =
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
    function_definitions     = collect N.extract_function_definition;
    type_definitions         = collect N.extract_type_definition;
    enum_definitions         = collect N.extract_enum_definition;
    register_definitions     = collect N.extract_register_definition;
    untranslated_definitions = collect N.extract_untranslated_definition;
    ignored_definitions      = List.map fst (collect N.extract_ignored_definition);
  }
