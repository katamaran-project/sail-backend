open Option
open Ast
open Libsail.Ast
open Libsail.Ast_defs
open Libsail.Anf
open Libsail

module Big_int = Nat_big_num


exception NotYetImplemented of l * string


(******************************************************************************)

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | _ -> " NOT YET SUPPORTED "

let string_of_position (position : Lexing.position) =
  match position with
  | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
     Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

let rec string_of_location (location : l) =
  match location with
  | Unknown -> "UnknownLocation"
  | Unique (k, loc) ->
     Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
  | Generated loc ->
     Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
  | Hint (hint, loc1, loc2) ->
     Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
  | Range (pos1, pos2) ->
     Printf.sprintf "Range(%s-%s)" (string_of_position pos1) (string_of_position pos2)

let not_yet_supported (location : l) (message : string) =
  Printf.printf "Not yet supported: %s at location %s\n" message (string_of_location location)

(******************************************************************************)

let ty_id_of_typ_id (Id_aux (aux, _)) =
  match aux with
  | Id "bool"   -> Bool
  | Id "int"    -> Int
  | Id "list"   -> List
  | Id "prod"   -> Prod
  | Id "unit"   -> Unit
  | Id "string" -> String
  | _ -> Id_nys

let rec ty_of_typ (Typ_aux (typ, _)) =
  let ty_of_arg (A_aux (aux, _)) =
    match aux with
    | A_typ typ -> ty_of_typ typ
    | _ -> Ty_nys in
  match typ with
  | Typ_id id          -> Ty_id (ty_id_of_typ_id id)
  | Typ_app (id, args) -> (
      match string_of_id id with
      | "atom"      -> Ty_id Int
      | "atom_bool" -> Ty_id Bool
      | _           -> Ty_app (ty_id_of_typ_id id, List.map ty_of_arg args)
    )
  | Typ_tuple (h_typ :: typs) ->
      let h_ty = ty_of_typ h_typ in
      let f ty1 typ2 =
        let ty2 = ty_of_typ typ2 in
        Ty_app (Prod, [ty1; ty2]) in
      List.fold_left f h_ty typs
  | _ -> Ty_nys


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
  | [funcl] -> some (ir_funcl funcl)
  | _       -> none

let translate_type_abbreviation _definition_annotation _type_annotation (Id_aux (identifier, identifier_location)) quantifier type_arg : definition =
  let TypQ_aux (quantifier, quantifier_location) = quantifier
  and A_aux (arg, arg_location) = type_arg
  in
  let translate_numeric_expression (Nexp_aux (numeric_expression, numexp_location)) =
    match quantifier with
    | TypQ_tq _ -> raise (NotYetImplemented (quantifier_location, "TypQ_tq"))
    | TypQ_no_forall ->
       (
         match numeric_expression with
         | Nexp_id _ -> raise (NotYetImplemented (numexp_location, "Nexp_id"))
         | Nexp_var _ -> raise (NotYetImplemented (numexp_location, "Nexp_var"))
         | Nexp_constant constant ->
            (
              match identifier with
              | Id id -> TypeDefinition (TD_abbreviation (id, TA_numeric_expression (Nexp_constant constant)))
              | Operator _ -> raise (NotYetImplemented (identifier_location, "Operator"))
            )
            
         | Nexp_app (_, _) -> raise (NotYetImplemented (numexp_location, "Nexp_app"))
         | Nexp_times (_, _) -> raise (NotYetImplemented (numexp_location, "Nexp_times"))
         | Nexp_sum (_, _) -> raise (NotYetImplemented (numexp_location, "Nexp_sum"))
         | Nexp_minus (_, _) -> raise (NotYetImplemented (numexp_location, "Nexp_minus"))
         | Nexp_exp _ -> raise (NotYetImplemented (numexp_location, "Nexp_exp"))
         | Nexp_neg _ -> raise (NotYetImplemented (numexp_location, "Nexp_neg"))
       )
  in
  match arg with
  | A_nexp numeric_expression ->
     translate_numeric_expression numeric_expression
  | A_typ _ -> raise (NotYetImplemented (arg_location, "A_typ"))
  | A_bool _ -> raise (NotYetImplemented (arg_location, "A_bool"))

let translate_type_definition (definition_annotation : def_annot) (TD_aux (type_definition, type_annotation)) : definition =
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg) ->
     translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_record (_, _, _, _) ->
     raise (NotYetImplemented (definition_annotation.loc, "TD_record"))
  | TD_variant (_, _, _, _) ->
     raise (NotYetImplemented (definition_annotation.loc, "TD_variant"))
  | TD_enum (_, _, _) ->
     raise (NotYetImplemented (definition_annotation.loc, "TD_enum"))
  | TD_bitfield (_, _, _) ->
     raise (NotYetImplemented (definition_annotation.loc, "TD_bitfield"))

let translate_top_level_constant (definition_annotation : def_annot) (VS_aux (value_specification, _vspec_annotation)) : definition =
  let VS_val_spec (TypSchm_aux (TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)), _type_scheme_location), identifier, _extern) = value_specification
  in
  raise (NotYetImplemented (definition_annotation.loc, "top level constant " ^ string_of_id identifier))

let translate_register (definition_annotation : def_annot) (DEC_aux (_x, _y)) : definition =
  raise (NotYetImplemented (definition_annotation.loc, "register"))

let translate_definition (DEF_aux (def, annotation) as sail_definition) : definition =
  try
    match def with
    | DEF_fundef fd ->
       (
         match ir_fundef fd with
         | Some translation -> FunctionDefinition translation
         | None             -> UntranslatedDefinition sail_definition
       )
    | DEF_type type_definition  -> translate_type_definition annotation type_definition
    | DEF_mapdef _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_mapdef"))
    | DEF_impl _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_impl"))
    | DEF_let _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_let"))
    | DEF_val value_specification ->
      translate_top_level_constant annotation value_specification
    | DEF_outcome (_, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_outcome"))
    | DEF_instantiation (_, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_instantiation"))
    | DEF_fixity (_, _, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_fixity"))
    | DEF_overload (_, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_overload"))
    | DEF_default _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_default"))
    | DEF_scattered _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_scattered"))
    | DEF_measure (_, _, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_measure"))
    | DEF_loop_measures (_, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_loop"))
    | DEF_register specification ->
      translate_register annotation specification
    | DEF_internal_mutrec _ ->
       raise (NotYetImplemented (annotation.loc, "DEF_internal"))
    | DEF_pragma (_, _, _) ->
       raise (NotYetImplemented (annotation.loc, "DEF_pragma"))
  with NotYetImplemented (location, message) ->
        not_yet_supported location message;
        UntranslatedDefinition sail_definition

let sail_to_nanosail ast name =
  let sail_definitions = ast.defs in
  let translated_definitions = List.map translate_definition sail_definitions in
  {
    program_name             = name;
    function_definitions     = List.filter_map extract_function_definition translated_definitions;
    type_definitions         = List.filter_map extract_type_definition translated_definitions;
    untranslated_definitions = []
  }
