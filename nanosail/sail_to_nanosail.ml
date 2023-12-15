open Nyi

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast


(******************************************************************************)

let translate_identifier (S.Id_aux (aux, location)) : N.identifier =
  match aux with
  | Id x       -> x
  | Operator x -> not_yet_implemented ~message:(Printf.sprintf "Operator %s" x) __POS__ location

(******************************************************************************)

let rec translate_numeric_expression (S.Nexp_aux (numeric_expression, numexp_location)) : N.numeric_expression =
  match numeric_expression with
  | Nexp_constant constant                     -> NE_constant constant
  | Nexp_times (x, y)                          -> NE_times (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_sum (x, y)                            -> NE_add (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_minus (x, y)                          -> NE_minus (translate_numeric_expression x, translate_numeric_expression y)
  | Nexp_neg x                                 -> NE_neg (translate_numeric_expression x)
  | Nexp_var (Kid_aux (Var string, _location)) -> NE_var string
  | Nexp_id identifier                         -> NE_id (translate_identifier identifier)
  | Nexp_exp _                                 -> not_yet_implemented __POS__ numexp_location
  | Nexp_app (_, _)                            -> not_yet_implemented __POS__ numexp_location

and translate_numeric_constraint (S.NC_aux (numeric_constraint, location)) =
  match numeric_constraint with
  | S.NC_equal (x, y)                          -> N.NC_equal (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_bounded_ge (x, y)                     -> N.NC_bounded_ge (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_bounded_gt (x, y)                     -> N.NC_bounded_gt (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_bounded_le (x, y)                     -> N.NC_bounded_le (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_bounded_lt (x, y)                     -> N.NC_bounded_lt (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_not_equal (x, y)                      -> N.NC_not_equal (translate_numeric_expression x, translate_numeric_expression y)
  | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> N.NC_set (kind_id, ns)
  | S.NC_or (x, y)                             -> N.NC_or (translate_numeric_constraint x, translate_numeric_constraint y)
  | S.NC_and (x, y)                            -> N.NC_and (translate_numeric_constraint x, translate_numeric_constraint y)
  | S.NC_var (Kid_aux (Var kind_id, _loc))     -> N.NC_var kind_id
  | S.NC_true                                  -> N.NC_true
  | S.NC_false                                 -> N.NC_false
  | S.NC_app (_, _)                            -> not_yet_implemented __POS__ location


let rec nanotype_of_sail_type (S.Typ_aux (typ, location)) =
  (*
    Types are representing as strings in Sail.
  *)
  let rec type_of_identifier identifier : N.nanotype =
    match translate_identifier identifier with
    | "bool"      -> Ty_bool
    | "int"       -> Ty_int
    | "unit"      -> Ty_unit
    | "string"    -> Ty_string
    | "atom"      -> Ty_atom
    | id          -> Ty_custom id

  (*
     Sail represents types with parameters with Typ_app (id, type_args).
     This function translates these to their corresponding nanotype.
  *)
  and translate_type_constructor
      (identifier     : S.id          )
      (type_arguments : S.typ_arg list) =
    let type_arguments' = List.map translate_type_argument type_arguments
    and identifier'     = translate_identifier identifier
    in
    match identifier', type_arguments' with
    | "list" , [ TA_type t ]  -> N.Ty_list t
    | id     , _              -> Ty_app (id, type_arguments')

  and translate_type_argument (S.A_aux (type_argument, location)) : N.type_argument =
    match type_argument with
    | A_nexp e -> TA_numexp (translate_numeric_expression e)
    | A_typ t  -> TA_type (nanotype_of_sail_type t)
    | A_bool _ -> not_yet_implemented __POS__ location
  in

  match typ with
  | Typ_internal_unknown            -> not_yet_implemented __POS__ location
  | Typ_var _                       -> not_yet_implemented __POS__ location
  | Typ_fn (_, _)                   -> not_yet_implemented __POS__ location
  | Typ_bidir (_, _)                -> not_yet_implemented __POS__ location
  | Typ_exist (_, _, _)             -> not_yet_implemented __POS__ location
  | Typ_id id                       -> type_of_identifier id
  | Typ_tuple items                 -> N.Ty_tuple (List.map nanotype_of_sail_type items)
  | Typ_app (identifier, type_args) -> translate_type_constructor identifier type_args



(******************************************************************************)

let ty_of_pexp (S.Pat_aux (aux, (location, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> nanotype_of_sail_type (Libsail.Type_check.typ_of exp)
  | Pat_when _       -> not_yet_implemented __POS__ location


(******************************************************************************)

let rec binds_of_pat (S.P_aux (aux, ((location, _annotation) as a))) =
  match aux with
  | P_lit (L_aux (lit, _)) ->
     begin
       match lit with
       | S.L_unit     -> [("()", N.Ty_unit)]
       | S.L_zero     -> not_yet_implemented __POS__ location
       | S.L_one      -> not_yet_implemented __POS__ location
       | S.L_true     -> not_yet_implemented __POS__ location
       | S.L_false    -> not_yet_implemented __POS__ location
       | S.L_num _    -> not_yet_implemented __POS__ location
       | S.L_hex _    -> not_yet_implemented __POS__ location
       | S.L_bin _    -> not_yet_implemented __POS__ location
       | S.L_string _ -> not_yet_implemented __POS__ location
       | S.L_undef    -> not_yet_implemented __POS__ location
       | S.L_real _   -> not_yet_implemented __POS__ location
     end
  | P_id id ->
      let x = translate_identifier id in
      let ty = nanotype_of_sail_type (Libsail.Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_tuple pats ->
      List.concat (List.map binds_of_pat pats)
  | S.P_wild                      -> not_yet_implemented __POS__ location
  | S.P_or (_, _)                 -> not_yet_implemented __POS__ location
  | S.P_not _                     -> not_yet_implemented __POS__ location
  | S.P_as (_, _)                 -> not_yet_implemented __POS__ location
  | S.P_typ (_, _)                -> not_yet_implemented __POS__ location
  | S.P_var (_, _)                -> not_yet_implemented __POS__ location
  | S.P_app (_, _)                -> not_yet_implemented __POS__ location
  | S.P_vector _                  -> not_yet_implemented __POS__ location
  | S.P_vector_concat _           -> not_yet_implemented __POS__ location
  | S.P_vector_subrange (_, _, _) -> not_yet_implemented __POS__ location
  | S.P_list _                    -> not_yet_implemented __POS__ location
  | S.P_cons (_, _)               -> not_yet_implemented __POS__ location
  | S.P_string_append _           -> not_yet_implemented __POS__ location
  | S.P_struct (_, _)             -> not_yet_implemented __POS__ location

let binds_of_pexp (S.Pat_aux (aux, (location, _annotation))) =
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ -> not_yet_implemented __POS__ location


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
  | AV_tuple elts ->
     begin
       match elts with
       | [] -> not_yet_implemented ~message:"Should not occur" __POS__ location
       | h::t ->
          let e_h = expression_of_aval location h in
          let f e1 aval2 =
            let e2 = expression_of_aval location aval2 in
            N.Exp_binop (Pair, e1, e2) in
          List.fold_left f e_h t
     end
  | AV_lit (lit, _)  -> N.Exp_val (value_of_lit lit)
  | AV_id (id, _)    -> N.Exp_var (translate_identifier id)
  | AV_list (lst, _) -> Exp_list (List.map (expression_of_aval location) lst)
  | AV_ref (_, _)    -> not_yet_implemented __POS__ location
  | AV_vector (_, _) -> not_yet_implemented __POS__ location
  | AV_record (_, _) -> not_yet_implemented __POS__ location
  | AV_cval (_, _)   -> not_yet_implemented __POS__ location


let make_sequence statements location =
  let rec aux statements =
    match statements with
    | []    -> not_yet_implemented ~message:"Should not happen" __POS__  location
    | [x]   -> x
    | x::xs -> N.Stm_seq (x, aux xs)
  in
  aux statements


let rec statement_of_aexp (S.AE_aux (aux, _, location)) =
  match aux with
  | AE_val aval ->
      N.Stm_exp (expression_of_aval location aval)
  | AE_app (id, avals, _) ->
     begin
       let x = translate_identifier id in
       match avals with
       | [aval1; aval2] when x = "sail_cons" ->
          let e1 = expression_of_aval location aval1 in
          let e2 = expression_of_aval location aval2 in
          Stm_exp (Exp_binop (Cons, e1, e2))
       | _ ->
          Stm_call (x, List.map (expression_of_aval location) avals)
     end
  | AE_let (_, id, _, aexp1, aexp2, _) ->
    let x = translate_identifier id in
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
  | S.AE_block (statements, last_statement, _type) ->
     let translated_statements = List.map statement_of_aexp (statements @ [last_statement])
     in
     make_sequence translated_statements location
  | S.AE_typ (_, _)              -> not_yet_implemented __POS__ location
  | S.AE_assign (_, _)           -> not_yet_implemented __POS__ location
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
  (*
      match matched {
        [||] => nil_clause,
        h :: t => cons_clause
      }
   *)
  | [ (AP_aux (AP_nil _, _, _), _, nil_clause);
      (AP_aux (AP_cons (
        AP_aux (AP_id (id_h, _), _, _),
        AP_aux (AP_id (id_t, _), _, _)
      ), _, _), _, cons_clause)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval location matched);
        alt_nil  = statement_of_aexp nil_clause;
        xh       = translate_identifier id_h;
        xt       = translate_identifier id_t;
        alt_cons = statement_of_aexp cons_clause;
        }
  (*
      match matched {
        h :: t => cons_clause
        [||] => nil_clause,
      }
   *)
  | [ (AP_aux (AP_cons (
        AP_aux (AP_id (id_h, _), _, _),
        AP_aux (AP_id (id_t, _), _, _)
      ), _, _), _, cons_clause);
      (AP_aux (AP_nil _, _, _), _, nil_clause)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval location matched);
        alt_nil  = statement_of_aexp nil_clause;
        xh       = translate_identifier id_h;
        xt       = translate_identifier id_t;
        alt_cons = statement_of_aexp cons_clause;
        }
  (*
      match matched {
        (id_l, id_r) => clause
      }
   *)
  | [ (AP_aux (AP_tuple [
        AP_aux (AP_id (id_l, _), _, _);
        AP_aux (AP_id (id_r, _), _, _);
      ], _, _),_ , clause)
    ] ->
      Stm_match_prod {
        s   = Stm_exp (expression_of_aval location matched);
        xl  = translate_identifier id_l;
        xr  = translate_identifier id_r;
        rhs = statement_of_aexp clause;
        }
  | _ -> not_yet_implemented __POS__ location

let body_of_pexp (S.Pat_aux (aux, (location, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (S.anf exp)
  | Pat_when _       -> not_yet_implemented __POS__ location


(******************************************************************************)

let ir_funcl (S.FCL_aux (S.FCL_funcl (id, pexp), _)) =
  {
    N.funName = translate_identifier(id);
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

let translate_kind (S.K_aux (kind, _location)) : Ast.kind =
  match kind with
  | S.K_type -> Ast.Kind_type
  | S.K_int  -> Ast.Kind_int
  | S.K_bool -> Ast.Kind_bool

let translate_kind_id (S.Kid_aux (Var kind_id, _id_loc)) : string =
  kind_id

let translate_type_quantifier_item (S.QI_aux (quantifier_item, location)) =
  match quantifier_item with
  | S.QI_id (KOpt_aux (KOpt_kind (kind, kind_id), _loc)) ->
    let kind'    = translate_kind kind
    and kind_id' = translate_kind_id kind_id
    in
    (kind_id', kind')
  | S.QI_constraint _ -> not_yet_implemented __POS__ location

let translate_type_quantifier (S.TypQ_aux (quantifier, _location)) =
  match quantifier with
  | S.TypQ_tq items  -> List.map translate_type_quantifier_item items
  | S.TypQ_no_forall -> []

let translate_type_abbreviation
      _definition_annotation
      _type_annotation
      (identifier : S.id)
      (quantifier : S.typquant)
      (S.A_aux (arg, _arg_location)) : N.definition =
  let quantifier' =
    translate_type_quantifier quantifier
  in
  let identifier' = translate_identifier identifier
  in
  let type_abbreviation =
    match arg with
    | A_nexp numeric_expression -> N.TA_numeric_expression (quantifier', translate_numeric_expression numeric_expression)
    | A_typ typ                 -> N.TA_alias (quantifier', nanotype_of_sail_type typ)
    | A_bool numeric_constraint -> N.TA_numeric_constraint (quantifier', translate_numeric_constraint numeric_constraint)
  in
  TypeDefinition (
    TD_abbreviation (identifier', type_abbreviation)
  )

let translate_enum
      (_definition_annotation : S.def_annot)
      (_type_annotation       : 'a S.annot )
      (identifier             : S.id       )
      (cases                  : S.id list  ) : N.definition
  =
  let identifier' = translate_identifier identifier
  and cases'      = List.map translate_identifier cases
  in
  EnumDefinition {
    identifier = identifier';
    cases      = cases'     ;
  }


let translate_variant
      (_definition_annotation : S.def_annot      )
      (identifier             : S.id             )
      (type_quantifier        : S.typquant       )
      (constructors           : S.type_union list)
      (_flag                  : bool             ) : N.definition
  =
  let identifier' = translate_identifier identifier
  in
  let type_quantifier' = translate_type_quantifier type_quantifier
  and constructors' =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      (translate_identifier identifier, nanotype_of_sail_type typ)
    in
    List.map translate_constructor constructors
  in
  VariantDefinition {
    identifier      = identifier'     ;
    type_quantifier = type_quantifier';
    constructors    = constructors'   ;
  }


let translate_type_definition (definition_annotation : S.def_annot) (S.TD_aux (type_definition, type_annotation)) : N.definition =
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg) ->
     translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_record (_, _, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc
  | TD_variant (identifier, type_quantifier, constructors, flag) ->
     translate_variant definition_annotation identifier type_quantifier constructors flag
  | TD_enum (identifier, cases, _) ->
     translate_enum definition_annotation type_annotation identifier cases
  | TD_bitfield (_, _, _) ->
     not_yet_implemented __POS__ definition_annotation.loc

let translate_top_level_type_constraint
      (_definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : N.definition =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          identifier, _extern) = value_specification
  in
  TopLevelTypeConstraintDefinition { identifier = translate_identifier identifier }

let translate_register
      (_definition_annotation : S.def_annot)
      (S.DEC_aux
         (DEC_reg (sail_type, identifier, expression),
          (_spec_location, _spec_annotation))) : N.definition
  =
  begin
    match expression with
    | None                                          -> ()
    | Some (E_aux (_expr, (location, _annotation))) -> not_yet_implemented __POS__ location
  end;
  let identifier' = translate_identifier identifier
  and nano_type   = nanotype_of_sail_type sail_type
  in
  RegisterDefinition {
    identifier = identifier';
    typ        = nano_type  ;
  }

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
         begin
           match ir_fundef fd with
           | Some translation -> N.FunctionDefinition translation
           | None             -> not_yet_implemented __POS__ annotation.loc
         end
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
         N.IgnoredDefinition
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
        begin
          match pragma with
          | "include_start" -> N.IgnoredDefinition
          | "include_end"   -> N.IgnoredDefinition
          | "file_start"    -> N.IgnoredDefinition
          | "file_end"      -> N.IgnoredDefinition
          | _               -> not_yet_implemented __POS__ location
        end
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
  and nano_definitions =
    List.map translate_definition ast.defs
  in
  let collect f =
    List.filter_map (lift f) nano_definitions
  in
  {
    program_name                          = name;
    function_definitions                  = collect N.extract_function_definition;
    top_level_type_constraint_definitions = collect N.extract_top_level_type_constraint_definition;
    type_definitions                      = collect N.extract_type_definition;
    enum_definitions                      = collect N.extract_enum_definition;
    variant_definitions                   = collect N.extract_variant_definition;
    register_definitions                  = collect N.extract_register_definition;
    untranslated_definitions              = collect N.extract_untranslated_definition;
    ignored_definitions                   = List.map fst (collect N.extract_ignored_definition);
  }
