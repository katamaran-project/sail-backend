open Base

module Big_int = Nat_big_num

module Translation = Translation
module Sanitation = Sanitation

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)


let string_of_identifier (S.Id_aux (aux, _location)) : string =
  match aux with
  | Id x       -> x
  | Operator x -> x
  

let translate_identifier (S.Id_aux (aux, location)) : N.identifier TC.t =
  match aux with
  | Id x       -> TC.return x
  | Operator x -> TC.not_yet_implemented ~message:(Printf.sprintf "Operator %s" x) [%here] location


let rec translate_numeric_expression (S.Nexp_aux (numeric_expression, numexp_location)) : N.numeric_expression TC.t =
  match numeric_expression with
  | Nexp_constant constant                     -> TC.return @@ N.NE_constant constant
  | Nexp_var (Kid_aux (Var string, _location)) -> TC.return @@ N.NE_var string
  | Nexp_times (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_times (x', y')
    end
  | Nexp_sum (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_add (x', y')
    end
  | Nexp_minus (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_minus (x', y')
    end
  | Nexp_neg x  -> begin
      let* x' = translate_numeric_expression x
      in
      TC.return @@ N.NE_neg x'
    end
  | Nexp_id identifier -> begin
      let* identifier' = translate_identifier identifier
      in
      TC.return @@ N.NE_id identifier'
    end
  | Nexp_exp _      -> TC.not_yet_implemented [%here] numexp_location
  | Nexp_app (_, _) -> TC.not_yet_implemented [%here] numexp_location

and translate_numeric_constraint (S.NC_aux (numeric_constraint, location)) : N.numeric_constraint TC.t =
  match numeric_constraint with
  | S.NC_equal (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_equal (x', y')
    end
  | S.NC_bounded_ge (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_ge (x', y')
    end
  | S.NC_bounded_gt (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_gt (x', y')
    end
  | S.NC_bounded_le (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_le (x', y')
    end
  | S.NC_bounded_lt (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_lt (x', y')
    end
  | S.NC_not_equal (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_not_equal (x', y')
    end
  | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> TC.return @@ N.NC_set (kind_id, ns)
  | S.NC_or (x, y) -> begin
      let* x' = translate_numeric_constraint x
      and* y' = translate_numeric_constraint y
      in
      TC.return @@ N.NC_or (x', y')
    end
  | S.NC_and (x, y) -> begin
      let* x' = translate_numeric_constraint x
      and* y' = translate_numeric_constraint y
      in
      TC.return @@ N.NC_and (x', y')
    end
  | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ N.NC_var kind_id
  | S.NC_true                                  -> TC.return @@ N.NC_true
  | S.NC_false                                 -> TC.return @@ N.NC_false
  | S.NC_app (_, _)                            -> TC.not_yet_implemented [%here] location


let rec nanotype_of_sail_type (S.Typ_aux (typ, location)) : N.nanotype TC.t =
  (*
    Types are representing as strings in Sail.
  *)
  let rec type_of_identifier identifier : N.nanotype TC.t =
    let* identifier' = translate_identifier identifier
    in
    match identifier' with
    | "bool"      -> TC.return @@ N.Ty_bool
    | "nat"       -> TC.return @@ N.Ty_nat
    | "int"       -> TC.return @@ N.Ty_int
    | "unit"      -> TC.return @@ N.Ty_unit
    | "string"    -> TC.return @@ N.Ty_string
    | "atom"      -> TC.return @@ N.Ty_atom
    | id          -> TC.return @@ N.Ty_custom id

  (*
     Sail represents types with parameters with Typ_app (id, type_args).
     This function translates these to their corresponding nanotype.
  *)
  and translate_type_constructor
      (identifier     : S.id          )
      (type_arguments : S.typ_arg list) =
    let* type_arguments' = TC.map translate_type_argument type_arguments
    and* identifier'     = translate_identifier identifier
    in
    match identifier', type_arguments' with
    | "list" , [ N.TA_type t ]  -> TC.return @@ N.Ty_list t
    | id     , _                -> TC.return @@ N.Ty_app (id, type_arguments')

  and translate_type_argument (S.A_aux (type_argument, _location)) : N.type_argument TC.t =
    match type_argument with
    | A_nexp e -> begin
        let* e' = translate_numeric_expression e
        in
        TC.return @@ N.TA_numexp e'
      end
    | A_typ t  -> begin
        let* t' = nanotype_of_sail_type t
        in
        TC.return @@ N.TA_type t'
      end
    | A_bool b -> begin
        let* b' = translate_numeric_constraint b
        in
        TC.return @@ N.TA_bool b'
      end
  in

  match typ with
  | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
  | Typ_var _                       -> TC.not_yet_implemented [%here] location
  | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
  | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location
  | Typ_exist (_, _, _)             -> TC.not_yet_implemented [%here] location
  | Typ_id id                       -> type_of_identifier id
  | Typ_tuple items                 -> begin
      let* items' = TC.map nanotype_of_sail_type items
      in
      TC.return @@ N.Ty_tuple items'
    end
  | Typ_app (identifier, type_args) -> translate_type_constructor identifier type_args


let ty_of_pexp (S.Pat_aux (aux, (location, _annot))) =
  match aux with
  | Pat_exp (_, exp) -> nanotype_of_sail_type (Libsail.Type_check.typ_of exp)
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let rec binds_of_pat (S.P_aux (aux, ((location, _annotation) as a))) =
  match aux with
  | P_lit (L_aux (lit, _)) ->
     begin
       match lit with
       | S.L_unit     -> TC.return @@ [("()", N.Ty_unit)]
       | S.L_zero     -> TC.not_yet_implemented [%here] location
       | S.L_one      -> TC.not_yet_implemented [%here] location
       | S.L_true     -> TC.not_yet_implemented [%here] location
       | S.L_false    -> TC.not_yet_implemented [%here] location
       | S.L_num _    -> TC.not_yet_implemented [%here] location
       | S.L_hex _    -> TC.not_yet_implemented [%here] location
       | S.L_bin _    -> TC.not_yet_implemented [%here] location
       | S.L_string _ -> TC.not_yet_implemented [%here] location
       | S.L_undef    -> TC.not_yet_implemented [%here] location
       | S.L_real _   -> TC.not_yet_implemented [%here] location
     end
  | P_id id ->
      let* x  = translate_identifier id in
      let* ty = nanotype_of_sail_type (Libsail.Type_check.typ_of_annot a)
      in
      TC.return [(x, ty)]
  | P_tuple pats -> begin
      let* pats' = TC.map binds_of_pat pats
      in
      TC.return @@ List.concat pats'
    end
  | S.P_wild                      -> TC.not_yet_implemented [%here] location
  | S.P_or (_, _)                 -> TC.not_yet_implemented [%here] location
  | S.P_not _                     -> TC.not_yet_implemented [%here] location
  | S.P_as (_, _)                 -> TC.not_yet_implemented [%here] location
  | S.P_typ (_, _)                -> TC.not_yet_implemented [%here] location
  | S.P_var (_, _)                -> TC.not_yet_implemented [%here] location
  | S.P_app (_, _)                -> TC.not_yet_implemented [%here] location
  | S.P_vector _                  -> TC.not_yet_implemented [%here] location
  | S.P_vector_concat _           -> TC.not_yet_implemented [%here] location
  | S.P_vector_subrange (_, _, _) -> TC.not_yet_implemented [%here] location
  | S.P_list _                    -> TC.not_yet_implemented [%here] location
  | S.P_cons (_, _)               -> TC.not_yet_implemented [%here] location
  | S.P_string_append _           -> TC.not_yet_implemented [%here] location
  | S.P_struct (_, _)             -> TC.not_yet_implemented [%here] location


let binds_of_pexp (S.Pat_aux (aux, (location, _annotation))) =
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ -> TC.not_yet_implemented [%here] location


let value_of_lit (S.L_aux (literal, location)) =
  match literal with
  | L_true     -> TC.return @@ N.Val_bool true
  | L_false    -> TC.return @@ N.Val_bool false
  | L_num n    -> TC.return @@ N.Val_int n
  | L_unit     -> TC.return @@ N.Val_unit
  | L_string s -> TC.return @@ N.Val_string s
  | S.L_zero   -> TC.not_yet_implemented [%here] location
  | S.L_one    -> TC.not_yet_implemented [%here] location
  | S.L_hex _  -> TC.not_yet_implemented [%here] location
  | S.L_bin _  -> TC.not_yet_implemented [%here] location
  | S.L_undef  -> TC.not_yet_implemented [%here] location
  | S.L_real _ -> TC.not_yet_implemented [%here] location


let rec expression_of_aval location (value : S.typ S.aval) =
  match value with
  | AV_tuple elts ->
     begin
       match elts with
       | [] -> TC.not_yet_implemented ~message:"Should not occur" [%here] location
       | h::t -> begin
           let* e_h = expression_of_aval location h
           in
           let f e1 aval2 =
             let* e2 = expression_of_aval location aval2
             in
             TC.return @@ N.Exp_binop (Pair, e1, e2) in
           TC.fold_left f e_h t
         end
     end
  | AV_lit (lit, _)   -> begin
      let* lit' = value_of_lit lit
      in
      TC.return @@ N.Exp_val lit'
    end
  | AV_id (id, _lvar) -> begin
      let* id' = translate_identifier id
      in
      TC.return @@ N.Exp_var id'
    end
  | AV_list (lst, _)  -> begin
      let* lst' = TC.map (expression_of_aval location) lst
      in
      TC.return @@ N.Exp_list lst'
    end
  | AV_ref (_, _)     -> TC.not_yet_implemented [%here] location
  | AV_vector (_, _)  -> TC.not_yet_implemented [%here] location
  | AV_record (_, _)  -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)    -> TC.not_yet_implemented [%here] location


let make_sequence statements location =
  let rec aux statements =
    match statements with
    | []    -> TC.not_yet_implemented ~message:"Should not happen" [%here]  location
    | [x]   -> TC.return x
    | x::xs -> begin
        let* xs' = aux xs
        in
        TC.return @@ N.Stm_seq (x, xs')
      end
  in
  aux statements


let rec statement_of_aexp (expression : S.typ S.aexp)  =
  let S.AE_aux (aux, _environment, location) =  expression
  in
  match aux with
  | AE_val aval -> begin
      let* aval' = expression_of_aval location aval
      in
      TC.return @@ N.Stm_exp aval'
    end
    
  | AE_app (id, avals, _) -> begin
      let* id' = translate_identifier id
      in
      match avals with
      | [aval1; aval2] when String.equal id' "sail_cons" -> begin
          let* e1 = expression_of_aval location aval1
          and* e2 = expression_of_aval location aval2
          in
          TC.return @@ N.Stm_exp (Exp_binop (Cons, e1, e2))
        end
      | _ -> begin
          let* args = TC.map (expression_of_aval location) avals
          in
          TC.return @@ N.Stm_call (id', args)
        end
    end
     
  | AE_let (_, id, _, aexp1, aexp2, _) -> begin
      let* id' = translate_identifier id
      and* s1 = statement_of_aexp aexp1
      and* s2 = statement_of_aexp aexp2
      in
      TC.return @@ N.Stm_let (id', s1, s2)
    end
       
  | AE_if (aval, aexp1, aexp2, _) -> begin
      let* condition =
        let* e = expression_of_aval location aval  (* use lift *)
        in
        TC.return @@ N.Stm_exp e
      and* when_true = statement_of_aexp aexp1
      and* when_false = statement_of_aexp aexp2
      in
      TC.return @@ N.Stm_match (MP_bool { condition; when_true; when_false })
    end
       
  | AE_match (aval, cases, _) -> statement_of_match location aval cases
      
  | S.AE_block (statements, last_statement, _type) -> begin
      let* translated_statements = TC.map statement_of_aexp (statements @ [last_statement])
      in
      make_sequence translated_statements location
    end
    
  | S.AE_typ (_, _)              -> TC.not_yet_implemented [%here] location
  | S.AE_assign (_, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_return (_, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_exit (_, _)             -> TC.not_yet_implemented [%here] location
  | S.AE_throw (_, _)            -> TC.not_yet_implemented [%here] location
  | S.AE_field (_, _, _)         -> TC.not_yet_implemented [%here] location
  | S.AE_try (_, _, _)           -> TC.not_yet_implemented [%here] location
  | S.AE_struct_update (_, _, _) -> TC.not_yet_implemented [%here] location
  | S.AE_for (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
  | S.AE_loop (_, _, _)          -> TC.not_yet_implemented [%here] location
  | S.AE_short_circuit (_, _, _) -> TC.not_yet_implemented [%here] location

and statement_of_match (location : S.l                                              )
                       (matched  : S.typ S.aval                                     )
                       (cases    : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list)
  =
  match matched with
  | S.AV_id (_id, aval) -> begin
      begin
        match aval with
        | S.Ast_util.Local (_mut, _typ) -> begin
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
              ] -> begin
                let* matched = TC.lift (fun x -> N.Stm_exp x) @@ expression_of_aval location matched
                and* when_nil = statement_of_aexp nil_clause
                and* when_cons =
                  let* id_head = translate_identifier id_h
                  and* id_tail = translate_identifier id_t
                  and* clause = statement_of_aexp cons_clause
                  in
                  TC.return (id_head, id_tail, clause)
                in
                let match_pattern =
                  N.MP_list {
                    matched;
                    when_cons;
                    when_nil;
                  }
                in
                TC.return @@ N.Stm_match match_pattern
              end
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
              ] -> begin
                let* matched = let* expr = expression_of_aval location matched in TC.return @@ N.Stm_exp expr (* TODO use lift *)
                and* when_nil = statement_of_aexp nil_clause
                and* when_cons =
                  let* id_head = translate_identifier id_h
                  and* id_tail = translate_identifier id_t
                  and* clause = statement_of_aexp cons_clause
                  in
                  TC.return (id_head, id_tail, clause)
                in
                let match_pattern =
                  N.MP_list {
                    matched;
                    when_cons;
                    when_nil;
                  }
                in
                TC.return @@ N.Stm_match match_pattern
              end
           (*
               match matched {
                 (id_l, id_r) => clause
               }
           *)
            | [ (AP_aux (AP_tuple [
                AP_aux (AP_id (id_l, _), _, _);
                AP_aux (AP_id (id_r, _), _, _);
              ], _, _),_ , clause)
              ] -> begin
                let* matched = let* expr = expression_of_aval location matched in TC.return @@ N.Stm_exp expr (* use lift *)
                and* id_fst = translate_identifier id_l
                and* id_snd = translate_identifier id_r
                and* body = statement_of_aexp clause
                in
                TC.return @@ N.Stm_match (N.MP_product { matched; id_fst; id_snd; body })
              end
            | _ -> TC.not_yet_implemented [%here] location
          end
        | S.Ast_util.Register _ -> TC.not_yet_implemented [%here] location
        | S.Ast_util.Enum _     -> TC.not_yet_implemented [%here] location
        | S.Ast_util.Unbound _  -> TC.not_yet_implemented [%here] location
      end
    end
  | S.AV_lit (_, _)    -> TC.not_yet_implemented [%here] location
  | S.AV_ref (_, _)    -> TC.not_yet_implemented [%here] location
  | S.AV_tuple _       -> TC.not_yet_implemented [%here] location
  | S.AV_list (_, _)   -> TC.not_yet_implemented [%here] location
  | S.AV_vector (_, _) -> TC.not_yet_implemented [%here] location
  | S.AV_record (_, _) -> TC.not_yet_implemented [%here] location
  | S.AV_cval (_, _)   -> TC.not_yet_implemented [%here] location



let body_of_pexp pexp =
  let S.Pat_aux (aux, (location, _annot)) = pexp
  in
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (S.anf exp)
  | Pat_when _       -> TC.not_yet_implemented [%here] location


let translate_function_definition
      (definition_annotation : S.def_annot               )
      (function_definition   : N.type_annotation S.fundef)
  =
  let S.FD_aux ((FD_function (_, _, funcls)), _) = function_definition
  in
  match funcls with
  | [funcl] -> begin
      let S.FCL_aux (S.FCL_funcl (id, pexp), (_def_annot, _type_annotation)) = funcl
      in
      let* function_name = translate_identifier id
      and* arg_types = binds_of_pexp pexp
      and* ret_type = ty_of_pexp pexp
      and* function_body = body_of_pexp pexp
      in
      TC.return @@ N.FunctionDefinition {
        function_name;
        function_type = {
            arg_types;
            ret_type;
          };
        function_body;
      }
    end
  | _ -> TC.not_yet_implemented [%here] definition_annotation.loc


let translate_kind (S.K_aux (kind, _location)) : Ast.kind TC.t =
  match kind with
  | S.K_type -> TC.return @@ Ast.Kind_type
  | S.K_int  -> TC.return @@ Ast.Kind_int
  | S.K_bool -> TC.return @@ Ast.Kind_bool


let translate_kind_id (S.Kid_aux (Var kind_id, _id_loc)) : string TC.t =
  TC.return @@ kind_id


let translate_type_quantifier_item (S.QI_aux (quantifier_item, location)) =
  match quantifier_item with
  | S.QI_id (KOpt_aux (KOpt_kind (kind, kind_id), _loc)) ->
    let* kind'    = translate_kind kind
    and* kind_id' = translate_kind_id kind_id
    in
    TC.return @@ (kind_id', kind')
  | S.QI_constraint _ -> TC.not_yet_implemented [%here] location


let translate_type_quantifier (S.TypQ_aux (quantifier, _location)) =
  match quantifier with
  | S.TypQ_tq items  -> TC.map translate_type_quantifier_item items
  | S.TypQ_no_forall -> TC.return @@ []


let translate_type_abbreviation
      _definition_annotation
      _type_annotation
      (identifier : S.id)
      (quantifier : S.typquant)
      (S.A_aux (arg, _arg_location)) : N.definition TC.t =
  let* quantifier' = translate_type_quantifier quantifier
  and* identifier' = translate_identifier identifier
  in
  let* type_abbreviation =
    match arg with
    | A_nexp numeric_expression -> begin
        let* numeric_expression' = translate_numeric_expression numeric_expression
        in          
        TC.return @@ N.TA_numeric_expression (quantifier', numeric_expression')
      end
    | A_typ typ -> begin
        let* typ' = nanotype_of_sail_type typ
        in
        TC.return @@ N.TA_alias (quantifier', typ')
      end
    | A_bool numeric_constraint -> begin
        let* numeric_constraint' = translate_numeric_constraint numeric_constraint
        in
        TC.return @@ N.TA_numeric_constraint (quantifier', numeric_constraint')
      end
  in
  TC.return @@ N.TypeDefinition (
    N.TD_abbreviation { identifier = identifier'; abbreviation = type_abbreviation }
  )


let translate_enum
      (_definition_annotation : S.def_annot)
      (_type_annotation       : 'a S.annot )
      (identifier             : S.id       )
      (cases                  : S.id list  ) : N.definition TC.t
  =
  let* identifier' = translate_identifier identifier
  and* cases'      = TC.map translate_identifier cases
  in
  TC.return @@ N.TypeDefinition (
    N.TD_enum {
      identifier = identifier';
      cases      = cases'     ;
    }
  )


let translate_variant
      (_definition_annotation : S.def_annot      )
      (identifier             : S.id             )
      (type_quantifier        : S.typquant       )
      (constructors           : S.type_union list)
      (_flag                  : bool             ) : N.definition TC.t
  =
  let* identifier' = translate_identifier identifier
  and* type_quantifier' = translate_type_quantifier type_quantifier
  and* constructors' =
    let translate_constructor (S.Tu_aux (Tu_ty_id (typ, identifier), _annotation)) =
      let* identifier' = translate_identifier identifier
      and* typ' = nanotype_of_sail_type typ
      in
      TC.return @@ (identifier', typ')
    in
    TC.map translate_constructor constructors
  in
  TC.return @@ N.TypeDefinition (
    N.TD_variant {
      identifier      = identifier'     ;
      type_quantifier = type_quantifier';
      constructors    = constructors'   ;
    }
  )


let translate_type_definition
      (definition_annotation     : S.def_annot                 )
      (annotated_type_definition : N.type_annotation S.type_def) : N.definition TC.t
  =
  let S.TD_aux (type_definition, type_annotation) = annotated_type_definition
  in
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg)                      -> translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_variant (identifier, type_quantifier, constructors, flag) -> translate_variant definition_annotation identifier type_quantifier constructors flag
  | TD_enum (identifier, cases, _)                               -> translate_enum definition_annotation type_annotation identifier cases
  | TD_record (_, _, _, _)                                       -> TC.not_yet_implemented [%here] definition_annotation.loc
  | TD_bitfield (_, _, _)                                        -> TC.not_yet_implemented [%here] definition_annotation.loc

let translate_top_level_type_constraint
      (_definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : N.definition TC.t =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          identifier, _extern) = value_specification
  in
  let* identifier' = translate_identifier identifier
  in
  TC.return @@ N.TopLevelTypeConstraintDefinition { identifier = identifier' }

let translate_register
      (_definition_annotation        : S.def_annot                 )
      (annotated_register_definition : N.type_annotation S.dec_spec) : N.definition TC.t
  =
  let (S.DEC_aux (DEC_reg (sail_type, identifier, expression), (_spec_location, _spec_annotation))) = annotated_register_definition
  in
  match expression with
  | None -> begin
      let* identifier' = translate_identifier identifier
      and* nanotype    = nanotype_of_sail_type sail_type
      in
      TC.return @@ N.RegisterDefinition {
          identifier = identifier';
          typ        = nanotype   ;
        }
    end
  | Some (E_aux (_expr, (location, _annotation))) -> TC.not_yet_implemented [%here] location


let translate_mapping_definition
      (_definition_annotation : S.def_annot)
      (S.MD_aux (_definition, (location, _mapping_annotation))) =
  TC.not_yet_implemented [%here] location

let translate_impl_definition
      (_definition_annotation : S.def_annot)
      (S.FCL_aux (_definition, (annot, _))) =
  TC.not_yet_implemented [%here] annot.loc

let translate_value_definition
      (_definition_annotation : S.def_annot)
      (S.LB_aux (_definition, (location, _value_def_annotation))) =
  TC.not_yet_implemented [%here] location

let translate_top_level_outcome_definition
      (_definition_annotation : S.def_annot)
      (S.OV_aux (_outcome, location))
      (_definitions : ('a S.def) list) =
  TC.not_yet_implemented [%here] location

let translate_definition (S.DEF_aux (def, annotation) as sail_definition) : (N.sail_definition * N.definition) TC.t =
  if
    Configuration.ignore_definition sail_definition
  then
    TC.return (sail_definition, N.IgnoredDefinition)
  else begin
    let translation =
      let* result =
        match def with
        | DEF_type type_definition                 -> translate_type_definition annotation type_definition
        | DEF_mapdef definition                    -> translate_mapping_definition annotation definition
        | DEF_impl impl_definition                 -> translate_impl_definition annotation impl_definition
        | DEF_let let_definition                   -> translate_value_definition annotation let_definition
        | DEF_val value_specification              -> translate_top_level_type_constraint annotation value_specification
        | DEF_outcome (outcome_spec, definitions)  -> translate_top_level_outcome_definition annotation outcome_spec definitions
        | DEF_instantiation (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_fixity (_, _, _)                     -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_overload (_, _)                      -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_default _                            -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_scattered _                          -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_measure (_, _, _)                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_loop_measures (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_register specification               -> translate_register annotation specification
        | DEF_internal_mutrec _                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_pragma (pragma, _argument, location) -> TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] location
        | DEF_fundef function_definition           -> translate_function_definition annotation function_definition
      in
      TC.return (sail_definition, result)
    in
    TC.recover translation begin fun error ->
      match error with
      | TC.NotYetImplemented (ocaml_location, sail_location, message) -> begin
          let untranslated_definition = N.UntranslatedDefinition {
              filename = ocaml_location.pos_fname;
              line_number = ocaml_location.pos_lnum;
              sail_location = sail_location;
              message = message
            }
          in
          TC.return (sail_definition, untranslated_definition)
        end
    end
  end

let translate (ast : Libsail.Type_check.tannot Libsail.Ast_defs.ast) name : N.program =
  let (result, _context) = TC.run @@ TC.map translate_definition ast.defs
  in
  match result with
  | TC.Success definitions -> { program_name = name; definitions  = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"

