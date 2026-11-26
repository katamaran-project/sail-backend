(*
   A Sail-to-Sail rewrite that folds conditionals in types whose condition is known.

   A type can depend on a condition, as in

     type physaddrbits_len : Int = if xlen == 32 then 34 else 64
     type pbits = bits(physaddrbits_len)

   Once --config fixes xlen (a target without abstract types requires this), the condition
   is constant, but Sail does not fold it: types keep forms such as bitvector(if 32 == 32 then 34 else 64)
   through all of Sail's rewrites. This rewrite replaces each such conditional by the branch
   its condition selects, in every type in the AST (type definitions, val specs, registers,
   function annotations, and the annotations in expressions and patterns), so that the type
   above becomes bitvector(34).

   A condition is known when, after expanding type synonyms, Sail's constraint_simp reduces it
   to true or false. Conditionals on type variables (if 'n == 32 then ...) are left alone.

   The rewrite changes only the syntax of the AST: recheck_defs must follow, so that the type
   environment and the type annotations agree with the folded types.
*)
open ExtBase

module S = Libsail.Ast
module Env = Libsail.Type_check.Env
module Rewriter = Libsail.Rewriter


(* Whether the condition is known to hold (Some true), known not to hold (Some false), or neither (None) *)
let decide
    (env       : Env.t           )
    (condition : S.n_constraint  ) : bool option
  =
  let S.NC_aux (simplified, _) = Libsail.Ast_util.constraint_simp (Env.expand_constraint_synonyms env condition)
  in
  match simplified with
  | NC_true                -> Some true
  | NC_false               -> Some false
  | NC_equal (_, _)        -> None
  | NC_not_equal (_, _)    -> None
  | NC_ge (_, _)           -> None
  | NC_gt (_, _)           -> None
  | NC_le (_, _)           -> None
  | NC_lt (_, _)           -> None
  | NC_set (_, _)          -> None
  | NC_or (_, _)           -> None
  | NC_and (_, _)          -> None
  | NC_app (_, _)          -> None
  | NC_id _                -> None
  | NC_var _               -> None


let rec fold_numeric_expression
    (env                : Env.t   )
    (numeric_expression : S.nexp  ) : S.nexp
  =
  let fold = fold_numeric_expression env
  in
  let S.Nexp_aux (unwrapped, location) = numeric_expression
  in
  let wrap (unwrapped : S.nexp_aux) : S.nexp =
    S.Nexp_aux (unwrapped, location)
  in
  match unwrapped with
  | Nexp_if (condition, then_branch, else_branch) -> begin
      let condition = fold_numeric_constraint env condition
      in
      match decide env condition with
      | Some true  -> fold then_branch
      | Some false -> fold else_branch
      | None       -> wrap @@ Nexp_if (condition, fold then_branch, fold else_branch)
    end
  | Nexp_id _                   -> numeric_expression
  | Nexp_var _                  -> numeric_expression
  | Nexp_constant _             -> numeric_expression
  | Nexp_app (identifier, args) -> wrap @@ Nexp_app (identifier, List.map args ~f:fold)
  | Nexp_times (left, right)    -> wrap @@ Nexp_times (fold left, fold right)
  | Nexp_sum (left, right)      -> wrap @@ Nexp_sum (fold left, fold right)
  | Nexp_minus (left, right)    -> wrap @@ Nexp_minus (fold left, fold right)
  | Nexp_exp exponent           -> wrap @@ Nexp_exp (fold exponent)
  | Nexp_neg negated            -> wrap @@ Nexp_neg (fold negated)


and fold_numeric_constraint
    (env                : Env.t           )
    (numeric_constraint : S.n_constraint  ) : S.n_constraint
  =
  let fold = fold_numeric_constraint env
  and fold_numeric = fold_numeric_expression env
  and fold_argument = fold_type_argument env
  in
  let S.NC_aux (unwrapped, location) = numeric_constraint
  in
  let wrap (unwrapped : S.n_constraint_aux) : S.n_constraint =
    S.NC_aux (unwrapped, location)
  in
  match unwrapped with
  | NC_equal (left, right)     -> wrap @@ NC_equal (fold_argument left, fold_argument right)
  | NC_not_equal (left, right) -> wrap @@ NC_not_equal (fold_argument left, fold_argument right)
  | NC_ge (left, right)        -> wrap @@ NC_ge (fold_numeric left, fold_numeric right)
  | NC_gt (left, right)        -> wrap @@ NC_gt (fold_numeric left, fold_numeric right)
  | NC_le (left, right)        -> wrap @@ NC_le (fold_numeric left, fold_numeric right)
  | NC_lt (left, right)        -> wrap @@ NC_lt (fold_numeric left, fold_numeric right)
  | NC_set (element, values)   -> wrap @@ NC_set (fold_numeric element, values)
  | NC_or (left, right)        -> wrap @@ NC_or (fold left, fold right)
  | NC_and (left, right)       -> wrap @@ NC_and (fold left, fold right)
  | NC_app (identifier, args)  -> wrap @@ NC_app (identifier, List.map args ~f:fold_argument)
  | NC_id _                    -> numeric_constraint
  | NC_var _                   -> numeric_constraint
  | NC_true                    -> numeric_constraint
  | NC_false                   -> numeric_constraint


and fold_type_argument
    (env           : Env.t      )
    (type_argument : S.typ_arg  ) : S.typ_arg
  =
  let S.A_aux (unwrapped, location) = type_argument
  in
  match unwrapped with
  | A_nexp numeric_expression -> S.A_aux (A_nexp (fold_numeric_expression env numeric_expression), location)
  | A_typ typ                 -> S.A_aux (A_typ (fold_type env typ), location)
  | A_bool numeric_constraint -> S.A_aux (A_bool (fold_numeric_constraint env numeric_constraint), location)


and fold_type
    (env : Env.t  )
    (typ : S.typ  ) : S.typ
  =
  let fold = fold_type env
  in
  let S.Typ_aux (unwrapped, location) = typ
  in
  let wrap (unwrapped : S.typ_aux) : S.typ =
    S.Typ_aux (unwrapped, location)
  in
  match unwrapped with
  | Typ_internal_unknown                      -> typ
  | Typ_id _                                  -> typ
  | Typ_var _                                 -> typ
  | Typ_fn (parameter_types, result_type)     -> wrap @@ Typ_fn (List.map parameter_types ~f:fold, fold result_type)
  | Typ_bidir (left, right)                   -> wrap @@ Typ_bidir (fold left, fold right)
  | Typ_tuple types                           -> wrap @@ Typ_tuple (List.map types ~f:fold)
  | Typ_app (identifier, type_arguments)      -> wrap @@ Typ_app (identifier, List.map type_arguments ~f:(fold_type_argument env))
  | Typ_exist (kinded_ids, constraint_, body) -> wrap @@ Typ_exist (kinded_ids, fold_numeric_constraint env constraint_, fold body)


let fold_type_quantifier
    (env             : Env.t        )
    (type_quantifier : S.typquant   ) : S.typquant
  =
  let fold_item (S.QI_aux (item, location) as quantifier_item : S.quant_item) : S.quant_item =
    match item with
    | QI_id _                          -> quantifier_item
    | QI_constraint numeric_constraint -> S.QI_aux (QI_constraint (fold_numeric_constraint env numeric_constraint), location)
  in
  List.map type_quantifier ~f:fold_item


let fold_type_scheme
    (env         : Env.t      )
    (type_scheme : S.typschm  ) : S.typschm
  =
  let S.TypSchm_aux (TypSchm_ts (type_quantifier, typ), location) = type_scheme
  in
  S.TypSchm_aux (TypSchm_ts (fold_type_quantifier env type_quantifier, fold_type env typ), location)


let fold_type_definition
    (env             : Env.t                                     )
    (type_definition : Libsail.Type_check.tannot S.type_def      ) : Libsail.Type_check.tannot S.type_def
  =
  let S.TD_aux (unwrapped, annotation) = type_definition
  in
  let fold_quantifier = fold_type_quantifier env
  and fold = fold_type env
  in
  let unwrapped : S.type_def_aux =
    match unwrapped with
    | TD_abbrev (identifier, type_quantifier, type_argument) ->
       TD_abbrev (identifier, fold_quantifier type_quantifier, fold_type_argument env type_argument)
    | TD_record (identifier, type_quantifier, fields, flag) ->
       let fold_field (((field_identifier, field_type), field_annotation) : (S.id * S.typ) * unit S.def_annot) =
         ((field_identifier, fold field_type), field_annotation)
       in
       TD_record (identifier, fold_quantifier type_quantifier, List.map fields ~f:fold_field, flag)
    | TD_variant (identifier, type_quantifier, constructors, flag) ->
       let fold_constructor (S.Tu_aux (Tu_ty_id (constructor_type, constructor_identifier), constructor_annotation) : S.type_union) =
         S.Tu_aux (Tu_ty_id (fold constructor_type, constructor_identifier), constructor_annotation)
       in
       TD_variant (identifier, fold_quantifier type_quantifier, List.map constructors ~f:fold_constructor, flag)
    | TD_abstract (_, _, _)    -> unwrapped
    | TD_enum (_, _, _)        -> unwrapped
    | TD_bitfield (_, _, _)    -> unwrapped
  in
  S.TD_aux (unwrapped, annotation)


(*
   The rewrite, with the signature of Sail's rewrites (Libsail.Rewrites.rewrite_sequence).
   env is the type environment of the whole model, in which the synonyms in conditions are expanded.
*)
let rewrite
    (context     : Libsail.Initial_check.ctx         )
    (effect_info : Libsail.Effects.side_effect_info  )
    (env         : Env.t                             )
    (ast         : Libsail.Type_check.typed_ast      ) : Libsail.Initial_check.ctx * Libsail.Type_check.typed_ast * Libsail.Effects.side_effect_info * Env.t
  =
  let fold = fold_type env
  in
  let pattern_algebra =
    { Rewriter.id_pat_alg with p_typ = (fun (typ, pattern) -> S.P_typ (fold typ, pattern)) }
  in
  let expression_algebra =
    {
      Rewriter.id_exp_alg with
      e_typ        = (fun (typ, expression) -> S.E_typ (fold typ, expression));
      e_sizeof     = (fun numeric_expression -> S.E_sizeof (fold_numeric_expression env numeric_expression));
      e_constraint = (fun numeric_constraint -> S.E_constraint (fold_numeric_constraint env numeric_constraint));
      le_typ       = (fun (typ, identifier) -> S.LE_typ (fold typ, identifier));
      pat_alg      = pattern_algebra;
    }
  in
  let rewrite_function
      (rewriters           : (Libsail.Type_check.tannot, Env.t) Rewriter.rewriters)
      (function_definition : Libsail.Type_check.tannot S.fundef                   ) : Libsail.Type_check.tannot S.fundef
    =
    let S.FD_aux (FD_function (recursion, type_annotation, clauses), annotation) = function_definition
    in
    let type_annotation =
      let S.Typ_annot_opt_aux (unwrapped, location) = type_annotation
      in
      match unwrapped with
      | Typ_annot_opt_some (type_quantifier, typ) -> S.Typ_annot_opt_aux (Typ_annot_opt_some (fold_type_quantifier env type_quantifier, fold typ), location)
      | Typ_annot_opt_none                        -> type_annotation
    in
    Rewriter.rewrite_fun rewriters (S.FD_aux (FD_function (recursion, type_annotation, clauses), annotation))
  in
  let rewrite_definition
      (rewriters  : (Libsail.Type_check.tannot, Env.t) Rewriter.rewriters)
      (definition : Libsail.Type_check.typed_def                         ) : Libsail.Type_check.typed_def
    =
    let S.DEF_aux (unwrapped, definition_annotation) = definition
    in
    let wrap (unwrapped : (Libsail.Type_check.tannot, Env.t) S.def_aux) : Libsail.Type_check.typed_def =
      S.DEF_aux (unwrapped, definition_annotation)
    in
    match unwrapped with
    | DEF_type type_definition ->
       wrap @@ DEF_type (fold_type_definition env type_definition)
    | DEF_val (VS_aux (VS_val_spec (type_scheme, identifier, extern), annotation)) ->
       wrap @@ DEF_val (VS_aux (VS_val_spec (fold_type_scheme env type_scheme, identifier, extern), annotation))
    | DEF_register (DEC_aux (DEC_reg (typ, identifier, initial_value), annotation)) ->
       let initial_value = Option.map initial_value ~f:(rewriters.rewrite_exp rewriters)
       in
       wrap @@ DEF_register (DEC_aux (DEC_reg (fold typ, identifier, initial_value), annotation))
    | DEF_constraint numeric_constraint ->
       wrap @@ DEF_constraint (fold_numeric_constraint env numeric_constraint)
    (* functions (through rewrite_function), values, and definitions without types of their own *)
    | DEF_fundef _                -> Rewriter.rewrite_def rewriters definition
    | DEF_internal_mutrec _       -> Rewriter.rewrite_def rewriters definition
    | DEF_let (_, _)              -> Rewriter.rewrite_def rewriters definition
    | DEF_impl _                  -> Rewriter.rewrite_def rewriters definition
    | DEF_mapdef _                -> Rewriter.rewrite_def rewriters definition
    | DEF_outcome (_, _)          -> Rewriter.rewrite_def rewriters definition
    | DEF_scattered _             -> Rewriter.rewrite_def rewriters definition
    | DEF_measure (_, _, _)       -> Rewriter.rewrite_def rewriters definition
    | DEF_loop_measures (_, _)    -> Rewriter.rewrite_def rewriters definition
    | DEF_instantiation (_, _)    -> Rewriter.rewrite_def rewriters definition
    | DEF_overload (_, _)         -> Rewriter.rewrite_def rewriters definition
    | DEF_fixity (_, _, _)        -> Rewriter.rewrite_def rewriters definition
    | DEF_default _               -> Rewriter.rewrite_def rewriters definition
    | DEF_pragma (_, _)           -> Rewriter.rewrite_def rewriters definition
  in
  let rewriters =
    {
      Rewriter.rewriters_base with
      rewrite_exp = (fun _ expression -> Rewriter.fold_exp expression_algebra expression);
      rewrite_pat = (fun _ pattern -> Rewriter.fold_pat pattern_algebra pattern);
      rewrite_fun = rewrite_function;
      rewrite_def = rewrite_definition;
    }
  in
  (context, Rewriter.rewrite_ast_base rewriters ast, effect_info, env)
