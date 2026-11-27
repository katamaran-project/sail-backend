open ExtBase


(*
   muSail functions take named parameters, but a Sail function can take its arguments apart in its parameter list:

     function rX_bits Regidx(i) : regidx = rX(Regno(unsigned(i)))

   A parameter that is not a plain name becomes a fresh parameter arg#N, and the body starts by matching it
   against the original pattern:

     function rX_bits arg#0 : regidx = match arg#0 { Regidx(i) => rX(Regno(unsigned(i))) }

   Sail's Lean backend does the same when it prints a function (add_function_pattern in pretty_print_lean.ml).
   The match is translated like any other: here, a match on a union with one constructor.
*)


module S = struct
  include Libsail.Ast
  include Libsail.Ast_util
end

module TypeCheck = Libsail.Type_check


(* A parameter that was replaced by a fresh name, and the pattern the body matches it against *)
type demoted_parameter =
  {
    identifier : S.id;
    typ        : S.typ;
    pattern    : TypeCheck.tannot S.pat;
  }


(*
   Whether the pattern binds a parameter as it is, which the translation supports directly:
   a name, a wildcard or (), possibly with a type annotation.
*)
let rec is_plain_parameter (pattern : TypeCheck.tannot S.pat) : bool =
  let S.P_aux (unwrapped_pattern, _annotation) = pattern
  in
  match unwrapped_pattern with
  | P_id _                     -> true
  | P_wild                     -> true
  | P_typ (_, inner_pattern)   -> is_plain_parameter inner_pattern
  | P_lit (L_aux (literal, _)) -> begin
      match literal with
      | L_unit     -> true
      | L_true     -> false
      | L_false    -> false
      | L_num _    -> false
      | L_hex _    -> false
      | L_bin _    -> false
      | L_string _ -> false
      | L_real _   -> false
    end
  | P_or (_, _)                 -> false
  | P_not _                     -> false
  | P_as (_, _)                 -> false
  | P_var (_, _)                -> false
  | P_app (_, _)                -> false
  | P_vector _                  -> false
  | P_vector_concat _           -> false
  | P_vector_subrange (_, _, _) -> false
  | P_tuple _                   -> false
  | P_list _                    -> false
  | P_cons (_, _)               -> false
  | P_string_append _           -> false
  | P_struct (_, _, _)          -> false


let demoted_parameter_identifier (index : int) : S.id =
  S.mk_id @@ Printf.sprintf "arg#%d" index


(*
   Replaces each parameter that is not plain by a fresh name; returns the new parameter pattern and the replaced parameters.

   Several parameters form a tuple pattern, (Regidx(i) : regidx, data : xlenbits), whose elements are replaced separately;
   a single parameter is replaced as a whole.
*)
let demote_parameters (parameter_bindings : TypeCheck.tannot S.pat) : TypeCheck.tannot S.pat * demoted_parameter list =
  let demote
      (index   : int                    )
      (pattern : TypeCheck.tannot S.pat ) : int * (TypeCheck.tannot S.pat * demoted_parameter option)
    =
    if
      is_plain_parameter pattern
    then
      (index, (pattern, None))
    else begin
      let S.P_aux (_, annotation) = pattern
      and identifier              = demoted_parameter_identifier index
      in
      let demoted = { identifier; typ = TypeCheck.typ_of_pat pattern; pattern }
      in
      (index + 1, (S.P_aux (P_id identifier, annotation), Some demoted))
    end
  in
  let demote_elements (elements : TypeCheck.tannot S.pat list) : TypeCheck.tannot S.pat list * demoted_parameter list =
    let _, results = List.fold_map elements ~init:0 ~f:demote
    in
    (List.map ~f:fst results, List.filter_map ~f:snd results)
  in
  let rec demote_top_level (pattern : TypeCheck.tannot S.pat) : TypeCheck.tannot S.pat * demoted_parameter list =
    let S.P_aux (unwrapped_pattern, annotation) = pattern
    in
    match unwrapped_pattern with
    | P_tuple elements -> begin
        let elements', demoted = demote_elements elements
        in
        (S.P_aux (P_tuple elements', annotation), demoted)
      end
    | P_typ (typ, (S.P_aux (P_tuple _, _) as inner_pattern)) -> begin
        let inner_pattern', demoted = demote_top_level inner_pattern
        in
        (S.P_aux (P_typ (typ, inner_pattern'), annotation), demoted)
      end
    | P_typ (_, _)                -> demote_single pattern
    | P_id _                      -> demote_single pattern
    | P_wild                      -> demote_single pattern
    | P_lit _                     -> demote_single pattern
    | P_or (_, _)                 -> demote_single pattern
    | P_not _                     -> demote_single pattern
    | P_as (_, _)                 -> demote_single pattern
    | P_var (_, _)                -> demote_single pattern
    | P_app (_, _)                -> demote_single pattern
    | P_vector _                  -> demote_single pattern
    | P_vector_concat _           -> demote_single pattern
    | P_vector_subrange (_, _, _) -> demote_single pattern
    | P_list _                    -> demote_single pattern
    | P_cons (_, _)               -> demote_single pattern
    | P_string_append _           -> demote_single pattern
    | P_struct (_, _, _)          -> demote_single pattern
  and demote_single (pattern : TypeCheck.tannot S.pat) : TypeCheck.tannot S.pat * demoted_parameter list =
    let _, (pattern', demoted) = demote 0 pattern
    in
    (pattern', Option.to_list demoted)
  in
  demote_top_level parameter_bindings


(*
   Wraps the body in a match of each replaced parameter against its pattern, the first parameter outermost:

     match arg#0 { pattern0 => match arg#1 { pattern1 => body } }

   The fresh name is added to the environment of its own occurrence, so that the translation sees a local variable.
*)
let match_demoted_parameters
    (demoted : demoted_parameter list  )
    (body    : TypeCheck.tannot S.exp  ) : TypeCheck.tannot S.exp
  =
  let match_parameter
      (parameter : demoted_parameter     )
      (body      : TypeCheck.tannot S.exp) : TypeCheck.tannot S.exp
    =
    let location         = S.exp_loc body
    and body_type        = TypeCheck.typ_of body
    and body_environment = TypeCheck.env_of body
    in
    let scrutinee_environment = TypeCheck.Env.add_local parameter.identifier (S.Immutable, parameter.typ) body_environment
    in
    let scrutinee =
      S.E_aux (E_id parameter.identifier, (location, TypeCheck.mk_tannot scrutinee_environment parameter.typ))
    and annotation =
      (location, TypeCheck.mk_tannot body_environment body_type)
    in
    S.E_aux (E_match (scrutinee, [ S.Pat_aux (Pat_exp (parameter.pattern, body), annotation) ]), annotation)
  in
  List.fold_right demoted ~init:body ~f:match_parameter
