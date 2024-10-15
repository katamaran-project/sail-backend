module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC        = TranslationContext
module Bindings  = Libsail.Ast_util.Bindings
module StringMap = Map.String

open Base
open Monads.Notations.Star(TC)


(* todo helper function that reads out identifier and takes into account the provenance (local vs register) *)

let type_from_lvar
      (lvar : S.typ S.Ast_util.lvar)
      (loc  : S.l                  ) : S.typ TC.t
  =
  match lvar with
  | S.Ast_util.Register t   -> TC.return t
  | S.Ast_util.Enum t       -> TC.return t
  | S.Ast_util.Local (_, t) -> TC.return t
  | S.Ast_util.Unbound _    -> TC.not_yet_implemented [%here] loc


let create_if_statement
      ~(condition  : Ast.Statement.t)
      ~(when_true  : Ast.Statement.t)
      ~(when_false : Ast.Statement.t) : Ast.Statement.t
  =
  Ast.Statement.Match (MatchBool { condition; when_true; when_false })


let statement_of_lvar
      (identifier : Ast.Identifier.t        )
      (lvar       : S.typ S.Ast_util.lvar   )
      (location   : S.l                     ) : Ast.Statement.t TC.t
  =
  match lvar with
  | Register _   -> TC.return @@ Ast.Statement.ReadRegister identifier
  | Local (_, _) -> TC.return @@ Ast.Statement.Expression (Ast.Expression.Variable identifier)
  | Enum _       -> TC.not_yet_implemented [%here] location
  | Unbound _    -> TC.not_yet_implemented [%here] location


let translate_return_type (sail_type : Libsail.Ast.typ) : Ast.Type.t TC.t =
  Nanotype.nanotype_of_sail_type sail_type


let rec translate_parameter_bindings (pattern : Libsail.Type_check.tannot S.pat) : (Ast.Identifier.t * Ast.Type.t) list TC.t  =
  let S.P_aux (unwrapped_pattern, ((location, _annotation) as annotation)) = pattern
  in
  match unwrapped_pattern with
  | P_lit (L_aux (lit, _loc)) ->
     begin
       match lit with
       | L_unit     -> TC.return @@ [(Ast.Identifier.mk "()", Ast.Type.Unit)] (* todo rather ugly *)
       | L_zero     -> TC.not_yet_implemented [%here] location
       | L_one      -> TC.not_yet_implemented [%here] location
       | L_true     -> TC.not_yet_implemented [%here] location
       | L_false    -> TC.not_yet_implemented [%here] location
       | L_num _    -> TC.not_yet_implemented [%here] location
       | L_hex _    -> TC.not_yet_implemented [%here] location
       | L_bin _    -> TC.not_yet_implemented [%here] location
       | L_string _ -> TC.not_yet_implemented [%here] location
       | L_undef    -> TC.not_yet_implemented [%here] location
       | L_real _   -> TC.not_yet_implemented [%here] location
     end
  | P_id id -> begin
      let* x  = Identifier.translate_identifier [%here] id in
      let* ty = Nanotype.nanotype_of_sail_type @@ Libsail.Type_check.typ_of_annot annotation
      in
      TC.return [(x, ty)]
    end
  | P_tuple pats -> begin (* todo correction: only top level tuple should be turned into a list *)
      let* pats' = TC.map ~f:translate_parameter_bindings pats
      in
      TC.return @@ List.concat pats'
    end
  | P_wild -> begin
      let* typ = Nanotype.nanotype_of_sail_type @@ Libsail.Type_check.typ_of_annot annotation
      and* id  = TC.generate_unique_identifier ~underscore:true ()
      in
      TC.return [(id, typ)]
    end
  | P_typ (_typ, pattern)       -> translate_parameter_bindings pattern (* parameter is annotated with type, e.g., function foo(x : int) = { } *)
  | P_or (_, _)                 -> TC.not_yet_implemented [%here] location
  | P_not _                     -> TC.not_yet_implemented [%here] location
  | P_as (_, _)                 -> TC.not_yet_implemented [%here] location
  | P_var (_, _)                -> TC.not_yet_implemented [%here] location
  | P_app (_, _)                -> TC.not_yet_implemented [%here] location
  | P_vector _                  -> TC.not_yet_implemented [%here] location
  | P_vector_concat _           -> TC.not_yet_implemented [%here] location
  | P_vector_subrange (_, _, _) -> TC.not_yet_implemented [%here] location
  | P_list _                    -> TC.not_yet_implemented [%here] location
  | P_cons (_, _)               -> TC.not_yet_implemented [%here] location
  | P_string_append _           -> TC.not_yet_implemented [%here] location
  | P_struct (_, _)             -> TC.not_yet_implemented [%here] location


let value_of_literal (literal : S.lit) : Ast.Value.t TC.t =
  let S.L_aux (unwrapped_literal, literal_location) = literal
  in
  match unwrapped_literal with
  | L_true     -> TC.return @@ Ast.Value.Bool true
  | L_false    -> TC.return @@ Ast.Value.Bool false
  | L_num n    -> TC.return @@ Ast.Value.Int n
  | L_unit     -> TC.return @@ Ast.Value.Unit
  | L_string s -> TC.return @@ Ast.Value.String s
  | L_zero   -> TC.return @@ Ast.Value.Bit false
  | L_one    -> TC.return @@ Ast.Value.Bit true
  | L_hex _  -> TC.not_yet_implemented [%here] literal_location
  | L_bin _  -> TC.not_yet_implemented [%here] literal_location
  | L_undef  -> TC.not_yet_implemented [%here] literal_location
  | L_real _ -> TC.not_yet_implemented [%here] literal_location


let flatten_named_statements (named_statements : (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list list) : (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list =
  let flattened = List.concat named_statements
  in
  let statement_names = List.map ~f:(fun (x, _, _) -> x) flattened
  in
  if List.contains_dup statement_names ~compare:Ast.Identifier.compare
  then failwith "BUG: two statements bear the same name"
  else flattened


(*
  Sail has only expressions, microSail makes the distinction between statements and expressions.
  Also, microSail statements can evaluate to a value, just like expressions do.

  Some Sail expressions need to be translated into microSail statements (e.g., reading from a register).

  This function returns a triple:

  - The first element, of type Ast.Expression.t, is the part of the Sail expression that fits in a microSail expression
  - The second element, of type Ast.Type.t, represents type of the first element
  - The third element, a list of "named statements" (i.e., triples of identifiers, types and statements), are the parts of the Sail expression
  that were translated into microSail statements. Since the evaluation result of a statement can be referred in the resulting microSail expression,
  we also name each statement.

  For example, a result

  (expr, t, [("a", t1, s1); ("b", t2, s2)])

  should be interpreted as

    let a : t1 = s1 in
    let b : t2 = s2 in
    expr

  with expr : t
 *)
let rec expression_of_aval
          (location : S.l         )
          (value    : S.typ S.aval) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
  =
  let expression_of_tuple (elements : S.typ S.aval list) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t =
    if
      List.is_empty elements
    then
      TC.not_yet_implemented ~message:"Encountered empty tuple; should not occur" [%here] location (* todo change to failure *)
    else begin
        let* translation_triples    = TC.map ~f:(expression_of_aval location) elements
        in
        let translation_expressions = List.map ~f:(fun (x, _, _) -> x) translation_triples
        and translation_types       = List.map ~f:(fun (_, x, _) -> x) translation_triples
        and translation_statements  = List.map ~f:(fun (_, _, x) -> x) translation_triples
        and make_pair x y           = Ast.Expression.BinaryOperation (Pair, x, y)
        in
        let resulting_expression    = Auxlib.reduce ~f:make_pair translation_expressions
        in
        TC.return (resulting_expression, Ast.Type.Tuple translation_types, flatten_named_statements translation_statements)
      end

  and expression_of_literal
        (literal : S.lit)
        (typ     : S.typ) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let* lit' = value_of_literal literal
    and* typ' = Nanotype.nanotype_of_sail_type typ
    in
    TC.return @@ (Ast.Expression.Val lit', typ', [])

  and expression_of_identifier
        (identifier : S.id                       )
        (lvar       : S.typ Libsail.Ast_util.lvar) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let* id' = Identifier.translate_identifier [%here] identifier
    in
    match lvar with
    | Local (_, typ) -> begin
        let* typ' =
          Nanotype.nanotype_of_sail_type typ
        in
        TC.return (Ast.Expression.Variable id', typ', [])
      end
    | Register typ -> begin
        let* typ' = Nanotype.nanotype_of_sail_type typ
        in
        let* unique_id =
          let prefix = Printf.sprintf "reg_%s_" (Ast.Identifier.string_of id')
          in
          TC.generate_unique_identifier ~prefix ()
        in
        let named_statements =
          [(unique_id, typ', Ast.Statement.ReadRegister id')]
        in
        TC.return (Ast.Expression.Variable unique_id, typ', named_statements)
      end
    | Enum typ -> begin
        let* typ' = Nanotype.nanotype_of_sail_type typ
        in
        let* type_identifier =
          match typ' with
          | Ast.Type.Enum type_identifier -> TC.return type_identifier
          | _                             -> TC.fail [%here] "Expected enum type"
        in
        let enum_value =
          Ast.Expression.Enum { type_identifier; constructor_identifier = id' }
        in
        TC.return (enum_value, typ', [])
      end
    | Unbound _    -> TC.not_yet_implemented [%here] location

  and expression_of_list
        (list : S.typ S.aval list)
        (typ : S.typ             ) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let* translation_triples = TC.map ~f:(expression_of_aval location) list
    and* typ'                = Nanotype.nanotype_of_sail_type typ
    in
    let translation_expressions, _, translation_statements =
      List.unzip3 translation_triples
    in
    TC.return (
        Ast.Expression.List translation_expressions,
        typ',
        flatten_named_statements translation_statements
      )

  and expression_of_record
        (bindings : S.typ S.aval Bindings.t)
        (typ      : S.typ                  ) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let S.Typ_aux (unwrapped_type, _typ_location) = typ
    in
    (* Find out which record we're building *)
    match unwrapped_type with
    | Typ_id type_identifier -> begin
        (*
           We know the record's name, we need the fields,
           more specifically, the order they are defined in
        *)
        let* type_identifier' =
          Identifier.translate_identifier [%here] type_identifier
        in
        let* type_definition =
          TC.lookup_type_definition_of_kind Ast.Definition.Select.of_record type_identifier'
        in
        match type_definition with
        | Some record_definition -> begin
            let record_field_names =
              List.map ~f:fst record_definition.fields
            in
            let* mapping, named_statements =
              translate_bindings location bindings
            in
            (*
               List of variables that contain the values
               for each of the record's fields
            *)
            let* field_value_identifiers =
              TC.map
                record_field_names
                ~f:(fun record_field_name -> begin
                      match Ast.Identifier.Map.find mapping record_field_name with
                      | Some value_identifier -> TC.return value_identifier
                      | None                  -> TC.fail [%here] "Field names in definition do not match those in literal"
                    end)
            in
            let record =
              Ast.Expression.Record {
                type_identifier = type_identifier';
                variable_identifiers = field_value_identifiers;
              }
            in
            TC.return (record, Ast.Type.Record type_identifier', named_statements)
          end
        | None -> TC.fail [%here] @@ Printf.sprintf "Expected to find record definition for %s" @@ StringOf.Sail.id type_identifier
      end
    | _ -> TC.fail [%here] "Unexpected type"

  and expression_of_vector
      (values : S.typ S.aval list)
      (typ    : S.typ            ) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let* values', value_types', named_statements' =
      let* triples =
        TC.map ~f:(expression_of_aval location) values
      in
      let values' =
        List.map ~f:Auxlib.first3 triples
      and types' =
        List.map ~f:Auxlib.second3 triples
      and named_statements' =
        flatten_named_statements @@ List.map ~f:Auxlib.third3 triples
      in
      TC.return (values', types', named_statements')
    in
    let* expression' =
      let is_bit (t : Ast.Type.t) : bool =
        Ast.Type.equal t Ast.Type.Bit
      in
      if
        List.for_all ~f:is_bit value_types'
      then
        TC.return @@ Ast.Expression.Bitvector values'
      else begin
        let element_types =
          String.concat ~sep:", " @@ List.map ~f:(fun t -> FExpr.to_string @@ Ast.Type.to_fexpr t) value_types'
        in
        TC.fail [%here] @@ Printf.sprintf "Elements of bit vector should all be bits; they were %s" element_types
      end
    in
    let* typ' =
      Nanotype.nanotype_of_sail_type typ
    in
    TC.return @@ (expression', typ', named_statements')

  in
  match value with
  | AV_tuple elements         -> expression_of_tuple elements
  | AV_lit (literal, typ)     -> expression_of_literal literal typ
  | AV_id (id, lvar)          -> expression_of_identifier id lvar
  | AV_list (list, typ)       -> expression_of_list list typ
  | AV_record (bindings, typ) -> expression_of_record bindings typ
  | AV_vector (values, typ)   -> expression_of_vector values typ
  | AV_ref (_, _)             -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)            -> TC.not_yet_implemented [%here] location


and translate_bindings
    (location : S.l                    )
    (bindings : S.typ S.aval Bindings.t) : (Ast.Identifier.t Ast.Identifier.Map.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
  =
  let rec process_bindings
      (binding_pairs    : (S.id * S.typ S.aval) list)
      (mapping          : Ast.Identifier.t Ast.Identifier.Map.t)
      (named_statements : (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) : (Ast.Identifier.t Ast.Identifier.Map.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    match binding_pairs with
    | [] -> TC.return (mapping, named_statements)
    | (identifier, value) :: rest -> begin
        let* identifier' =
          Identifier.translate_identifier [%here] identifier
        and* value', value_type', extra_named_statements =
          expression_of_aval location value
        and* fresh_id =
          TC.generate_unique_identifier ()
        in
        let extra_named_statement =
          (fresh_id, value_type', Ast.Statement.Expression value')
        and updated_mapping =
          Ast.Identifier.Map.add_exn ~key:identifier' ~data:fresh_id mapping
        in
        let updated_named_statements =
          List.concat [ named_statements; extra_named_statements; [extra_named_statement] ]
        in
        process_bindings rest updated_mapping updated_named_statements
      end
  in
  let result =
    let initial_binding_pairs    = Bindings.to_list bindings
    and initial_mapping          = Ast.Identifier.Map.empty
    and initial_named_statements = []
    in
    process_bindings
      initial_binding_pairs
      initial_mapping
      initial_named_statements
  in
  result


let make_sequence statements location =
  let rec aux statements =
    match statements with
    | []    -> TC.not_yet_implemented ~message:"Should not happen" [%here]  location
    | [x]   -> TC.return x
    | x::xs -> begin
        let* xs' = aux xs
        in
        TC.return @@ Ast.Statement.Seq (x, xs')
      end
  in
  aux statements


(*
  Given a list of named statements (i.e., pairs of strings and statements)
  and a statement, generates nested let bindings for each of the named statements
  and puts statement in the center.

  For example, say
  - named_statements = [("a", s1), ("b", s2)]
  - statement        = stm

  This function then returns

    let a = s1 in
    let b = s2 in
    stm
 *)
let rec wrap_in_named_statements_context
      (named_statements : (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list)
      (statement        : Ast.Statement.t                                       ) : Ast.Statement.t
  =
  match named_statements with
  | (variable_identifier, binding_statement_type, binding_statement)::rest -> begin
      let body_statement = wrap_in_named_statements_context rest statement
      in
      Let {
          variable_identifier   ;
          binding_statement_type;
          binding_statement     ;
          body_statement        ;
        }
    end
  | []                -> statement


type with_destructured_record_data = {
    record_identifier      : Ast.Identifier.t;
    record_type_identifier : Ast.Identifier.t;
    field_identifiers      : Ast.Identifier.t list;
    variable_identifiers   : Ast.Identifier.t list;
  }

let with_destructured_record
      (location       : S.l                                              )
      (value          : S.typ S.aval                                     )
      (body_generator : with_destructured_record_data -> Ast.Statement.t TC.t) : Ast.Statement.t TC.t
  =
  match value with
  | S.AV_id (record_identifier, lvar) -> begin
      let* record_identifier =
        Identifier.translate_identifier [%here] record_identifier
      and* S.Typ_aux (t, _loc) =
        type_from_lvar lvar location
      in
      match t with
      | Typ_id record_type_identifier -> begin
          let* record_type_identifier =
            Identifier.translate_identifier [%here] record_type_identifier
          in
          let* lookup_result =
            TC.lookup_type_definition_of_kind Ast.Definition.Select.of_record record_type_identifier
          in
          match lookup_result with
          | Some record_type_definition -> begin
              let field_identifiers =
                List.map ~f:fst record_type_definition.fields
              in
              let* variable_identifiers =
                TC.map ~f:(fun x -> TC.generate_unique_identifier ~prefix:(Ast.Identifier.string_of x) ()) field_identifiers
              in
              let* body =
                body_generator {
                  record_identifier;
                  record_type_identifier;
                  field_identifiers;
                  variable_identifiers
                }
              in
              let* destructured_record =
                statement_of_lvar record_identifier lvar location
              in
              TC.return @@ Ast.Statement.DestructureRecord {
                               record_type_identifier;
                               field_identifiers;
                               variable_identifiers;
                               destructured_record;
                               body
                             }
            end
          | None -> begin
              let message =
                Printf.sprintf "Tried looking up %s; expected to find record type definition" (Ast.Identifier.string_of record_type_identifier)
              in
              (* TC.fail [%here] message *) (* todo make this a failure again *)
              TC.not_yet_implemented ~message [%here] location
            end
        end
      | Typ_internal_unknown -> TC.not_yet_implemented [%here] location
      | Typ_var _            -> TC.not_yet_implemented [%here] location
      | Typ_fn (_, _)        -> TC.not_yet_implemented [%here] location
      | Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] location
      | Typ_tuple _          -> TC.not_yet_implemented [%here] location
      | Typ_app (_, _)       -> TC.not_yet_implemented [%here] location
      | Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] location
    end
  | Libsail.Anf.AV_lit (_, _)    -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_ref (_, _)    -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_tuple _       -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_list (_, _)   -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_vector (_, _) -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_record (_, _) -> TC.not_yet_implemented [%here] location
  | Libsail.Anf.AV_cval (_, _)   -> TC.not_yet_implemented [%here] location


let rec statement_of_aexp (expression : S.typ S.aexp) : Ast.Statement.t TC.t =
  let S.AE_aux (unwrapped_expression, _environment, location) = expression
  in

  (*
    Translation of a match expression

    match <matched> {
      pattern1 => expression1,
      pattern2 => expression2,
      ...
    }
   *)
  let statement_of_match
      (location : S.l                                              )
      (matched  : S.typ S.aval                                     )
      (cases    : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list) : Ast.Statement.t TC.t =

    (*
        MATCHING LISTS
    *)
    let rec match_list () =
      (* matched is a list *)
      let* () =
        let error_message =
          lazy "matching list; expected exactly two cases"
        in
        TC.check [%here] (List.length cases = 2) error_message
      in

      let* nil_case, cons_case =
        match cases with
        | [ (AP_aux (AP_nil  _, _, _), _, _) as nil_case;
            (AP_aux (AP_cons _, _, _), _, _) as cons_case ] -> TC.return (nil_case, cons_case)
        | [ (AP_aux (AP_cons _, _, _), _, _) as cons_case;
            (AP_aux (AP_nil  _, _, _), _, _) as nil_case  ] -> TC.return (nil_case, cons_case)
        | _                                                 -> TC.fail [%here] "unrecognized cases; should be nil and cons"
      in

      match nil_case, cons_case with
      | ( (AP_aux (AP_nil _, _, _), _, nil_clause),
          (AP_aux (AP_cons (
                       AP_aux (AP_id (id_h, _), _, _),
                       AP_aux (AP_id (id_t, _), _, _)
                     ), _, _), _, cons_clause) ) -> begin
          let* matched =
            let* expression, _expression_type, named_statements = expression_of_aval location matched
            in
            TC.return @@ wrap_in_named_statements_context named_statements @@ Ast.Statement.Expression expression

          and* when_nil =
            statement_of_aexp nil_clause

          and* when_cons =
            let* id_head = Identifier.translate_identifier [%here] id_h
            and* id_tail = Identifier.translate_identifier [%here] id_t
            and* clause = statement_of_aexp cons_clause
            in
            TC.return (id_head, id_tail, clause)
          in
          let match_pattern =
            Ast.Statement.MatchList {
                matched;
                when_cons;
                when_nil;
              }
          in
          TC.return @@ Ast.Statement.Match match_pattern
        end
      | _ -> TC.fail [%here] "list cases do not have expected structure"

    (*
        MATCHING TUPLES
    *)
    and match_tuple () =
      (* the matched variable is a tuple *)
      let* () =
        let n_cases = List.length cases
        in
        let error_message = lazy (Printf.sprintf "match tuple; expected only one case, got %d" n_cases)
        in
        TC.check [%here] (n_cases = 1) error_message
      in

      match cases with
      | [ (AP_aux (AP_tuple [
                       AP_aux (AP_id (id_l, _), _, _);
                       AP_aux (AP_id (id_r, _), _, _);
                     ], _, _),_ , clause) ] -> begin
          let* (matched, named_statements) =
            let* expression, _expression_type, named_statements = expression_of_aval location matched
            in
            TC.return (Ast.Statement.Expression expression, named_statements)
          and* id_fst = Identifier.translate_identifier [%here] id_l
          and* id_snd = Identifier.translate_identifier [%here] id_r
          and* body   = statement_of_aexp clause
          in
          TC.return @@ wrap_in_named_statements_context named_statements @@ Ast.Statement.Match (Ast.Statement.MatchProduct { matched; id_fst; id_snd; body })
        end
      | _ -> TC.not_yet_implemented [%here] location

    and match_type_by_identifier (type_identifier : S.id) =
      let S.Id_aux (type_identifier, location) = type_identifier
      in

      match type_identifier with
      | S.Id id -> begin
          if
            String.equal id "unit"
          then
            match_unit ()
          else begin
            let* type_definition = TC.lookup_type_definition @@ Ast.Identifier.mk id
            in
            match type_definition with
            | Some (Abbreviation def) -> match_abbreviation def
            | Some (Variant def)      -> match_variant def
            | Some (Enum def)         -> match_enum def
            | Some (Record def)       -> match_record def
            | None                    -> TC.fail [%here] @@ Printf.sprintf "Unknown type %s" id
          end
        end
      | S.Operator _ -> TC.not_yet_implemented [%here] location

    (*
       MATCHING UNIT
    *)
    and match_unit () =
      match cases with
      | [case] -> begin
          let (_, _, body) = case
          in
          statement_of_aexp body
        end
      | _      -> TC.fail [%here] "Expected exactly one case when matching unit"

    (*
       MATCHING ENUMS
    *)
    and match_enum (enum_definition : Ast.Definition.Type.Enum.t) =
      (* the matched variable has type enum as described by enum_definition *)
      let* () =
        let n_match_cases = List.length cases
        and n_enum_cases = List.length enum_definition.cases
        in
        let error_message = lazy begin
                                let enum_values = String.concat ~sep:", " (List.map ~f:Ast.Identifier.string_of enum_definition.cases)
                                in
                                Printf.sprintf
                                  "expected fewer or as many match cases (%d) as there are enum values (%d: %s)"
                                  n_match_cases
                                  n_enum_cases
                                  enum_values
                              end
        in
        TC.check [%here] (n_match_cases <= n_enum_cases) error_message
      in

      let process_case
            (table      : Ast.Statement.t Ast.Identifier.Map.t        )
            (match_case : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp)) : Ast.Statement.t Ast.Identifier.Map.t TC.t
        =
        let (AP_aux (pattern, _environment, _location), condition, body) = match_case
        in
        (*
           condition is an extra condition that needs to be satisfied for the branch to be taken;
           if no condition is given, the condition is simply true (or at least, the Sail representation for this value)
        *)
        match condition with
        | S.AE_aux (S.AE_val (S.AV_lit (L_aux (L_true, _), _)), _, _) -> begin
            match pattern with
            | S.AP_id (S.Id_aux (id, location), _typ) -> begin
                match id with
                | S.Operator  _   -> TC.not_yet_implemented [%here] location
                | S.Id identifier -> begin
                    let* () =
                      let error_message = lazy begin
                        Printf.sprintf
                          "encountered unknown case %s while matching an %s value"
                          identifier
                          (Ast.Identifier.string_of enum_definition.identifier)
                      end
                      in
                      TC.check
                        [%here]
                        (List.mem enum_definition.cases (Ast.Identifier.mk identifier) ~equal:Ast.Identifier.equal)
                        error_message
                    in
                    let* body' = statement_of_aexp body
                    in
                    let result = Ast.Identifier.Map.add table ~key:(Ast.Identifier.mk identifier) ~data:body'
                    in
                    match result with
                    | `Duplicate -> begin
                        let error_message = Printf.sprintf "duplicate case %s in enum match" identifier
                        in
                        TC.fail [%here] error_message
                      end
                    | `Ok table' -> TC.return table'
                  end
              end
            | S.AP_wild S.Typ_aux (_, _loc) -> begin
                let* body' = statement_of_aexp body
                in
                let add_case table case =
                  match Ast.Identifier.Map.add table ~key:case ~data:body' with
                  | `Duplicate -> table   (* wildcard only fills in missing cases, so ignore if there's already an entry for this enum case *)
                  | `Ok table' -> table'
                in
                TC.return @@ List.fold_left enum_definition.cases ~init:table ~f:add_case
              end
            | S.AP_tuple _        -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_global (_, _)  -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_app (_, _, _)  -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_cons (_, _)    -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_as (_, _, _)   -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_struct (_, _)  -> TC.fail [%here] "unexpected case while matching on enum"
            | S.AP_nil _          -> TC.fail [%here] "unexpected case while matching on enum"
          end
        | _ -> TC.fail [%here] "no conditions supported on matches"
      in
      let* (matched, named_statements) =
        let* matched_expression, _matched_expression_type, named_statements = expression_of_aval location matched
        in
        TC.return @@ (Ast.Statement.Expression matched_expression, named_statements)
      and* cases = TC.fold_left ~f:process_case ~init:Ast.Identifier.Map.empty cases
      in
      let matched_type =
        Ast.Type.Enum enum_definition.identifier
      in
      let* matched_identifier =
        TC.generate_unique_identifier ~prefix:"matched" ()
      in
      let match_statement =
        Ast.Statement.Let {
          variable_identifier    = matched_identifier;
          binding_statement_type = matched_type;
          binding_statement      = matched;
          body_statement         = Ast.Statement.Match (Ast.Statement.MatchEnum {
              matched            = matched_identifier;
              matched_type       = enum_definition.identifier;
              cases
            })
        }
      in
      TC.return @@ wrap_in_named_statements_context named_statements match_statement

    (*
        MATCHING VARIANTS
    *)
    and match_variant (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Statement.t TC.t =
      let process_case
          (acc  : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t)
          (case : S.typ S.apat * S.typ S.aexp * S.typ S.aexp                    ) : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t TC.t
        =
        let (pattern, condition, clause) = case
        in
        let* translated_clause = statement_of_aexp clause
        in
        (*
           condition is an extra condition that needs to be satisfied for the branch to be taken;
           if no condition is given, the condition is simply true (or at least, the Sail representation for this value)
        *)
        match condition with
        | S.AE_aux (S.AE_val (S.AV_lit (L_aux (L_true, _), _)), _, _) -> begin
            let AP_aux (pattern, _environment, _pattern_location) = pattern
            in
            match pattern with
            | Libsail.Anf.AP_app (variant_tag_identifier, subpattern, _typ) -> begin
                (*
                   deals with case TAG(x, y, z, ...)

                   x, y, z must all be simple identifiers, no nested patterns are supported
                 *)
                let* variant_tag_identifier = Identifier.translate_identifier [%here] variant_tag_identifier
                in
                let AP_aux (subpattern, _environment, subpattern_location) = subpattern
                in
                match subpattern with
                | S.AP_tuple tuple_patterns -> begin
                    (* assumes tuple patterns are all identifiers *)
                    let extract_identifiers (tuple_pattern : S.typ S.apat) =
                      let S.AP_aux (tuple_pattern, _env, tuple_pattern_location) = tuple_pattern
                      in
                      match tuple_pattern with
                      | Libsail.Anf.AP_id (identifier, _typ) -> Identifier.translate_identifier [%here] identifier
                      | Libsail.Anf.AP_tuple _               -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_global (_, _)         -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_app (_, _, _)         -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_cons (_, _)           -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_as (_, _, _)          -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_struct (_, _)         -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_nil _                 -> TC.not_yet_implemented [%here] tuple_pattern_location
                      | Libsail.Anf.AP_wild _                -> TC.not_yet_implemented [%here] tuple_pattern_location
                    in
                    let* identifiers = TC.map ~f:extract_identifiers tuple_patterns
                    in
                    TC.return @@ Ast.Identifier.Map.add_exn acc ~key:variant_tag_identifier ~data:(identifiers, translated_clause)
                  end
                | S.AP_id (identifier, _typ) -> begin
                    let* identifier = Identifier.translate_identifier [%here] identifier
                    in
                    TC.return @@ Ast.Identifier.Map.add_exn acc ~key:variant_tag_identifier ~data:([identifier], translated_clause)
                  end
                | S.AP_wild _typ     -> begin
                    let* identifier = TC.generate_unique_identifier ~underscore:true ()
                    in
                    TC.return @@ Ast.Identifier.Map.add_exn acc ~key:variant_tag_identifier ~data:([identifier], translated_clause)
                  end
                | S.AP_global (_, _) -> TC.not_yet_implemented [%here] subpattern_location
                | S.AP_app (_, _, _) -> TC.not_yet_implemented [%here] subpattern_location
                | S.AP_cons (_, _)   -> TC.not_yet_implemented [%here] subpattern_location
                | S.AP_as (_, _, _)  -> TC.not_yet_implemented [%here] subpattern_location
                | S.AP_struct (_, _) -> TC.not_yet_implemented [%here] subpattern_location
                | S.AP_nil _         -> TC.not_yet_implemented [%here] subpattern_location
              end
            | S.AP_wild _ -> begin
                (* only adds to table if constructor is missing *)
                let add_missing_case
                    (acc                 : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t)
                    (variant_constructor : Ast.Definition.Type.Variant.constructor                       ) =
                  let (constructor_tag, fields) = variant_constructor
                  in
                  let* field_vars =
                    TC.map ~f:(fun _ -> TC.generate_unique_identifier ~underscore:true ()) fields
                  in
                  let acc' = match Ast.Identifier.Map.add acc ~key:constructor_tag ~data:(field_vars, translated_clause) with
                    | `Duplicate -> acc   (* constructor has already been dealt with previously, so ignore this case *)
                    | `Ok acc'   -> acc'  (* missing case found                                                      *)
                  in
                  TC.return acc'
                in
                TC.fold_left ~f:add_missing_case ~init:acc variant_definition.constructors
              end
            | S.AP_tuple _       -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_id (_, _)     -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_global (_, _) -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_cons (_, _)   -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_as (_, _, _)  -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_struct (_, _) -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
            | S.AP_nil _         -> TC.fail [%here] "we're matching a variant; only AP_app should occur here"
          end
        | _ -> TC.fail [%here] "variant cases do not have expected structure"
      in
      let* cases = TC.fold_left ~f:process_case ~init:Ast.Identifier.Map.empty cases
      in
      let* match_statement =
        let* matched_expression, _matched_expression_type, named_statements = expression_of_aval location matched
        and* matched_identifier = TC.generate_unique_identifier ~prefix:"matched" ()
        in
        let matched      = Ast.Statement.Expression matched_expression
        and matched_type = variant_definition.identifier
        in
        let match_statement =
          Ast.Statement.Let {
            variable_identifier    = matched_identifier;
            binding_statement_type = Ast.Type.Variant variant_definition.identifier;
            binding_statement      = matched;
            body_statement         = Ast.Statement.Match (Ast.Statement.MatchVariant { matched = matched_identifier; matched_type; cases })
          }
        in
        TC.return @@ wrap_in_named_statements_context named_statements match_statement
      in
      TC.return match_statement

    and match_abbreviation (_type_abbreviation : Ast.Definition.Type.Abbreviation.t) =
      TC.not_yet_implemented [%here] location

    and match_record (_record_definition : Ast.Definition.Type.Record.t) =
      TC.not_yet_implemented [%here] location

    and match_typed (Typ_aux (type_of_matched, location) : S.typ) =
      match type_of_matched with
      | S.Typ_app (Id_aux (Id "list", _), _) -> match_list ()
      | S.Typ_tuple _                        -> match_tuple ()
      | S.Typ_id id                          -> match_type_by_identifier id
      | S.Typ_internal_unknown               -> TC.not_yet_implemented [%here] location
      | S.Typ_var _                          -> TC.not_yet_implemented [%here] location
      | S.Typ_fn (_, _)                      -> TC.not_yet_implemented [%here] location
      | S.Typ_bidir (_, _)                   -> TC.not_yet_implemented [%here] location
      | S.Typ_app (_, _)                     -> TC.not_yet_implemented [%here] location
      | S.Typ_exist (_, _, _)                -> TC.not_yet_implemented [%here] location

    and match_literal
        (literal       : S.lit)
        (_literal_type : S.typ) : Ast.Statement.t TC.t =
      let L_aux (literal, _loc) = literal
      in
      match literal with
      | S.L_unit     -> begin
          match cases with
          | [ case ] -> begin
              let pattern, _condition, clause = case
              in
              let S.AP_aux (pattern, _type_check_environment, _loc) = pattern
              in
              match pattern with
               | S.AP_id (_, _)   -> TC.not_yet_implemented [%here] location
               | S.AP_wild _      -> statement_of_aexp clause
               | _                -> TC.fail [%here] "Expected unit to be bound to either wildcard or identifier"
            end
          | _        -> TC.fail [%here] "Matching unit; expected exactly one case"
        end
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

    in
    match matched with
    | S.AV_id (_id, lvar) -> begin
        match lvar with (* todo replace by type_from_lvar *)
        | S.Ast_util.Local (_mut, typ) -> match_typed typ
        | S.Ast_util.Register typ      -> match_typed typ
        | S.Ast_util.Enum typ          -> match_typed typ
        | S.Ast_util.Unbound _         -> TC.not_yet_implemented [%here] location
      end
    | S.AV_lit (literal, literal_type) -> match_literal literal literal_type
    | S.AV_ref (_, _)    -> TC.not_yet_implemented [%here] location
    | S.AV_tuple _       -> TC.not_yet_implemented [%here] location
    | S.AV_list (_, _)   -> TC.not_yet_implemented [%here] location
    | S.AV_vector (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_record (_, _) -> TC.not_yet_implemented [%here] location
    | S.AV_cval (_, _)   -> TC.not_yet_implemented [%here] location

  and statement_of_field_access
        (location         : S.l         )
        (value            : S.typ S.aval)
        (field_identifier : S.id        )
        (_field_type      : S.typ       ) : Ast.Statement.t TC.t
    =
    let* field_identifier = Identifier.translate_identifier [%here] field_identifier
    in
    with_destructured_record location value @@
      fun { record_type_identifier; field_identifiers; variable_identifiers; _ } -> (
        match Auxlib.find_index_of ~f:(Ast.Identifier.equal field_identifier) field_identifiers with
        | Some selected_field_index -> begin
            let expression =
              Ast.Expression.Variable (List.nth_exn variable_identifiers selected_field_index)
            in
            TC.return @@ Ast.Statement.Expression expression
          end
        | None -> TC.fail [%here] @@ Printf.sprintf "Record %s should have field named %s" (Ast.Identifier.string_of record_type_identifier) (Ast.Identifier.string_of field_identifier)
      )

  and statement_of_value (value : S.typ S.aval) : Ast.Statement.t TC.t =
    let* expression, _expression_type, named_statements = expression_of_aval location value
    in
    TC.return @@ wrap_in_named_statements_context named_statements @@ Ast.Statement.Expression expression

  and statement_of_application
          (receiver_identifier : S.id             )
          (arguments           : S.typ S.aval list)
          (_typ                : S.typ            )
    =
    let* receiver_identifier' = Identifier.translate_identifier [%here] receiver_identifier
    and* translated_arguments = TC.map ~f:(expression_of_aval location) arguments
    in
    let argument_expressions, _argument_expression_types, unflattened_named_statements =
      List.unzip3 translated_arguments
    in
    let named_statements =
      flatten_named_statements unflattened_named_statements
    in
    let wrap =
      wrap_in_named_statements_context named_statements
    in
    let binary_operation (operator : Ast.BinaryOperator.t) : Ast.Statement.t TC.t
      =
        match argument_expressions with
        | [x; y] -> TC.return @@ wrap @@ Ast.Statement.Expression (BinaryOperation (operator, x, y))
        | _      -> TC.fail [%here] "binary operation should have 2 arguments"
    in
    let generic_function_call () =
      (*
        Sail translates the construction of a variant type to a function call.

          union Foo {
            Bar : unit
          }

          let x = Bar ()

        Here, Bar () is represented by a call to a function named Bar.

        In muSail, the construction of a variant type is a special expression exp_union.

        The code below checks if a function call is actually a variant constructor,
        and translates it to a specialized expression.
      *)
      let* variant_definition =
        TC.lookup_variant_by_constructor receiver_identifier'
      in
      match variant_definition with
      | Some variant_definition -> begin
          (* Function call needs to be translated to variant value construction *)
          match argument_expressions with
          | [argument] -> begin
              (*
                 A variant value can take 0+ fields. All these fields are packed into a single value, stored in <argument>.

                   ()        : unit
                   x         : x
                   (x, y)    : Pair(x, y)
                   (x, y, z) : Pair(Pair(x, y), z)

                 flatten_fields extracts the different arguments from these potentially nested pairs.
                 We probably do not deal correctly with the case that one of the fields is an actual pair.
              *)
              let rec flatten_fields (expression : Ast.Expression.t) =
                match expression with
                | Ast.Expression.Val Ast.Value.Unit                              -> []
                | Ast.Expression.BinaryOperation (Ast.BinaryOperator.Pair, x, y) -> List.concat [flatten_fields x; flatten_fields y]
                | _                                                              -> [expression]
              in
              let fields =
                flatten_fields argument
              in
              let variant =
                Ast.Expression.Variant {
                  type_identifier        = variant_definition.identifier;
                  constructor_identifier = receiver_identifier';
                  fields                 = fields;
                }
              in
              TC.return @@ wrap @@ Ast.Statement.Expression variant
            end
          | _ -> TC.fail [%here] "Should not happen: variant fields are packed into one value"
        end
      | None -> begin
          (* Function call does not refer to variant constructor *)
          TC.return @@ wrap @@ Ast.Statement.Call (receiver_identifier', argument_expressions)
        end
    in

    match Ast.Identifier.string_of receiver_identifier' with
    | "sail_cons" -> binary_operation Cons
    | "add_atom"  -> binary_operation Plus
    | "sub_atom"  -> binary_operation Minus
    | "mult_atom" -> binary_operation Times
    | "lt_int"    -> binary_operation LessThan
    | "lteq_int"  -> binary_operation LessThanOrEqualTo
    | "gt_int"    -> binary_operation GreaterThan
    | "gteq_int"  -> binary_operation GreaterThanOrEqualTo
    | "eq_int"    -> binary_operation EqualTo
    | "neq_int"   -> binary_operation NotEqualTo
    | _           -> generic_function_call ()

  and statement_of_let
        (_mutability : Libsail.Ast_util.mut)
        (identifier  : S.id                )
        (typ1        : S.typ               )
        (expression  : S.typ S.aexp        )
        (body        : S.typ S.aexp        )
        (_typ2       : S.typ               )
    =
    let* id'   = Identifier.translate_identifier [%here] identifier  (* todo rename to record fields *)
    and* typ1' = Nanotype.nanotype_of_sail_type typ1
    and* s1    = statement_of_aexp expression
    and* s2    = statement_of_aexp body
    in
    TC.return @@ Ast.Statement.Let {
                     variable_identifier = id';
                     binding_statement_type = typ1';
                     binding_statement = s1;
                     body_statement = s2;
                   }

  and statement_of_if
        (condition   : S.typ S.aval)
        (then_clause : S.typ S.aexp)
        (else_clause : S.typ S.aexp)
        (_typ        : S.typ       )
    =
    let* (condition, condition_named_statements) =
      let* condition_expression, _condition_expression_type, named_statements = expression_of_aval location condition
      in
      TC.return (Ast.Statement.Expression condition_expression, named_statements)
    and* when_true = statement_of_aexp then_clause
    and* when_false = statement_of_aexp else_clause
    in
    TC.return @@ wrap_in_named_statements_context condition_named_statements @@ Ast.Statement.Match (MatchBool { condition; when_true; when_false })

  and statement_of_block
        (statements     : S.typ S.aexp list)
        (last_statement : S.typ S.aexp     )
        (_typ           : S.typ            )
    =
    let* translated_statements = TC.map ~f:statement_of_aexp (statements @ [last_statement])
    in
    make_sequence translated_statements location

  and statement_of_struct_update
        (aval     : S.typ S.aval           )
        (bindings : S.typ S.aval Bindings.t)
        (_typ     : S.typ                  )
    =
    (*
      Processes single assignment to field
      - pair: contains information pertaining to the field update
      - acc : data structure in which to accumulate the translated information
     *)
    let process_binding
        (acc  : Ast.Identifier.t Ast.Identifier.Map.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list)
        (pair : S.id * S.typ S.aval                                                                           )
      =
      let field_map       , named_statements = acc
      and field_identifier, value            = pair
      in
      let* field_identifier =
        Identifier.translate_identifier [%here] field_identifier
      in
      (* Generate fresh name for variable that will be assigned the value of the field *)
      let* variable_identifier =
        TC.generate_unique_identifier ~prefix:("updated_" ^ (Ast.Identifier.string_of field_identifier)) ()
      in
      (* Convert assigned expression to statement *)
      let* (field_type, named_statement) =
        let* expression, expression_type, named_statements = expression_of_aval location value
        in
        TC.return @@ (expression_type, wrap_in_named_statements_context named_statements (Ast.Statement.Expression expression))
      in
      let named_statements' =
        (variable_identifier, field_type, named_statement) :: named_statements
      in
      let field_map' =
        StringMap.overwrite ~key:field_identifier ~data:variable_identifier field_map
      in
      TC.return @@ (field_map', named_statements')
    in
    with_destructured_record location aval @@ fun { record_type_identifier; field_identifiers; variable_identifiers; _ } -> begin
      let* field_map, named_statements =
        let initial_field_map : Ast.Identifier.t Ast.Identifier.Map.t =
          Ast.Identifier.Map.of_alist_exn @@ List.zip_exn field_identifiers variable_identifiers
        in
        TC.fold_left ~f:process_binding ~init:(initial_field_map, []) (Bindings.bindings bindings)
      in
      let record_expression =
        let type_identifier = record_type_identifier
        and variable_identifiers =
          List.map field_identifiers ~f:(StringMap.find_exn field_map)
        in
        Ast.Expression.Record { type_identifier; variable_identifiers }
      in
      TC.return @@ wrap_in_named_statements_context named_statements (Expression record_expression)
    end

  and statement_of_assignment
        (lhs : Libsail.Ast.typ Libsail.Anf.alexp)
        (rhs : Libsail.Ast.typ Libsail.Anf.aexp ) : Ast.Statement.t TC.t
    =
    match lhs with
    | Libsail.Anf.AL_id (id, lhs_type) -> begin
        let* id_in_lhs = Identifier.translate_identifier [%here] id
        in
        let* is_register = TC.is_register id_in_lhs
        in
        if is_register
        then begin
          (*
               r1 = expr

             becomes

               let _ = let x = expr
                       in
                       r1 = x
               in
               ()

             This is necessary for the following reasons:
             - Sail does not make the distinction between expressions and statements, whereas muSail does
             - In Sail, the type of assignment if unit, but not in muSail
           *)
          let* rhs_identifier = TC.generate_unique_identifier ()
          and* translated_rhs = statement_of_aexp rhs
          and* rhs_type       = Nanotype.nanotype_of_sail_type lhs_type;
          in
          let write_register_translation =
            Ast.Statement.Let {
              variable_identifier    = rhs_identifier;
              binding_statement_type = rhs_type;
              binding_statement      = translated_rhs;
              body_statement         = Ast.Statement.WriteRegister { register_identifier = id_in_lhs; written_value = rhs_identifier }
            }
          in
          let* dummy_variable = TC.generate_unique_identifier ()
          in
          TC.return begin
            Ast.Statement.Let {
              variable_identifier    = dummy_variable;
              binding_statement_type = rhs_type;
              binding_statement      = write_register_translation;
              body_statement         = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit)
            }
          end
        end
        else begin
          TC.not_yet_implemented ~message:"assignment to local variable" [%here] location
        end
      end
    | Libsail.Anf.AL_addr (_, _)  -> TC.not_yet_implemented [%here] location
    | Libsail.Anf.AL_field (_, _) -> TC.not_yet_implemented [%here] location

  and statement_of_short_circuit
        (logical_operator : S.sc_op     )
        (lhs              : S.typ S.aval)
        (rhs              : S.typ S.aexp)
    =
    let* lhs_expression, _lhs_expression_type, lhs_named_statements = expression_of_aval location lhs
    and* rhs_statement = statement_of_aexp rhs
    in
    let lhs_expr_as_statement = Ast.Statement.Expression lhs_expression
    in
    let if_statement =
      match logical_operator with
      | Libsail.Anf.SC_and -> begin
          (*
            Translate

              x && y

            to

              if ( x ) { y } else { false }
           *)
          let condition  = lhs_expr_as_statement
          and when_true  = rhs_statement
          and when_false = Ast.(Statement.Expression (Val (Bool false)))
          in
          create_if_statement ~condition ~when_true ~when_false
        end
      | Libsail.Anf.SC_or -> begin
          (*
            Translate

              x || y

            to

              if ( x ) { true } else { y }
           *)
          let condition  = lhs_expr_as_statement
          and when_true  = Ast.(Statement.Expression (Val (Bool true)))
          and when_false = rhs_statement
          in
          create_if_statement ~condition ~when_true ~when_false
        end
    in
    TC.return @@ wrap_in_named_statements_context lhs_named_statements if_statement

  and statement_of_cast
        (expression  : S.typ S.aexp)
        (target_type : S.typ       ) : Ast.Statement.t TC.t
    =
    let* translated_expression = statement_of_aexp expression
    and* translated_type       = Nanotype.nanotype_of_sail_type target_type
    in
    TC.return @@ Ast.Statement.Cast (translated_expression, translated_type)

  and statement_of_throw
        (_aval : Libsail.Ast.typ Libsail.Anf.aval)
        (_typ  : Libsail.Ast.typ                 ) : Ast.Statement.t TC.t
    =
    TC.return @@ Ast.Statement.Fail "\"failure\"" (* todo *)

  and statement_of_exit
        (_aval : Libsail.Ast.typ Libsail.Anf.aval)
        (_typ  : Libsail.Ast.typ                 ) : Ast.Statement.t TC.t
    =
    TC.return @@ Ast.Statement.Fail "\"failure\"" (* todo *)

  in
  match unwrapped_expression with
  | AE_val value                                                  -> statement_of_value value
  | AE_app (id, avals, typ)                                       -> statement_of_application id avals typ
  | AE_let (mutability, identifier, typ1, expression, body, typ2) -> statement_of_let mutability identifier typ1 expression body typ2
  | AE_if (condition, then_clause, else_clause, typ)              -> statement_of_if condition then_clause else_clause typ
  | AE_match (aval, cases, _)                                     -> statement_of_match location aval cases
  | AE_block (statements, last_statement, typ)                    -> statement_of_block statements last_statement typ
  | AE_field (aval, field_identifier, field_type)                 -> statement_of_field_access location aval field_identifier field_type
  | AE_struct_update (aval, bindings, typ)                        -> statement_of_struct_update aval bindings typ
  | AE_assign (lhs, rhs)                                          -> statement_of_assignment lhs rhs
  | AE_short_circuit (logical_operator, lhs, rhs)                 -> statement_of_short_circuit logical_operator lhs rhs
  | AE_typ (expression, target_type)                              -> statement_of_cast expression target_type
  | AE_throw (aval, typ)                                          -> statement_of_throw aval typ
  | AE_exit (aval, typ)                                           -> statement_of_exit aval typ
  | AE_return (_, _)                                              -> TC.not_yet_implemented [%here] location
  | AE_try (_, _, _)                                              -> TC.not_yet_implemented [%here] location
  | AE_for (_, _, _, _, _, _)                                     -> TC.not_yet_implemented [%here] location
  | AE_loop (_, _, _)                                             -> TC.not_yet_implemented [%here] location


type sail_function_parts =
  {
    identifier         : S.id;
    parameter_bindings : Sail.type_annotation Libsail.Ast.pat;
    body               : Libsail.Ast.typ Libsail.Anf.aexp;
    return_type        : Libsail.Ast.typ;
  }


let extract_function_parts (function_clause : Sail.type_annotation Libsail.Ast.funcl) : sail_function_parts TC.t =
  let S.FCL_aux (S.FCL_funcl (identifier, clause), (_def_annot, _type_annotation)) = function_clause
  in
  let S.Pat_aux (unwrapped_clause, (location, _annotation)) = clause
  in
  match unwrapped_clause with
   | Libsail.Ast.Pat_when (_, _, _) -> TC.not_yet_implemented [%here] location
   | Libsail.Ast.Pat_exp (parameter_bindings, raw_body) -> begin
       let body = S.anf raw_body in
       let return_type = Libsail.Type_check.typ_of raw_body
       in
       TC.return @@ {
         identifier;
         parameter_bindings;
         body;
         return_type
       }
     end


let translate_body = statement_of_aexp


let translate_function_definition
      (definition_annotation : S.def_annot                  )
      (function_definition   : Sail.type_annotation S.fundef) : Ast.Definition.t TC.t
  =
  let S.FD_aux ((FD_function (_, _, funcls)), _) = function_definition
  in
  match funcls with
  | [function_clause] -> begin
      let* parts = extract_function_parts function_clause
      in
      let* function_name          = Identifier.translate_identifier [%here] parts.identifier
      and* parameters             = translate_parameter_bindings parts.parameter_bindings
      and* return_type            = translate_return_type parts.return_type
      and* function_body          = translate_body parts.body
      and* extended_function_type = ExtendedType.determine_extended_type parts.parameter_bindings parts.return_type
      in
      TC.return @@ Ast.Definition.FunctionDefinition {
        function_name;
        function_type = {
            parameters;
            return_type;
          };
        extended_function_type;
        function_body;
      }
    end
  | _ -> TC.not_yet_implemented [%here] definition_annotation.loc
