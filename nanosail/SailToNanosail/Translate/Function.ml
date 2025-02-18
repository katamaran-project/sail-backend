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

open! ExtBase
open Monads.Notations.Star(TC)


(* todo helper function that reads out identifier and takes into account the provenance (local vs register) *)

let sail_type_of_lvar
      (lvar : S.typ S.lvar)
      (loc  : S.l         ) : S.typ TC.t
  =
  match lvar with
  | Register t   -> TC.return t
  | Enum t       -> TC.return t
  | Local (_, t) -> TC.return t
  | Unbound _    -> TC.not_yet_implemented [%here] loc


let type_of_aval
    (value    : S.typ S.aval)
    (location : S.l         ) : Ast.Type.t TC.t
  =
  match value with
   | AV_lit (_literal, literal_type) -> Nanotype.nanotype_of_sail_type literal_type
   | AV_id (_identifier, lvar)       -> let* t = sail_type_of_lvar lvar location in Nanotype.nanotype_of_sail_type t
   | AV_list (_elements, typ)        -> begin
       let* element_type = Nanotype.nanotype_of_sail_type typ
       in
       TC.return @@ Ast.Type.List element_type
     end
   | AV_ref (_, _)                   -> TC.not_yet_implemented [%here] location
   | AV_tuple _                      -> TC.not_yet_implemented [%here] location
   | AV_vector (_, _)                -> TC.not_yet_implemented [%here] location
   | AV_record (_, _)                -> TC.not_yet_implemented [%here] location
   | AV_cval (_, _)                  -> TC.not_yet_implemented [%here] location


(*
   If statements need to be translated to a match on boolean values.

     if condition then A else B

   becomes

     match condition {
       true => A,
       false => B
     }
*)
let create_if_statement
      ~(condition  : Ast.Statement.t)
      ~(when_true  : Ast.Statement.t)
      ~(when_false : Ast.Statement.t) : Ast.Statement.t TC.t
  =
  let* condition_variable =
    TC.generate_unique_identifier ()
  in
  let match_pattern =
    Ast.Statement.MatchBool {
      condition = condition_variable;
      when_true;
      when_false
    }
  in
  TC.return begin
    Ast.Statement.Let {
      binder = condition_variable;
      binding_statement_type = Ast.Type.Bool;
      binding_statement = condition;
      body_statement = Ast.Statement.Match match_pattern
    }
  end


let statement_of_lvar
    (identifier : Ast.Identifier.t     )
    (lvar       : S.typ S.Ast_util.lvar)
    (location   : S.l                  ) : Ast.Statement.t TC.t
  =
  match lvar with
  | Register _     -> TC.return @@ Ast.Statement.ReadRegister identifier
  | Local (_, typ) -> begin
      let* typ' = Nanotype.nanotype_of_sail_type typ
      in
      TC.return @@ Ast.Statement.Expression (Ast.Expression.Variable (identifier, typ'))
    end
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
        let resulting_expression    = List.reduce ~f:make_pair translation_expressions
        in
        let* expression_type =
          match translation_types with
          | []       -> TC.fail [%here] "should not occur"
          | [_]      -> TC.fail [%here] "should not occur"
          | _        -> TC.return @@ Ast.Type.Tuple translation_types
        in
        TC.return (resulting_expression, expression_type, flatten_named_statements translation_statements)
      end

  and expression_of_literal
        (literal : S.lit)
        (typ     : S.typ) : (Ast.Expression.t * Ast.Type.t * (Ast.Identifier.t * Ast.Type.t * Ast.Statement.t) list) TC.t
    =
    let* lit' = Literal.value_of_literal literal
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
        TC.return (Ast.Expression.Variable (id', typ'), typ', [])
      end
    | Register typ -> begin
        let* typ' = Nanotype.nanotype_of_sail_type typ
        in
        let* unique_id =
          let prefix = Printf.sprintf "reg_%s_" (Ast.Identifier.to_string id')
          in
          TC.generate_unique_identifier ~prefix ()
        in
        let named_statements =
          [(unique_id, typ', Ast.Statement.ReadRegister id')]
        in
        TC.return (Ast.Expression.Variable (unique_id, typ'), typ', named_statements)
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
        let* record_definition =
          TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_record_named type_identifier')
        in
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
            fields          = field_value_identifiers;
          }
        in
        TC.return (record, Ast.Type.Record type_identifier', named_statements)
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
        List.map ~f:Auxlib.Triple.first triples
      and types' =
        List.map ~f:Auxlib.Triple.second triples
      and named_statements' =
        flatten_named_statements @@ List.map ~f:Auxlib.Triple.third triples
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
  | (binder, binding_statement_type, binding_statement)::rest -> begin
      let body_statement = wrap_in_named_statements_context rest statement
      in
      Let {
          binder;
          binding_statement_type;
          binding_statement;
          body_statement;
        }
    end
  | []                -> statement


type with_destructured_record_data = {
    record_identifier      : Ast.Identifier.t;
    record_type_identifier : Ast.Identifier.t;
    fields                 : (Ast.Identifier.t * Ast.Type.t) list;
    binders                : Ast.Identifier.t list;
  }

let with_destructured_record
      (location       : S.l                                              )
      (value          : S.typ S.aval                                     )
      (body_generator : with_destructured_record_data -> Ast.Statement.t TC.t) : Ast.Statement.t TC.t
  =
  match value with
  | AV_id (record_identifier, lvar) -> begin
      let* record_identifier =
        Identifier.translate_identifier [%here] record_identifier
      and* S.Typ_aux (t, _loc) =
        sail_type_of_lvar lvar location
      in
      match t with
      | Typ_id record_type_identifier -> begin
          let* record_type_identifier =
            Identifier.translate_identifier [%here] record_type_identifier
          in
          let* record_type_definition =
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_record_named record_type_identifier)
          in
          let fields =
            record_type_definition.fields
          in
          let field_identifiers =
            List.map ~f:fst fields
          in
          let* binders =
            TC.map ~f:(fun x -> TC.generate_unique_identifier ~prefix:(Ast.Identifier.to_string x) ()) field_identifiers
          in
          let* body =
            body_generator {
              record_identifier;
              record_type_identifier;
              fields;
              binders
            }
          in
          let* destructured_record =
            statement_of_lvar record_identifier lvar location
          in
          TC.return @@ Ast.Statement.DestructureRecord {
                           record_type_identifier;
                           field_identifiers;
                           binders;
                           destructured_record;
                           body
                         }
        end
      | Typ_internal_unknown -> TC.not_yet_implemented [%here] location
      | Typ_var _            -> TC.not_yet_implemented [%here] location
      | Typ_fn (_, _)        -> TC.not_yet_implemented [%here] location
      | Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] location
      | Typ_tuple _          -> TC.not_yet_implemented [%here] location
      | Typ_app (_, _)       -> TC.not_yet_implemented [%here] location
      | Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] location
    end
  | AV_lit (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_ref (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_tuple _         -> TC.not_yet_implemented [%here] location
  | AV_list (_, _)     -> TC.not_yet_implemented [%here] location
  | AV_vector (_, _)   -> TC.not_yet_implemented [%here] location
  | AV_record (_, _)   -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)     -> TC.not_yet_implemented [%here] location


let rec statement_of_aexp (expression : S.typ S.aexp) : Ast.Statement.t TC.t =
  let S.AE_aux (unwrapped_expression, annotation) = expression
  in
  let location = annotation.loc
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
      (matched  : S.typ S.aval                                     )
      (cases    : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list) : Ast.Statement.t TC.t
    =
    let* matched_variable =
      TC.generate_unique_identifier ()
    and* cases_with_translated_bodies =
      let translate_case_body (pattern, condition, body) =
        let* translated_body = statement_of_aexp body
        in
        TC.return (pattern, condition, translated_body)
      in
      TC.map ~f:translate_case_body cases
    and* matched, matched_variable_type =
      let* expression, expression_type, named_statements = expression_of_aval location matched
      in
      TC.return (wrap_in_named_statements_context named_statements @@ Ast.Statement.Expression expression, expression_type)
    in
    let* match_statement =
      Match.translate location matched_variable matched_variable_type cases_with_translated_bodies
    in
    TC.return begin
      Ast.Statement.Let {
        binder                 = matched_variable;
        binding_statement_type = matched_variable_type;
        binding_statement      = matched;
        body_statement         = match_statement
      }
    end

  and statement_of_field_access
        (value            : S.typ S.aval)
        (field_identifier : S.id        )
        (_field_type      : S.typ       ) : Ast.Statement.t TC.t
    =
    let* field_identifier = Identifier.translate_identifier [%here] field_identifier
    in
    with_destructured_record location value @@
      fun { record_type_identifier; fields; binders; _ } -> (
        match List.find_index_of ~f:(fun field -> Ast.Identifier.equal field_identifier (fst field)) fields with
        | Some selected_field_index -> begin
            let expression =
              let binder        = List.nth_exn binders selected_field_index
              and variable_type  = snd @@ List.nth_exn fields selected_field_index
              in
              Ast.Expression.Variable (binder, variable_type)
            in
            TC.return @@ Ast.Statement.Expression expression
          end
        | None -> TC.fail [%here] @@ Printf.sprintf "Record %s should have field named %s" (Ast.Identifier.to_string record_type_identifier) (Ast.Identifier.to_string field_identifier)
      )

  and statement_of_value (value : S.typ S.aval) : Ast.Statement.t TC.t =
    let* expression, _expression_type, named_statements = expression_of_aval location value
    in
    TC.return @@ wrap_in_named_statements_context named_statements @@ Ast.Statement.Expression expression

  and statement_of_application
      (receiver_identifier : S.id             )
      (arguments           : S.typ S.aval list)
      (_typ                : S.typ            ) : Ast.Statement.t TC.t
    =
    let* receiver_identifier' = Identifier.translate_identifier [%here] receiver_identifier
    and* translated_arguments = TC.map ~f:(expression_of_aval location) arguments
    in
    let argument_expressions, argument_expression_types, unflattened_named_statements =
      List.unzip3 translated_arguments
    in
    let named_statements =
      flatten_named_statements unflattened_named_statements
    in
    let wrap =
      wrap_in_named_statements_context named_statements
    in
    let statement_of_binary_operation (operator : Ast.BinaryOperator.t) : Ast.Statement.t TC.t =
      match argument_expressions with
      | [x; y] -> TC.return @@ wrap @@ Ast.Statement.Expression (BinaryOperation (operator, x, y))
      | _      -> TC.fail [%here] "binary operation should have 2 arguments"
    in
    let statement_of_function_call () =
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
          (*
             We identified receiver_identifier as a constructor for a variant.
             Function call needs to be translated to variant value construction.
          *)
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
                | Val Unit                     -> []
                | BinaryOperation (Pair, x, y) -> List.concat [flatten_fields x; flatten_fields y]
                | _                            -> [expression]
              in
              let fields =
                flatten_fields argument
              in
              let variant : Ast.Expression.t =
                Variant {
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
          let call_statement =
            Ast.Statement.Call (receiver_identifier', argument_expressions)
          in
          (* Warn user when polymorphic function was called *)
          let* () =
            let* called_function_type_constraint =
              TC.lookup_definition_opt (Ast.Definition.Select.top_level_type_constraint_definition_named receiver_identifier')
            in
            match called_function_type_constraint with
            | Some called_function_type_constraint -> begin
                if
                  called_function_type_constraint.polymorphic
                then begin
                  let message = lazy begin
                    PP.vertical [
                      PP.format "Call to polymorphic function detected";
                      PP.description_list [
                        (
                          PP.string "Function name",
                          PP.string @@ Ast.Identifier.to_string receiver_identifier'
                        );
                        (
                          PP.string "Location",
                          PP.string @@ StringOf.Sail.location location
                        );
                        (
                          PP.string "Argument types",
                          PP.indent begin
                            PP.numbered_list begin
                              List.map ~f:(Fn.compose FExpr.pp Ast.Type.to_fexpr) argument_expression_types
                            end
                          end
                        );
                      ]
                    ]
                  end
                  in
                  TC.log [%here] Logging.warning message
                end
                else
                  TC.return ()
              end
            | None -> begin
                let message = lazy begin
                  PP.format "Could not determine whether function %s is polymorphic: no corresponding function definition found" (Ast.Identifier.to_string receiver_identifier')
                end
                in
                TC.log [%here] Logging.warning message
              end
          in
          TC.return @@ wrap call_statement
        end
    in

    match Ast.Identifier.to_string receiver_identifier' with
    | "sail_cons" -> statement_of_binary_operation Cons
    | "add_atom"  -> statement_of_binary_operation Plus
    | "sub_atom"  -> statement_of_binary_operation Minus
    | "mult_atom" -> statement_of_binary_operation Times
    | "lt_int"    -> statement_of_binary_operation @@ StandardComparison LessThan
    | "lteq_int"  -> statement_of_binary_operation @@ StandardComparison LessThanOrEqualTo
    | "gt_int"    -> statement_of_binary_operation @@ StandardComparison GreaterThan
    | "gteq_int"  -> statement_of_binary_operation @@ StandardComparison GreaterThanOrEqualTo
    | "eq_int"    -> statement_of_binary_operation EqualTo
    | "neq_int"   -> statement_of_binary_operation NotEqualTo
    | _           -> statement_of_function_call ()

  and statement_of_let
        (_mutability : Libsail.Ast_util.mut)
        (identifier  : S.id                )
        (typ1        : S.typ               )
        (expression  : S.typ S.aexp        )
        (body        : S.typ S.aexp        )
        (_typ2       : S.typ               )
    =
    let* id'   = Identifier.translate_identifier [%here] identifier
    and* typ1' = Nanotype.nanotype_of_sail_type typ1
    and* s1    = statement_of_aexp expression
    and* s2    = statement_of_aexp body
    in
    TC.return begin
      Ast.Statement.Let {
        binder                 = id';
        binding_statement_type = typ1';
        binding_statement      = s1;
        body_statement         = s2;
      }
    end

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
    and* when_true =
      statement_of_aexp then_clause
    and* when_false =
      statement_of_aexp else_clause
    and* condition_variable =
      TC.generate_unique_identifier ()
    in
    let match_pattern =
      Ast.Statement.MatchBool {
        condition = condition_variable;
        when_true;
        when_false
      }
    in
    TC.return begin
      wrap_in_named_statements_context condition_named_statements begin
        Ast.Statement.Let {
          binder                 = condition_variable;
          binding_statement_type = Ast.Type.Bool;
          binding_statement      = condition;
          body_statement         = Ast.Statement.Match match_pattern;
        }
      end
    end

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
        (_typ     : S.typ                  ) : Ast.Statement.t TC.t
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
      let* binder =
        TC.generate_unique_identifier ~prefix:("updated_" ^ (Ast.Identifier.to_string field_identifier)) ()
      in
      (* Convert assigned expression to statement *)
      let* (field_type, named_statement) =
        let* expression, expression_type, named_statements = expression_of_aval location value
        in
        TC.return @@ (expression_type, wrap_in_named_statements_context named_statements (Ast.Statement.Expression expression))
      in
      let named_statements' =
        (binder, field_type, named_statement) :: named_statements
      in
      let field_map' =
        StringMap.overwrite ~key:field_identifier ~data:binder field_map
      in
      TC.return @@ (field_map', named_statements')
    in
    with_destructured_record location aval @@ fun { record_type_identifier; fields; binders; _ } -> begin
      let field_identifiers = List.map ~f:fst fields
      in
      let* field_map, named_statements =
        let initial_field_map : Ast.Identifier.t Ast.Identifier.Map.t =
          Ast.Identifier.Map.of_alist_exn @@ List.zip_exn field_identifiers binders
        in
        TC.fold_left ~f:process_binding ~init:(initial_field_map, []) (Bindings.bindings bindings)
      in
      let record_expression =
        let type_identifier = record_type_identifier
        and fields =
          List.map field_identifiers ~f:(StringMap.find_exn field_map)
        in
        Ast.Expression.Record { type_identifier; fields }
      in
      TC.return @@ wrap_in_named_statements_context named_statements (Expression record_expression)
    end

  and statement_of_assignment
        (lhs : Libsail.Ast.typ Libsail.Anf.alexp)
        (rhs : Libsail.Ast.typ Libsail.Anf.aexp ) : Ast.Statement.t TC.t
    =
    match lhs with
    | AL_id (id, lhs_type) -> begin
        let* id_in_lhs = Identifier.translate_identifier [%here] id
        in
        let* is_register = TC.is_register id_in_lhs
        in
        if is_register
        then begin
          (*
               r1 = expr

             becomes

               (
                 let x = expr
                 in
                 r1 = x
               );
               ()

             This is necessary for the following reasons:
             - Sail does not make the distinction between expressions and statements, whereas muSail does.
               We first need to evaluate expr, store it in a variable, and then assign its value to the register.
             - In Sail, the type of assignment is unit, but not in muSail.
               Adding an explicit unit keeps the types the same.
           *)
          let* rhs_identifier = TC.generate_unique_identifier ()
          and* translated_rhs = statement_of_aexp rhs
          and* rhs_type       = Nanotype.nanotype_of_sail_type lhs_type;
          in
          let write_register_translation =
            Ast.Statement.Let {
              binder                 = rhs_identifier;
              binding_statement_type = rhs_type;
              binding_statement      = translated_rhs;
              body_statement         = Ast.Statement.WriteRegister { register_identifier = id_in_lhs; written_value = rhs_identifier }
            }
          in
          TC.return begin
            Ast.Statement.Seq (
              write_register_translation,
              Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit)
            )
          end
        end
        else begin
            let message =
              Printf.sprintf "assignment to local variable (nonregister) %s" (Ast.Identifier.to_string id_in_lhs)
            in
            TC.not_yet_implemented ~message [%here] location
        end
      end
    | AL_addr (_, _)  -> TC.not_yet_implemented [%here] location
    | AL_field (_, _) -> TC.not_yet_implemented [%here] location

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
    let* if_statement =
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
  | AE_match (aval, cases, _)                                     -> statement_of_match aval cases
  | AE_block (statements, last_statement, typ)                    -> statement_of_block statements last_statement typ
  | AE_field (aval, field_identifier, field_type)                 -> statement_of_field_access aval field_identifier field_type
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
   | Pat_when (_, _, _) -> TC.not_yet_implemented [%here] location
   | Pat_exp (parameter_bindings, raw_body) -> begin
       let body        = S.anf raw_body
       and return_type = Libsail.Type_check.typ_of raw_body
       in
       TC.return @@ {
         identifier;
         parameter_bindings;
         body;
         return_type
       }
     end


let translate_body body =
  let* body' = statement_of_aexp body
  in
  TC.return body'


let translate_function_definition
    (full_sail_definition  : Sail.sail_definition         )
    (definition_annotation : Sail.definition_annotation   )
    (function_definition   : Sail.type_annotation S.fundef) : Ast.Definition.t TC.t
  =
  TC.translation_block [%here] (PP.string "Translating function") begin
    let S.FD_aux ((FD_function (_rec_opt, _tannot_opt, funcls)), (_location, type_annotation)) = function_definition
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
        let* () =
          let S.Typ_annot_opt_aux (unwrapped_tannot_opt, _tannot_location) = _tannot_opt
          in
          let* tannot_opt_message =
            match unwrapped_tannot_opt with
            | Typ_annot_opt_none -> TC.return @@ PP.string "None"
            | Typ_annot_opt_some (type_quantifier, typ) -> begin
                let* _type_quantifier' = TypeQuantifier.translate_type_quantifier type_quantifier
                and* _typ'             = Nanotype.nanotype_of_sail_type typ
                in
                TC.return @@ PP.string "Some"
              end
          in
          let* type_annotation_message =
            TC.return @@ PP.string @@ StringOf.Sail.type_annotation type_annotation
          in
          let message = lazy begin
            let properties =
              PP.description_list [
                (PP.string "Function name", PP.string @@ Ast.Identifier.to_string function_name);
                (PP.string "Original", PP.from_multiline_string @@ StringOf.Sail.definition full_sail_definition);
                (PP.string "type_annotation", type_annotation_message);
                (PP.string "tannot_opt", tannot_opt_message);
              ]
            in
            PP.vertical [
              PP.format "Translated function %s" (Ast.Identifier.to_string function_name);
              PP.indent properties;
            ]
          end
          in
          TC.log [%here] Logging.info message
        in
        let* polymorphic =
          let* top_level_type_constraint =
            TC.lookup_definition_opt (Ast.Definition.Select.top_level_type_constraint_definition_named function_name)
          in
          match top_level_type_constraint with
          | Some top_level_type_constraint -> begin
              TC.return top_level_type_constraint.polymorphic
            end
          | None -> begin
              let* () =
                let message = lazy begin
                  PP.format "No top level type constraint found for function %s; assuming it is not polymorphic" (Ast.Identifier.to_string function_name)
                end
                in
                TC.log [%here] Logging.warning message
              in
              TC.return false
            end
        in
        let function_body =
          Ast.Simplify.simplify_statement function_body
        in
        let* () =
          if polymorphic
          then begin
            let message = lazy begin
              PP.format "Encountered polymorphic function %s" (Ast.Identifier.to_string function_name)
            end
            in
            TC.log [%here] Logging.warning message
          end
          else TC.return ()
        in
        TC.return @@ Ast.Definition.FunctionDefinition {
          function_name;
          function_type = {
            parameters;
            return_type;
          };
          extended_function_type;
          function_body;
          polymorphic;
        }
      end
    | _ -> TC.not_yet_implemented [%here] definition_annotation.loc
  end
