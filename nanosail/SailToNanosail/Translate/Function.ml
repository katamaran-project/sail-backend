open ExtBase

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC        = TranslationContext
module Bindings  = Libsail.Ast_compare.Bindings

open Monads.Notations.Star(TC)


let sail_type_of_lvar
      (lvar : S.typ S.lvar)
      (loc  : S.l         ) : S.typ TC.t
  =
  match lvar with
  | Register t   -> TC.return t
  | Enum t       -> TC.return t
  | Local (_, t) -> TC.return t
  | Unbound _    -> TC.not_yet_implemented [%here] loc


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
  let body_statement : Ast.Statement.t =
    Match begin
      MatchBool {
        condition = condition_variable;
        when_true;
        when_false
      }
    end
  in
  TC.return begin
    Ast.Statement.Let {
      binder                 = condition_variable;
      binding_statement_type = Bool;
      binding_statement      = condition;
      body_statement;
    }
  end


(*
   Translates a reference to an identifier that Sail considers local (Local in lvar).
   Sail also uses Local for references to top-level lets: those are translated
   as references to the top-level value (exp_val <type> <identifier> in muSail).
*)
let expression_of_local_identifier
    (identifier : Ast.Identifier.t)
    (typ        : Ast.Type.t      )
    (location   : S.l             ) : Ast.Expression.t TC.t
  =
  let* refers_to_top_level_value = TC.refers_to_top_level_value identifier
  in
  match refers_to_top_level_value with
  | Some true  -> TC.return @@ Ast.Expression.TopLevelValue (identifier, typ)
  | Some false -> TC.return @@ Ast.Expression.Variable (identifier, typ)
  | None       -> begin
      let message =
        Printf.sprintf
          "%s is both a top-level value and a local variable of this function (shadowing)"
          (Ast.Identifier.to_string identifier)
      in
      TC.not_yet_implemented ~message [%here] location
    end


(*
   Translates a read of the register with the given identifier and (Sail) type.
   A register that the configuration excludes (exclude-registers) is not in Reg:
   reading it yields an unconstrained value of its type instead.
*)
let statement_of_register_read
    (identifier : Ast.Identifier.t)
    (typ        : S.typ           ) : Ast.Statement.t TC.t
  =
  if
    Configuration.is_excluded_register (Ast.Identifier.to_string identifier)
  then
    let* typ' = Type.translate_type typ
    in
    TC.return @@ Ast.Statement.Unconstrained typ'
  else
    TC.return @@ Ast.Statement.ReadRegister identifier


let statement_of_lvar
    (identifier : Ast.Identifier.t     )
    (lvar       : S.typ S.Ast_util.lvar)
    (location   : S.l                  ) : Ast.Statement.t TC.t
  =
  match lvar with
  | Local (_mutability, typ) -> begin
      let* typ' = Type.translate_type typ
      in
      let* expression = expression_of_local_identifier identifier typ' location
      in
      TC.return @@ Ast.Statement.Expression expression
    end
  | Register typ -> statement_of_register_read identifier typ
  | Enum _       -> TC.not_yet_implemented [%here] location
  | Unbound _    -> TC.not_yet_implemented [%here] location


let translate_return_type (sail_type : Libsail.Ast.typ) : Ast.Type.t TC.t =
  let* translation = Type.translate_type sail_type
  in
  TC.return @@ Ast.Type.simplify translation


let translate_parameter_bindings (pattern : Libsail.Type_check.tannot S.pat) : (Ast.Identifier.t * Ast.Type.t) list TC.t =
  let rec translate
      (recurse : bool                           )
      (pattern : Libsail.Type_check.tannot S.pat) : (Ast.Identifier.t * Ast.Type.t) list TC.t
    =
    let S.P_aux (unwrapped_pattern, ((location, _annotation) as annotation)) = pattern
    in
    match unwrapped_pattern with
    | P_lit (L_aux (lit, _loc)) ->
      begin
        match lit with
        | L_unit     -> TC.return @@ [(Ast.Identifier.mk "()", Ast.Type.Unit)] (* todo rather ugly *)
        | L_true     -> TC.not_yet_implemented [%here] location
        | L_false    -> TC.not_yet_implemented [%here] location
        | L_num _    -> TC.not_yet_implemented [%here] location
        | L_hex _    -> TC.not_yet_implemented [%here] location
        | L_bin _    -> TC.not_yet_implemented [%here] location
        | L_string _ -> TC.not_yet_implemented [%here] location
        | L_real _   -> TC.not_yet_implemented [%here] location
      end
    | P_id id -> begin
        let* identifier = Identifier.translate_identifier [%here] id in
        let* typ = Type.translate_type @@ Libsail.Type_check.typ_of_annot annotation
        in
        TC.return [(identifier, Ast.Type.simplify typ)]
      end
    | P_tuple pats -> begin
        if
          recurse
        then
          let* pats' = TC.map ~f:(translate false) pats
          in
          TC.return @@ List.concat pats'
        else
          TC.not_yet_implemented [%here] location
      end
    | P_wild -> begin
        let* typ = Type.translate_type @@ Libsail.Type_check.typ_of_annot annotation
        and* id  = TC.generate_unique_identifier ~underscore:true ()
        in
        TC.return [(id, Ast.Type.simplify typ)]
      end
    | P_typ (_typ, pattern)       -> translate recurse pattern (* parameter is annotated with type, e.g., function foo(x : int) = { } *)
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
    | P_struct (_, _, _)          -> TC.not_yet_implemented [%here] location
  in
  translate true pattern



type named_statement =
  {
    identifier : Ast.Identifier.t;
    typ        : Ast.Type.t;
    statement  : Ast.Statement.t;
  }


(*
   Flattens a list of lists of named statements in a list of statements.
   Checks that there are no name clashes.
*)
let flatten_named_statements (named_statements : named_statement list list) : named_statement list =
  let flattened : named_statement list =
    List.concat named_statements
  in
  let statement_names : Ast.Identifier.t list =
    List.map ~f:(fun s -> s.identifier) flattened
  in
  if
    List.contains_dup statement_names ~compare:Ast.Identifier.compare
  then
    failwith "BUG: two statements bear the same name"
  else
    flattened


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

  (expr, t, [ns1; ns2])

  should be interpreted as

    let ns1.identifier : ns1.typ = ns1.statement in
    let ns2.identifier : ns2.typ = ns2.statement in
    expr

  with expr : t
 *)
let rec expression_of_aval
    (location : S.l         )
    (value    : S.typ S.aval) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
  =
  let expression_of_tuple (elements : S.typ S.aval list) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t =
    if
      List.is_empty elements
    then
      TC.fail [%here] "Encountered empty tuple; should not occur"
    else begin
        let* translation_triples    = TC.map ~f:(expression_of_aval location) elements
        in
        let translation_expressions = List.map ~f:(fun (x, _, _) -> x) translation_triples
        and translation_types       = List.map ~f:(fun (_, x, _) -> x) translation_triples
        and translation_statements  = List.map ~f:(fun (_, _, x) -> x) translation_triples
        in
        (*
           A pair is muSail's bop.pair (ty.prod); a longer tuple is exp_tuple (ty.tuple), as its type says.
           (Longer tuples used to be nested pairs, of type ty.prod (ty.prod ...), which did not match ty.tuple.)
        *)
        let resulting_expression : Ast.Expression.t option =
          match translation_expressions with
          | []       -> None
          | [ x ]    -> Some x
          | [ x; y ] -> Some (Ast.Expression.BinaryOperation (Pair, x, y))
          | elements -> Some (Ast.Expression.Tuple elements)
        in
        let* expression_type =
          match translation_types with
          | []       -> TC.fail [%here] "should not occur"
          | [_]      -> TC.fail [%here] "should not occur"
          | _        -> TC.return @@ Ast.Type.Tuple translation_types
        in
        match resulting_expression with
        | Some resulting_expression -> TC.return (resulting_expression, expression_type, flatten_named_statements translation_statements)
        | None                      -> TC.fail [%here] "should not occur"
      end

  and expression_of_literal
        (literal : S.lit)
        (typ     : S.typ) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
    =
    let* typ' = Type.translate_type typ
    in
    match Literal.bits_of_literal literal with
    | Some bits when not (S.is_bit_typ typ) -> begin
        (*
           Bitvector literals used to be rewritten by Sail into vectors of bits.
           We produce the same translation as for such vectors (see expression_of_vector).
        *)
        let bit_expression (bit : bool) = Ast.Expression.Value (Ast.Value.Bit bit)
        in
        TC.return @@ (Ast.Expression.Bitvector (List.map ~f:bit_expression bits), typ', [])
      end
    | _ -> begin
        let* lit' = Literal.value_of_literal literal typ
        in
        TC.return @@ (Ast.Expression.Value lit', typ', [])
      end

  and expression_of_identifier
        (identifier : S.id                       )
        (lvar       : S.typ Libsail.Ast_util.lvar) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
    =
    let* id' = Identifier.translate_identifier [%here] identifier
    in
    match lvar with
    | Local (_, typ) -> begin
        let* typ' =
          Type.translate_type typ
        in
        let* expression =
          expression_of_local_identifier id' typ' location
        in
        TC.return (expression, typ', [])
      end
    | Register typ -> begin
        let* typ' = Type.translate_type typ
        in
        let* unique_id =
          let prefix = Printf.sprintf "reg_%s_" (Ast.Identifier.to_string id')
          in
          TC.generate_unique_identifier ~prefix ()
        in
        let* statement = statement_of_register_read id' typ
        in
        let named_statements : named_statement list =
          [
            {
              identifier = unique_id;
              typ        = typ';
              statement;
            }
          ]
        in
        TC.return (Ast.Expression.Variable (unique_id, typ'), typ', named_statements)
      end
    | Enum typ -> begin
        let* typ' = Type.translate_type typ
        in
        let* type_identifier =
          match typ' with
          Enum type_identifier -> TC.return type_identifier
          | _                  -> TC.fail [%here] "Expected enum type"
        in
        let enum_value =
          Ast.Expression.Enum { type_identifier; constructor_identifier = id' }
        in
        TC.return (enum_value, typ', [])
      end
    | Unbound _    -> TC.not_yet_implemented [%here] location

  and expression_of_list
        (list : S.typ S.aval list)
        (typ : S.typ             ) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
    =
    let* translation_triples = TC.map ~f:(expression_of_aval location) list
    and* typ'                = Type.translate_type typ
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
        (typ      : S.typ                  ) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
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
      (typ    : S.typ            ) : (Ast.Expression.t * Ast.Type.t * named_statement list) TC.t
    =
    let* values', value_types', named_statements' =
      let* triples =
        TC.map ~f:(expression_of_aval location) values
      in
      let values' =
        List.map ~f:Tuple.Triple.first triples
      and types' =
        List.map ~f:Tuple.Triple.second triples
      and named_statements' =
        flatten_named_statements @@ List.map ~f:Tuple.Triple.third triples
      in
      TC.return (values', types', named_statements')
    in
    let* expression' =
      (* In Sail, bit is bitvector(1), so elements can also have the type bitvector(1) *)
      let is_bit (t : Ast.Type.t) : bool =
        let t = Ast.Type.simplify @@ Ast.Type.strip_aliases t
        in
        Ast.Type.equal t Ast.Type.Bit
        ||
        Ast.Type.equal t (Ast.Type.Bitvector (Ast.Numeric.Expression.Constant Z.one))
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
      Type.translate_type typ
    in
    TC.return @@ (expression', typ', named_statements')

  in
  match value with
  | AV_tuple elements         -> expression_of_tuple elements
  | AV_lit (literal, typ)     -> expression_of_literal literal typ
  | AV_id (name, lvar)        -> expression_of_identifier (Sail.id_of_name name) lvar
  | AV_list (list, typ)       -> expression_of_list list typ
  | AV_record (bindings, typ) -> expression_of_record bindings typ
  | AV_vector (values, typ)   -> expression_of_vector values typ
  | AV_ref (_, _)             -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)            -> TC.not_yet_implemented [%here] location
  | AV_abstract (_, _)        -> TC.not_yet_implemented [%here] location
  | AV_undef _                -> TC.not_yet_implemented [%here] location


and translate_bindings
    (location : S.l                    )
    (bindings : S.typ S.aval Bindings.t) : (Ast.Identifier.t Ast.Identifier.Map.t * named_statement list) TC.t
  =
  let rec process_bindings
      (binding_pairs    : (S.id * S.typ S.aval) list           )
      (mapping          : Ast.Identifier.t Ast.Identifier.Map.t)
      (named_statements : named_statement list                 ) : (Ast.Identifier.t Ast.Identifier.Map.t * named_statement list) TC.t
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
        let extra_named_statement : named_statement =
          {
            identifier = fresh_id;
            typ        = value_type';
            statement  = Ast.Statement.Expression value'
          }
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


let find_monomorphization
    (polymorphic_function_identifier : Ast.Identifier.t)
    (argument_types                  : Ast.Type.t list ) : Ast.Definition.Function.t option TC.t
  =
  let* polymorphic_function_definition =
    TC.lookup_definition_opt (Ast.Definition.Select.function_definition_named polymorphic_function_identifier)
  in
  match polymorphic_function_definition with
  | Some polymorphic_function_definition -> begin
      let has_required_parameter_types (function_definition : Ast.Definition.Function.t) : bool =
        let parameter_types = List.map ~f:snd function_definition.function_type.parameters
        in
        List.equal Ast.Type.equal argument_types parameter_types
      in
      TC.return begin
        List.find polymorphic_function_definition.monomorphs ~f:has_required_parameter_types
      end
    end
  | None -> TC.return None

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
      (named_statements : named_statement list)
      (statement        : Ast.Statement.t     ) : Ast.Statement.t
  =
  match named_statements with
  | { identifier = binder; typ = binding_statement_type; statement = binding_statement }::rest -> begin
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
  (* identifier of variable containing record *)
  record_identifier      : Ast.Identifier.t;

  (* name of the record type *)
  record_type_identifier : Ast.Identifier.t;

  (* field names and types *)
  fields                 : (Ast.Identifier.t * Ast.Type.t) list;

  (* variables bound to each field *)
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
        Identifier.translate_identifier [%here] @@ Sail.id_of_name record_identifier
      and* S.Typ_aux (t, _loc) =
        sail_type_of_lvar lvar location
      in
      match t with
      | Typ_id record_type_identifier -> begin
          (* The record's type can be an alias of the record type (e.g., type regtype = Capability) *)
          let* record_type_identifier =
            let* translated_record_type_identifier =
              Identifier.translate_identifier [%here] record_type_identifier
            and* translated_type =
              Type.translate_type (S.Typ_aux (t, _loc))
            in
            match Ast.Type.strip_aliases translated_type with
            | Record record_type_identifier -> TC.return record_type_identifier
            | Int _
            | Bool
            | String
            | Bit
            | List _
            | Unit
            | Enum _
            | Bitvector _
            | Tuple _
            | Sum (_, _)
            | Variant _
            | Application (_, _)
            | Alias (_, _)
            | Range (_, _)
            | Function _
            | TypeVariable _
            | Nat
            | Vector (_, _)
            | Implicit _                    -> TC.return translated_record_type_identifier
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
          TC.return begin
            Ast.Statement.DestructureRecord {
              record_type_identifier;
              field_identifiers;
              binders;
              destructured_record;
              body
            }
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
  | AV_lit (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_ref (_, _)      -> TC.not_yet_implemented [%here] location
  | AV_tuple _         -> TC.not_yet_implemented [%here] location
  | AV_list (_, _)     -> TC.not_yet_implemented [%here] location
  | AV_vector (_, _)   -> TC.not_yet_implemented [%here] location
  | AV_record (_, _)   -> TC.not_yet_implemented [%here] location
  | AV_cval (_, _)     -> TC.not_yet_implemented [%here] location
  | AV_abstract (_, _) -> TC.not_yet_implemented [%here] location
  | AV_undef _         -> TC.not_yet_implemented [%here] location


(*
   Whether a called function is the builtin early_return, to which Sail's early_return rewrite
   turns the returns that it cannot remove (see statement_of_application).
*)
let is_early_return (function_identifier : S.id) : bool =
  String.equal (Libsail.Ast_util.string_of_id function_identifier) "early_return"


(*
   Translate Sail expression to statements.

   The expression_type parameter is the type of the expression, if it is known.
   This seemingly useless parameter was introduced because

   * We translate Sail-calls to exit to muSail-failures.
   * muSail-failures expect a type, which we want to provide explicitly.
     (Originally this was translated as a wildcard so as to let Coq infer its value,
     but it was deemed necessary to make it explicit.)
   * The type information contained in the Sail-AST seems
     to insist that a call to exit always has type unit,
     even in contexts where other types are expected.
     For example, given the Sail code "if cond then 0 else exit(())",
     one would expect exit to be polymorphic and to return an int
     in this case, but this is contradicted by the information provided us by Sail.
     It seems that the Sail type checker understands exit
     and tolerates type inconsistencies.

   So, in order to generate the correct type, we must carry around
   the typing context ourselves.

   The reason expression_type is "S.typ option" instead of just "S.typ":
   it was once unclear what the expected types of a and b
   in a sequence "a; b; c" should be (they are unit, see statement_of_block).
*)
(*
   A branch in which Sail's flow typing fixes size variables, such as the then-branch of
   if width == 4 for width : int('n), with 'p == 8 * 'n among the function's constraints:
   Sail's type checker added 'n == 4 to the branch's environment, and its solver finds 'p == 32.
   The branch becomes a Specialize: its translation with each fixed variable replaced by its value,
   the variables in scope whose type mentions one (rebound with a cast in Rocq), and the branch's type.
   Only the size variables of the types of the variables in scope are considered (the solver is not
   free); a variable the enclosing environment already fixes is left alone.
*)
(*
   A width that is not a constant nor a size variable, such as bits(8 * ('p8_times_n / 8)) in read_ram,
   or bits(8 * 'n) where the signature has bits('p8_times_n) with 'p8_times_n == 8 * 'n, cannot be
   identified with the parameter's width by Rocq. When Sail's solver proves, under the constraints in
   the environment, that the width equals a size variable of the function's quantifier, the width is
   that variable. Sail's existential variables (named with a #) are not candidates.
*)
let rec normalise_widths
    (environment : Libsail.Type_check.Env.t)
    (typ         : S.typ                   ) : S.typ
  =
  let candidates : S.kid list =
    Libsail.Ast_compare.KBindings.bindings (Libsail.Type_check.Env.get_typ_vars environment)
    |> List.filter_map ~f:(fun (kid, kind) ->
        match kind with
        | S.K_int when not (String.contains (Libsail.Ast_util.string_of_kid kid) '#') -> Some kid
        | K_int | K_type | K_bool -> None
      )
  in
  let normalise_nexp (nexp : S.nexp) : S.nexp =
    let S.Nexp_aux (unwrapped_nexp, _) = nexp
    in
    match unwrapped_nexp with
    | Nexp_constant _ | Nexp_var _ | Nexp_id _ -> nexp
    | Nexp_app _ | Nexp_times _ | Nexp_sum _ | Nexp_minus _ | Nexp_neg _ | Nexp_exp _ | Nexp_if _ -> begin
        let in_nexp = Libsail.Ast_util.tyvars_of_nexp nexp
        in
        (* only when every size variable of the width is a candidate, so that the solver has the constraints *)
        if
          Libsail.Ast_compare.KidSet.is_empty in_nexp
          || not (Libsail.Ast_compare.KidSet.for_all (fun kid -> List.mem candidates kid ~equal:(fun a b -> Libsail.Ast_compare.Kid.compare a b = 0)) in_nexp)
        then
          nexp
        else
          match
            List.find candidates ~f:(fun kid ->
                try Libsail.Type_check.prove __POS__ environment (Libsail.Ast_util.nc_eq nexp (Libsail.Ast_util.nvar kid))
                with _ -> false)
          with
          | Some kid -> Libsail.Ast_util.nvar kid
          | None     -> nexp
      end
  in
  let S.Typ_aux (unwrapped_typ, location) = typ
  in
  let normalise_argument (S.A_aux (argument, argument_location) : S.typ_arg) : S.typ_arg =
    match argument with
    | A_nexp nexp -> S.A_aux (A_nexp (normalise_nexp nexp), argument_location)
    | A_typ typ   -> S.A_aux (A_typ (normalise_widths environment typ), argument_location)
    | A_bool _    -> S.A_aux (argument, argument_location)
  in
  match unwrapped_typ with
  | Typ_app (S.Id_aux (Id ("atom" | "range" | "atom_bool" | "implicit" | "itself"), _), _) -> typ
  | Typ_app (identifier, arguments) -> S.Typ_aux (Typ_app (identifier, List.map ~f:normalise_argument arguments), location)
  | Typ_tuple types                 -> S.Typ_aux (Typ_tuple (List.map ~f:(normalise_widths environment) types), location)
  | Typ_internal_unknown | Typ_id _ | Typ_var _ | Typ_fn _ | Typ_bidir _ | Typ_exist _ -> typ


let specialise_branch
    ~(outer_environment : Libsail.Type_check.Env.t)
    ~(branch            : S.typ S.aexp            )
    ~(branch_type       : S.typ                   )
    (statement          : Ast.Statement.t         ) : Ast.Statement.t TC.t
  =
  let S.AE_aux (_, branch_annotation) = branch
  in
  let inner_environment = branch_annotation.env
  in
  let locals : (S.id * S.typ) list =
    List.map ~f:(fun (identifier, (_mutability, typ)) -> (identifier, typ)) @@ Bindings.bindings @@ Libsail.Type_check.Env.get_locals inner_environment
  in
  let outer_size_variables = Libsail.Type_check.Env.get_typ_vars outer_environment
  in
  let candidates : S.kid list =
    List.concat_map locals ~f:(fun (_, typ) -> Libsail.Ast_compare.KidSet.elements @@ Libsail.Ast_util.tyvars_of_typ typ)
    |> List.dedup_and_sort ~compare:Libsail.Ast_compare.Kid.compare
    |> List.filter ~f:(fun kid ->
        match Libsail.Ast_compare.KBindings.find_opt kid outer_size_variables with
        | Some K_int                -> true
        | Some (K_type | K_bool) | None -> false
      )
  in
  let fixed : (S.kid * Z.t) list =
    List.filter_map candidates ~f:(fun kid ->
        match Libsail.Type_check.solve_unique outer_environment (Libsail.Ast_util.nvar kid) with
        | Some _ -> None
        | None   -> begin
            match Libsail.Type_check.solve_unique inner_environment (Libsail.Ast_util.nvar kid) with
            | Some value -> Some (kid, value)
            | None       -> None
          end
      )
  in
  (*
     The facts: the constraints Sail's flow typing added in the branch (if 'n <= 8 ...),
     over the candidate size variables only.
  *)
  let* facts : Ast.Numeric.Constraint.t list =
    let outer_constraints = Libsail.Type_check.Env.get_constraints outer_environment
    in
    let is_candidate (kid : S.kid) : bool =
      List.mem candidates kid ~equal:(fun kid kid' -> Libsail.Ast_compare.Kid.compare kid kid' = 0)
    in
    Libsail.Type_check.Env.get_constraints inner_environment
    |> List.filter ~f:(fun numeric_constraint ->
        not (List.exists outer_constraints ~f:(fun numeric_constraint' -> Libsail.Ast_compare.NC.compare numeric_constraint numeric_constraint' = 0)))
    |> List.concat_map ~f:TypeQuantifier.sail_conjuncts
    |> List.filter ~f:(fun numeric_constraint ->
        let kids = Libsail.Ast_compare.KidSet.elements @@ Libsail.Ast_util.tyvars_of_constraint numeric_constraint
        in
        not (List.is_empty kids) && List.for_all kids ~f:is_candidate
      )
    |> TypeQuantifier.translate_conjuncts
  in
  match fixed, facts with
  | [], [] -> TC.return statement
  | _, _ -> begin
      let equalities : (Ast.Identifier.t * Z.t) list =
        List.map fixed ~f:(fun (kid, value) -> (Identifier.translate_kind_identifier kid, value))
      in
      let body =
        Ast.Statement.substitute_numeric_expression_identifier (Ast.Numeric.size_variable_substitution equalities) statement
      in
      let free_variables = Ast.Statement.free_variables body
      in
      let mentions_fixed (typ : S.typ) : bool =
        List.exists fixed ~f:(fun (kid, _) -> Libsail.Ast_compare.KidSet.mem kid (Libsail.Ast_util.tyvars_of_typ typ))
      in
      let* rebinds : (Ast.Identifier.t * Ast.Type.t) list =
        TC.filter_map locals ~f:(fun (identifier, typ) ->
            if
              mentions_fixed typ
            then
              TC.recover
                begin
                  let* identifier' = Identifier.translate_identifier [%here] identifier
                  in
                  if
                    Ast.Identifier.Set.mem free_variables identifier'
                  then
                    let* typ' = Type.translate_type typ
                    in
                    TC.return @@ Some (identifier', typ')
                  else
                    TC.return None
                end
                (fun _ -> TC.return None)
            else
              TC.return None
          )
      in
      TC.recover
        begin
          let* result_type = Type.translate_type branch_type
          in
          let facts =
            List.map facts ~f:(Ast.Numeric.Constraint.substitute_numeric_expression_identifier (Ast.Numeric.size_variable_substitution equalities))
          in
          TC.return @@ Ast.Statement.Specialize { equalities; facts; rebinds; result_type; body }
        end
        (fun _ -> TC.return statement)
    end


let rec statement_of_aexp
    (expression_type : S.typ option)
    (expression      : S.typ S.aexp) : Ast.Statement.t TC.t
  =
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
  (*
     A call to a bitfield setter with a register reference, as Sail generates for

       mstatus->MIE() = v

     namely

       _set_Mstatus_MIE(ref mstatus, v)

     where Sail defines the setter as

       function _set_Mstatus_MIE (r_ref, v) = { let r = __deref(r_ref); ( *r_ref) = _update_Mstatus_MIE(r, v) }

     muSail has no register references, but the register is known at the call, so the call is inlined:

       let r  = read_register mstatus in
       let r' = _update_Mstatus_MIE(r, v) in
       write_register mstatus r';
       ()

     Other functions that take a register reference are not supported.
  *)
  let statement_of_register_setter_call
      (setter_identifier   : S.id        )
      (register_identifier : S.id        )
      (register_type       : S.typ       )
      (value               : S.typ S.aval) : Ast.Statement.t TC.t
    =
    let setter_name = Libsail.Ast_util.string_of_id setter_identifier
    in
    match String.chop_prefix setter_name ~prefix:"_set_" with
    | None -> TC.not_yet_implemented ~message:("register reference passed to " ^ setter_name) [%here] location
    | Some field_path -> begin
        let update_identifier = Ast.Identifier.mk ("_update_" ^ field_path)
        in
        let* update_definition =
          TC.lookup_definition_opt (Ast.Definition.Select.function_definition_named update_identifier)
        in
        match update_definition with
        | None -> TC.not_yet_implemented ~message:("register reference passed to " ^ setter_name ^ ", whose update function is not translated") [%here] location
        | Some _ -> begin
            let* register_identifier' = Identifier.translate_identifier [%here] register_identifier
            and* register_type'       = Type.translate_type register_type
            and* value_expression, _value_type, value_named_statements = expression_of_aval location value
            in
            let* read_identifier    = TC.generate_unique_identifier ()
            and* updated_identifier = TC.generate_unique_identifier ()
            in
            let* read_statement = statement_of_register_read register_identifier' register_type
            in
            let write_statement : Ast.Statement.t =
              if
                Configuration.is_excluded_register (Ast.Identifier.to_string register_identifier')
              then
                (* The register is excluded by the configuration (exclude-registers): the value is not written anywhere *)
                Expression (Value Unit)
              else
                Seq (
                  WriteRegister { register_identifier = register_identifier'; written_value = updated_identifier },
                  Expression (Value Unit)
                )
            in
            let statement : Ast.Statement.t =
              Let {
                binder                 = read_identifier;
                binding_statement_type = register_type';
                binding_statement      = read_statement;
                body_statement         = Let {
                    binder                 = updated_identifier;
                    binding_statement_type = register_type';
                    binding_statement      = Call (update_identifier, [ Variable (read_identifier, register_type'); value_expression ]);
                    body_statement         = write_statement;
                  }
              }
            in
            TC.return @@ wrap_in_named_statements_context value_named_statements statement
          end
      end
  in

  (*
     A match with a single case whose pattern is a struct of variables and wildcards,

       match s { struct { foo = x, bar = _, qux = z } => body }

     always matches: it destructures the record, and binds x and z to its fields.
     Returns None for any other match.
  *)
  let statement_of_struct_match
      (matched : S.typ S.aval                                     )
      (cases   : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list) : Ast.Statement.t TC.t option
    =
    let is_true (condition : S.typ S.aexp) : bool =
      match condition with
      | S.AE_aux (AE_val (AV_lit (S.L_aux (L_true, _), _)), _) -> true
      | _                                                      -> false
    in
    let field_binding (field_pattern : S.id * S.typ S.apat) : (S.id * S.Jib.name option) option =
      match field_pattern with
      | (field, S.AP_aux (AP_id (name, _), _)) -> Some (field, Some name)
      | (field, S.AP_aux (AP_wild _, _))       -> Some (field, None)
      | (_, _)                                 -> None
    in
    match cases with
    | [ (S.AP_aux (AP_struct (field_patterns, _), _), condition, body) ] when is_true condition -> begin
        match Option.all (List.map ~f:field_binding field_patterns) with
        | None                -> None
        | Some field_bindings -> Some begin
            with_destructured_record location matched @@ fun { fields; binders; _ } -> begin
              let* body = statement_of_aexp None body
              in
              TC.fold_left (List.rev field_bindings) ~init:body ~f:(fun body (field, name) ->
                  match name with
                  | None      -> TC.return body
                  | Some name -> begin
                      let* field = Identifier.translate_identifier [%here] field
                      and* name  = Identifier.translate_identifier [%here] (Sail.id_of_name name)
                      in
                      match List.findi fields ~f:(fun _ (field', _) -> Ast.Identifier.equal field field') with
                      | None -> TC.fail [%here] @@ Printf.sprintf "record has no field %s" (Ast.Identifier.to_string field)
                      | Some (index, (_, field_type)) -> begin
                          let binder = List.nth_exn binders index
                          in
                          TC.return @@ Ast.Statement.Let {
                            binder                 = name;
                            binding_statement_type = field_type;
                            binding_statement      = Expression (Variable (binder, field_type));
                            body_statement         = body;
                          }
                        end
                    end
                )
            end
          end
      end
    | _ -> None
  in

  let statement_of_match
      (matched     : S.typ S.aval                                     )
      (cases       : (S.typ S.apat * S.typ S.aexp * S.typ S.aexp) list)
      (output_type : S.typ                                            ) : Ast.Statement.t TC.t
    =
    match statement_of_struct_match matched cases with
    | Some statement -> statement
    | None ->
    let* matched_variable =
      TC.generate_unique_identifier ()
    and* output_type =
      Type.translate_type output_type
    and* cases_with_translated_bodies =
      let translate_case_body (pattern, condition, body) =
        let* translated_body = statement_of_aexp (Some output_type) body
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
      Match.translate location matched_variable matched_variable_type cases_with_translated_bodies output_type
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
      fun { record_type_identifier; fields; binders; record_identifier = _ } -> begin
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
      end

  and statement_of_value (value : S.typ S.aval) : Ast.Statement.t TC.t =
    let* expression, _expression_type, named_statements = expression_of_aval location value
    in
    TC.return @@ wrap_in_named_statements_context named_statements @@ Ast.Statement.Expression expression

  and statement_of_application
      (receiver_identifier : S.id             )
      (arguments           : S.typ S.aval list)
      (typ                 : S.typ            ) : Ast.Statement.t TC.t
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
      | _      -> TC.fail [%here] "binary operation should have exactly two arguments"
    in

    let statement_of_unary_operation (operator : Ast.UnaryOperator.t) : Ast.Statement.t TC.t =
      match argument_expressions with
      | [x] -> TC.return @@ wrap @@ Ast.Statement.Expression (UnaryOperation (operator, x))
      | _   -> TC.fail [%here] "unary operation should have exactly one argument"
    in

    let statement_of_comparison (comparison : Ast.BinaryOperator.Comparison.t) : Ast.Statement.t TC.t =
      statement_of_binary_operation @@ StandardComparison comparison
    in

    (*
       Constructing a value of a union that is translated to a sum, e.g.,

         Some(x)

       becomes an injection into the sum (exp_inl x).
       The type of the other side of the sum is taken from the type of the constructed value.
    *)
    let statement_of_injection
        (sum_definition : Ast.Definition.Type.Sum.t)
        (side           : Ast.SumSide.t            ) : Ast.Statement.t TC.t
      =
      let* translated_type = Type.translate_type typ
      in
      match translated_type, argument_expressions with
      | Sum (left_type, right_type), [ value ] -> begin
          TC.return @@ wrap @@ Ast.Statement.Expression (Injection { side; left_type; right_type; value })
        end
      | Sum (_, _), _ -> TC.fail [%here] "Should not happen: constructor fields are packed into one value"
      | _, _ -> begin
          let message =
            Printf.sprintf
              "expected constructor %s to produce a value of a sum type, got %s instead"
              (Ast.Identifier.to_string (Ast.Definition.Type.Sum.constructor sum_definition side).constructor_identifier)
              (Ast.Type.to_string translated_type)
          in
          TC.fail [%here] message
        end
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
      let* variant_construction : (Ast.Identifier.t * Ast.Identifier.t) option =
        let* union_instances = TC.union_instances
        in
        if
          UnionInstances.is_constructor_of_specialised_union union_instances receiver_identifier'
        then begin
          (*
             A constructor of a union with type parameters: the type of the constructed value
             determines which monomorphic copy of the union is constructed (see UnionInstances).
          *)
          let* constructed_type = Type.translate_type typ
          in
          match Ast.Type.strip_aliases constructed_type with
          | Variant instance_identifier -> begin
              let constructor_identifier = UnionInstances.constructor_in union_instances instance_identifier receiver_identifier'
              in
              TC.return @@ Some (instance_identifier, constructor_identifier)
            end
          | Int _ | Bool | String | Bit | List _ | Unit | Enum _ | Bitvector _ | Tuple _ | Sum (_, _) | Record _
          | Application (_, _) | Alias (_, _) | Range (_, _) | Function _ | TypeVariable _ | Nat | Vector (_, _) | Implicit _ -> begin
              let message =
                Printf.sprintf "expected constructor %s to produce a value of a union, got %s instead"
                  (Ast.Identifier.to_string receiver_identifier')
                  (Ast.Type.to_string constructed_type)
              in
              TC.fail [%here] message
            end
        end
        else begin
          let* variant_definition =
            TC.lookup_variant_by_constructor receiver_identifier'
          in
          TC.return @@ Option.map variant_definition ~f:(fun (variant_definition : Ast.Definition.Type.Variant.t) -> (variant_definition.identifier, receiver_identifier'))
        end
      in
      match variant_construction with
      | Some (variant_identifier, constructor_identifier) -> begin
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
                   (x, y, z) : Tuple [x; y; z]

                 flatten_fields extracts the different arguments from these potentially nested pairs.
                 We probably do not deal correctly with the case that one of the fields is an actual pair.

                 todo check why we don't work with tuples instead of nested pairs
              *)
              let rec flatten_fields (expression : Ast.Expression.t) =
                match expression with
                | Value Unit                   -> []
                | Tuple elements               -> elements
                | BinaryOperation (Pair, x, y) -> List.concat [flatten_fields x; flatten_fields y]
                | _                            -> [expression]
              in
              let fields =
                flatten_fields argument
              in
              let variant : Ast.Expression.t =
                Variant {
                  type_identifier        = variant_identifier;
                  constructor_identifier = constructor_identifier;
                  fields                 = fields;
                }
              in
              TC.return @@ wrap @@ Ast.Statement.Expression variant
            end
          | _ -> TC.fail [%here] "Should not happen: variant fields are packed into one value"
        end
      | None -> begin
          (* Function call does not refer to variant constructor *)
          let log_encounter_with_call_to_polymorphic_function (called_function_type_constraint : Ast.Definition.TopLevelTypeConstraint.t) : unit TC.t
            =
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
                    PP.string "Type quantifier",
                    FExpr.pp @@ Ast.TypeQuantifier.to_fexpr called_function_type_constraint.type_quantifier
                  );
                  (
                    PP.string "Parameter types",
                    FExpr.pp @@ Ast.Type.to_fexpr called_function_type_constraint.typ
                  );
                  (
                    PP.string "Argument types",
                    PP.indent begin
                      PP.numbered_list begin
                        List.map ~f:(FExpr.pp <. Ast.Type.to_fexpr) argument_expression_types
                      end
                    end
                  );
                ]
              ]
            end
            in
            TC.log [%here] Logging.warning message

          and log_no_type_constraint_found () =
            let message = lazy begin
              PP.format "Could not determine whether function %s is polymorphic: no corresponding function definition found" (Ast.Identifier.to_string receiver_identifier')
            end
            in
            TC.log [%here] Logging.warning message
          in

          let statement_of_polymorphic_function_call (called_function_type_constraint : Ast.Definition.TopLevelTypeConstraint.t) : Ast.Statement.t TC.t =
            (* store combination of argument types *)
            let* () = TC.register_polymorphic_function_call_type_arguments receiver_identifier' argument_expression_types
            in
            let* monomorph : Ast.Definition.Function.t option =
              find_monomorphization receiver_identifier' argument_expression_types
            in
            match monomorph with
            | Some monomorph -> begin
                (* Monomorph with matching parameter types found *)
                TC.return @@ wrap @@ Call (monomorph.function_name, argument_expressions)
              end
            | None -> begin
                (* No monomorph found, generate call to polymorphic function *)
                let* () = log_encounter_with_call_to_polymorphic_function called_function_type_constraint
                in
                TC.return @@ wrap @@ Call (receiver_identifier', argument_expressions)
              end
          in

          let call_statement : Ast.Statement.t =
            Call (receiver_identifier', argument_expressions)
          in
          (* Look up information about the called function; we need to determine whether it is polymorphic *)
          let* called_function_type_constraint =
            TC.lookup_definition_opt (Ast.Definition.Select.top_level_type_constraint_definition_named receiver_identifier')
          in
          match called_function_type_constraint with
          | Some called_function_type_constraint -> begin
              if
                called_function_type_constraint.polymorphic
              then
                (* called function turned out to be polymorphic *)
                statement_of_polymorphic_function_call called_function_type_constraint
              else
                (* Called function turned out NOT to be polymorphic; no action required *)
                TC.return @@ wrap call_statement
            end
          | None -> begin
              let* () = log_no_type_constraint_found ()
              in
              TC.return @@ wrap call_statement
            end
        end
    in

    (*
       A call to a function that the configuration excludes (exclude-functions-as-no-op and the like):
       the arguments are computed, and the call is replaced as the configuration says.
    *)
    let statement_of_excluded_function_call (exclusion : Configuration.FunctionExclusion.t) : Ast.Statement.t TC.t =
      let receiver_name = Libsail.Ast_util.string_of_id receiver_identifier
      in
      let* result_type = translate_return_type typ
      in
      match exclusion with
      | NoOp -> begin
          match Ast.Type.strip_aliases result_type with
          | Unit -> TC.return @@ wrap @@ Ast.Statement.Expression (Value Unit)
          | Int _ | Bool | String | Bit | List _ | Enum _ | Bitvector _ | Tuple _ | Sum (_, _) | Record _ | Variant _
          | Application (_, _) | Alias (_, _) | Range (_, _) | Function _ | TypeVariable _ | Nat | Vector (_, _) | Implicit _ -> begin
              let message =
                Printf.sprintf
                  "%s is excluded by %s, but this call to it has type %s instead of unit"
                  receiver_name
                  (Configuration.FunctionExclusion.slang_function_name exclusion)
                  (Ast.Type.to_string result_type)
              in
              TC.fail [%here] message
            end
        end
      | Failure -> begin
          let message =
            Printf.sprintf "%s is excluded by the configuration" receiver_name
          in
          TC.return @@ wrap @@ Ast.Statement.Fail (result_type, message)
        end
      | Unconstrained -> TC.return @@ wrap @@ Ast.Statement.Unconstrained result_type
      | Value value -> begin
          (* replace_calls_by_values replaces these calls before the ANF conversion; this one could not be replaced *)
          let message =
            Printf.sprintf
              "%s is excluded by %s, but its replacement %s does not parse, or does not type-check at the type of this call, %s"
              receiver_name
              (Configuration.FunctionExclusion.slang_function_name exclusion)
              value
              (Ast.Type.to_string result_type)
          in
          TC.fail [%here] message
        end
    in

    (*
       Sail's early_return rewrite (see sail_plugin/Rewrites.ml) removes the returns after which
       nothing else runs, and turns each remaining return into a call to the builtin early_return,
       which it declares as an extern. Translating that call as a call (a foreign call, as for any
       extern) would let the rest of the function run after the return, so it is not supported.
    *)
    if is_early_return receiver_identifier
    then TC.not_yet_implemented ~message:"early return that Sail's early_return rewrite could not remove" [%here] location
    else
    match Configuration.function_exclusion (Libsail.Ast_util.string_of_id receiver_identifier) with
    | Some exclusion -> statement_of_excluded_function_call exclusion
    | None ->
    match Ast.Identifier.to_string receiver_identifier' with
    | "sail_cons" -> statement_of_binary_operation Cons
    | "add_atom"  -> statement_of_binary_operation Plus
    | "sub_atom"  -> statement_of_binary_operation Minus
    | "mult_atom" -> statement_of_binary_operation Times
    | "negate_atom"                                                  (* Since Sail 0.20, -1 is parsed as negate_atom(1) *)
    | "negate_int" -> statement_of_unary_operation Neg
    | "lt_int"    -> statement_of_comparison LessThan
    | "lteq_int"  -> statement_of_comparison LessThanOrEqualTo
    | "gt_int"    -> statement_of_comparison GreaterThan
    | "gteq_int"  -> statement_of_comparison GreaterThanOrEqualTo
    | "eq_int"    -> statement_of_binary_operation EqualTo
    | "neq_int"   -> statement_of_binary_operation NotEqualTo
    | _           -> begin
        let* sum_constructor = TC.lookup_sum_by_constructor receiver_identifier'
        in
        match sum_constructor with
        | Some (sum_definition, side) -> statement_of_injection sum_definition side
        | None                        -> statement_of_function_call ()
      end

  and statement_of_let
        (_mutability     : Libsail.Ast_util.mut)
        (identifier      : S.id                )
        (expression_type : S.typ               )
        (expression      : S.typ S.aexp        )
        (body            : S.typ S.aexp        )
        (body_type       : S.typ               ) : Ast.Statement.t TC.t
    =
    let expression_type = normalise_widths annotation.env expression_type
    in
    let* binder                 = Identifier.translate_identifier [%here] identifier
    and* binding_statement_type = Type.translate_type expression_type
    and* binding_statement      = statement_of_aexp (Some expression_type) expression
    and* body_statement         = statement_of_aexp (Some body_type) body
    in
    let translation : Ast.Statement.t =
      Let {
        binder;
        binding_statement_type;
        binding_statement;
        body_statement;
      }
    in
    TC.return translation

  and statement_of_if
        (condition   : S.typ S.aval)
        (then_clause : S.typ S.aexp)
        (else_clause : S.typ S.aexp)
        (output_type : S.typ       ) : Ast.Statement.t TC.t
    =
    let* (condition, condition_named_statements) =
      let* condition_expression, _condition_expression_type, named_statements = expression_of_aval location condition
      in
      TC.return (Ast.Statement.Expression condition_expression, named_statements)
    and* when_true =
      let* when_true = statement_of_aexp (Some output_type) then_clause
      in
      specialise_branch ~outer_environment:annotation.env ~branch:then_clause ~branch_type:output_type when_true
    and* when_false =
      let* when_false = statement_of_aexp (Some output_type) else_clause
      in
      specialise_branch ~outer_environment:annotation.env ~branch:else_clause ~branch_type:output_type when_false
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
        (statements          : S.typ S.aexp list)
        (last_statement      : S.typ S.aexp     )
        (last_statement_type : S.typ            ) : Ast.Statement.t TC.t
    =
    let* translated_statements =
      (*
         In a block "a; b; c", a and b have type unit: Sail's type checker
         checks every statement of a block but the last against unit.
         Passing it on matters for exit, see statement_of_exit.
      *)
      let* all_but_last =
        TC.map ~f:(statement_of_aexp (Some Libsail.Ast_util.unit_typ)) statements
      and* last =
        statement_of_aexp (Some last_statement_type) last_statement
      in
      TC.return @@ List.concat [all_but_last; [last]]
    in
    TC.return @@ Ast.Statement.make_sequence translated_statements

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
        (acc  : Ast.Identifier.t Ast.Identifier.Map.t * named_statement list)
        (pair : S.id * S.typ S.aval                                         ) : (Ast.Identifier.t Ast.Identifier.Map.t * named_statement list) TC.t
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
      let named_statements' : named_statement list =
        {
          identifier = binder;
          typ        = field_type;
          statement  = named_statement
        }  :: named_statements
      in
      let field_map' =
        Map.overwrite ~key:field_identifier ~data:binder field_map
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
      let record_expression : Ast.Expression.t =
        let type_identifier =
          record_type_identifier
        and fields =
          List.map field_identifiers ~f:(Map.find_exn field_map)
        in
        Record { type_identifier; fields }
      in
      TC.return @@ wrap_in_named_statements_context named_statements (Expression record_expression)
    end

  and statement_of_assignment
        (lhs : S.typ S.alexp)
        (rhs : S.typ S.aexp ) : Ast.Statement.t TC.t
    =
    match lhs with
    | AL_id (id, lhs_type) -> begin
        let* id_in_lhs = Identifier.translate_identifier [%here] @@ Sail.id_of_name id
        in
        let* is_register = TC.is_register id_in_lhs
        in
        if
          Configuration.is_excluded_register (Ast.Identifier.to_string id_in_lhs)
        then begin
          (*
             The register is excluded by the configuration (exclude-registers):
             the written value is computed, but not written anywhere.

               r1 = expr

             becomes

               let x : τ = expr in ()

             The let's type fixes the implicit widths of a size-polymorphic call in expr.
          *)
          let* rhs_identifier = TC.generate_unique_identifier ()
          and* translated_rhs = statement_of_aexp None rhs
          and* rhs_type       = Type.translate_type lhs_type
          in
          TC.return @@ Ast.Statement.Let {
            binder                 = rhs_identifier;
            binding_statement_type = rhs_type;
            binding_statement      = translated_rhs;
            body_statement         = Expression (Value Unit);
          }
        end
        else if is_register
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
          and* translated_rhs = statement_of_aexp None rhs (* todo determine type *)
          and* rhs_type       = Type.translate_type lhs_type;
          in
          let write_register_translation : Ast.Statement.t =
            Let {
              binder                 = rhs_identifier;
              binding_statement_type = rhs_type;
              binding_statement      = translated_rhs;
              body_statement         = WriteRegister { register_identifier = id_in_lhs; written_value = rhs_identifier }
            }
          in
          TC.return begin
            Ast.Statement.Seq (
              write_register_translation,
              Expression (Value Unit)
            )
          end
        end
        else begin
          (*
               x = expr

             where x is a mutable local variable (var x = ...) becomes

               stm_assign "x" expr;
               ()

             muSail's local variables are mutable: stm_assign updates the variable bound by the enclosing stm_let.
             As for registers, the explicit unit keeps Sail's type of the assignment (stm_assign's value is the assigned value).
          *)
          let* translated_rhs = statement_of_aexp None rhs
          in
          TC.return begin
            Ast.Statement.Seq (
              AssignVariable { variable_identifier = id_in_lhs; assigned_value = translated_rhs },
              Expression (Value Unit)
            )
          end
        end
      end
    | AL_addr (_, _)  -> TC.not_yet_implemented [%here] location
    | AL_field (_, _) -> TC.not_yet_implemented [%here] location

  and statement_of_short_circuit
        (logical_operator : S.sc_op     )
        (lhs              : S.typ S.aval)
        (rhs              : S.typ S.aexp) : Ast.Statement.t TC.t
    =
    let* lhs_expression, _lhs_expression_type, lhs_named_statements = expression_of_aval location lhs
    and* rhs_statement = statement_of_aexp None rhs  (* todo construct bool type to pass along to statement_of_aexp *)
    in
    let lhs_expr_as_statement : Ast.Statement.t =
      Expression lhs_expression
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
          and when_false = Ast.(Statement.Expression (Value (Bool false)))
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
          let condition  : Ast.Statement.t = lhs_expr_as_statement
          and when_true  : Ast.Statement.t = Expression (Value (Bool true))
          and when_false : Ast.Statement.t = rhs_statement
          in
          create_if_statement ~condition ~when_true ~when_false
        end
    in
    TC.return @@ wrap_in_named_statements_context lhs_named_statements if_statement

  and statement_of_cast
        (expression  : S.typ S.aexp)
        (target_type : S.typ       ) : Ast.Statement.t TC.t
    =
    let* translated_expression = statement_of_aexp (Some target_type) expression
    and* translated_type       = Type.translate_type target_type
    in
    TC.return @@ Ast.Statement.Cast (translated_expression, translated_type)

  and statement_of_throw
        (_aval : Libsail.Ast.typ Libsail.Anf.aval)
        (typ   : Libsail.Ast.typ                 ) : Ast.Statement.t TC.t
    =
    let* translated_type = Type.translate_type typ
    in
    TC.return @@ Ast.Statement.Fail (translated_type, "failure")

  and statement_of_exit
        (_aval : Libsail.Ast.typ Libsail.Anf.aval)
        (typ   : Libsail.Ast.typ                 ) : Ast.Statement.t TC.t
    =
    (* the type that Sail provides is untrustworthy, so we use our own type information *)
    let* () =
      let message = lazy begin
        PP.vertical [
          PP.string "Mismatch between expected type and given type";
          PP.description_list [
            (
              PP.string "Expected",
              PP.string begin
                Option.value ~default:"<unknown>" begin
                  Option.map ~f:StringOf.Sail.typ expression_type
                end
              end
            );
            (
              PP.string "Given",
              PP.string @@ StringOf.Sail.typ typ
            )
          ];
          PP.string "If Coq does not complain about the translation, it should be safe to ignore this warning";
        ]
      end
      in
      if not (expression_type = Some typ)
      then TC.log [%here] Logging.warning message
      else TC.return ()
    in
    match expression_type with
    | Some expression_type -> begin
        let* translated_type = Type.translate_type expression_type
        in
        TC.return @@ Ast.Statement.Fail (translated_type, "failure")
      end
    | None -> TC.fail [%here] "Unknown output type" (* todo keep optional type in Fail so that we can generate a _ and hope that Coq can infer it *)

  in
  (* Record the Sail source location of this expression so that, further down
     the pipeline, generation errors can point back to the original Sail code. *)
  let* translated_statement =
    match unwrapped_expression with
    | AE_val value                                                  -> statement_of_value value
    | AE_app (id, [ AV_ref (register_identifier, Register register_type); value ], _typ) -> statement_of_register_setter_call (Sail.id_of_function_id id) register_identifier register_type value
    | AE_app (id, avals, typ)                                       -> statement_of_application (Sail.id_of_function_id id) avals typ
    | AE_let (mutability, name, typ1, expression, body, typ2)       -> statement_of_let mutability (Sail.id_of_name name) typ1 expression body typ2
    | AE_if (condition, then_clause, else_clause, typ)              -> statement_of_if condition then_clause else_clause typ
    | AE_match (aval, cases, output_type)                           -> statement_of_match aval (List.map cases ~f:(fun (pattern, guard, body, _uannot) -> (pattern, guard, body))) output_type
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
  in
  TC.return @@ Ast.Statement.Located (location, translated_statement)


type sail_function_parts =
  {
    identifier         : S.id;
    parameter_bindings : Sail.type_annotation Libsail.Ast.pat;
    body               : Libsail.Ast.typ Libsail.Anf.aexp;
    return_type        : Libsail.Ast.typ;

    (* Identifiers bound in the parameters and body (see TC.refers_to_top_level_value) *)
    locally_bound_identifiers : Ast.Identifier.Set.t;
  }


(*
   The message that replaces assert messages that are computed at run time,
   see drop_run_time_assert_messages.
*)
let dropped_assert_message : string = "assertion failed (message computed at run time, dropped by the translation)"


let is_string_literal (expression : Sail.type_annotation S.exp) : bool =
  let S.E_aux (unwrapped_expression, _annotation) = expression
  in
  match unwrapped_expression with
  | E_lit (L_aux (literal, _location)) -> begin
      match literal with
      | L_string _ -> true
      | L_true     -> false
      | L_false    -> false
      | L_num _    -> false
      | L_unit     -> false
      | L_bin _    -> false
      | L_hex _    -> false
      | L_real _   -> false
    end
  | E_vector _                  -> false
  | E_block _                   -> false
  | E_id _                      -> false
  | E_typ (_, _)                -> false
  | E_app (_, _)                -> false
  | E_tuple _                   -> false
  | E_if (_, _, _)              -> false
  | E_loop (_, _, _, _)         -> false
  | E_for (_, _, _, _, _, _)    -> false
  | E_vector_append (_, _)      -> false
  | E_list _                    -> false
  | E_cons (_, _)               -> false
  | E_struct (_, _)             -> false
  | E_struct_update (_, _)      -> false
  | E_field (_, _)              -> false
  | E_match (_, _)              -> false
  | E_let (_, _, _)             -> false
  | E_assign (_, _)             -> false
  | E_sizeof _                  -> false
  | E_return _                  -> false
  | E_exit _                    -> false
  | E_config _                  -> false
  | E_ref _                     -> false
  | E_throw _                   -> false
  | E_try (_, _)                -> false
  | E_assert (_, _)             -> false
  | E_var (_, _, _)             -> false
  | E_undef                     -> false
  | E_internal_plet (_, _, _)   -> false
  | E_internal_return _         -> false
  | E_internal_value _          -> false
  | E_internal_assume (_, _)    -> false
  | E_constraint _              -> false


(*
   muSail needs the message of an assert to be a string literal, but in Sail it can be
   any string expression, e.g., internal_error in sail-riscv's riscv_types.sail does

     assert (false, file ^ ":" ^ dec_str(line) ^ ": " ^ s)

   Messages have no meaning for verification, so we replace each message that is not
   a string literal by the literal dropped_assert_message. This happens before the
   conversion to ANF, so that the computation of the message (here calls to the string
   functions dec_str and concat_str, which have no muSail counterpart) disappears too.

   Sail evaluates the message even when the condition holds, so dropping it is only
   exact if computing the message has no side effects. That is what one expects of a
   message, but Sail cannot always tell: it considers every mapping possibly failing,
   so report_invalid_width's message, which calls the mapping size_mnemonic, counts
   as effectful. Messages are therefore dropped regardless, and the locations of the
   ones Sail considers effectful are returned, so that the caller can warn about them.
   This relies on the effect annotations made by the attach_effects rewrite,
   which is the last one in sail_plugin/Rewrites.ml.
*)
let drop_run_time_assert_messages (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp * S.l list =
  let effectful_message_locations : S.l list ref = ref []
  in
  let replace_message (message : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
    let S.E_aux (_unwrapped_message, (location, annotation)) = message
    in
    if is_string_literal message
    then message
    else begin
      if Libsail.Ast_util.effectful (Libsail.Type_check.effect_of message)
      then effectful_message_locations := location :: !effectful_message_locations;
      S.E_aux (S.E_lit (S.L_aux (S.L_string dropped_assert_message, location)), (location, annotation))
    end
  in
  let algebra =
    {
      Libsail.Rewriter.id_exp_alg with
      e_assert = (fun (condition, message) -> S.E_assert (condition, replace_message message))
    }
  in
  let result = Libsail.Rewriter.fold_exp algebra expression
  in
  (result, List.rev !effectful_message_locations)


(*
   Helper function that extracts all relevant data from Sail's AST.
*)
(*
   Identifiers bound in an expression: in patterns (let, match, ...), as loop variables, and by var.
*)
let locally_bound_sail_identifiers (expression : Sail.type_annotation S.exp) : Libsail.Ast_compare.IdSet.t =
  let open Libsail.Rewriter
  in
  let module IdSet = Libsail.Ast_compare.IdSet
  in
  let pat_alg =
    {
      (pure_pat_alg IdSet.empty IdSet.union) with
      p_id = IdSet.singleton;
      p_as = (fun (ids, id) -> IdSet.add id ids)
    }
  in
  let exp_alg =
    {
      (pure_exp_alg IdSet.empty IdSet.union) with
      pat_alg;
      e_for  = (fun (id, from, until, step, _order, body) -> IdSet.add id (IdSet.union (IdSet.union from until) (IdSet.union step body)));
      le_id  = IdSet.singleton;
      le_typ = (fun (_typ, id) -> IdSet.singleton id);
    }
  in
  fold_exp exp_alg expression


(* The called function and the arguments, if the expression is a function call *)
let sail_application_of (expression : Sail.type_annotation S.exp) : (string * Sail.type_annotation S.exp list) option =
  let S.E_aux (unwrapped_expression, _) = expression
  in
  match unwrapped_expression with
  | E_app (identifier, arguments) -> Some (Libsail.Ast_util.string_of_id identifier, arguments)
  | E_lit _                  -> None
  | E_vector _               -> None
  | E_block _                -> None
  | E_id _                   -> None
  | E_typ (_, _)             -> None
  | E_tuple _                -> None
  | E_if (_, _, _)           -> None
  | E_loop (_, _, _, _)      -> None
  | E_for (_, _, _, _, _, _) -> None
  | E_vector_append (_, _)   -> None
  | E_list _                 -> None
  | E_cons (_, _)            -> None
  | E_struct (_, _)          -> None
  | E_struct_update (_, _)   -> None
  | E_field (_, _)           -> None
  | E_match (_, _)           -> None
  | E_let (_, _, _)          -> None
  | E_assign (_, _)          -> None
  | E_sizeof _               -> None
  | E_return _               -> None
  | E_exit _                 -> None
  | E_config _               -> None
  | E_ref _                  -> None
  | E_throw _                -> None
  | E_try (_, _)             -> None
  | E_assert (_, _)          -> None
  | E_var (_, _, _)          -> None
  | E_undef                  -> None
  | E_internal_plet (_, _, _)-> None
  | E_internal_return _      -> None
  | E_internal_value _       -> None
  | E_internal_assume (_, _) -> None
  | E_constraint _           -> None


(* The value of a numeric expression that simplifies to a constant *)
let constant_of_sail_numeric_expression (numeric_expression : S.nexp) : Z.t option =
  let S.Nexp_aux (unwrapped_numeric_expression, _) = Libsail.Ast_util.nexp_simp numeric_expression
  in
  match unwrapped_numeric_expression with
  | Nexp_constant constant -> Some constant
  | Nexp_id _              -> None
  | Nexp_var _             -> None
  | Nexp_app (_, _)        -> None
  | Nexp_if (_, _, _)      -> None
  | Nexp_times (_, _)      -> None
  | Nexp_sum (_, _)        -> None
  | Nexp_minus (_, _)      -> None
  | Nexp_exp _             -> None
  | Nexp_neg _             -> None


(* The width of a bitvector expression, if it is known at compile time *)
let sail_bitvector_width (expression : Sail.type_annotation S.exp) : Z.t option =
  let module TypeCheck = Libsail.Type_check
  in
  Option.bind
    (TypeCheck.destruct_bitvector (TypeCheck.env_of expression) (TypeCheck.typ_of expression))
    ~f:constant_of_sail_numeric_expression


(* The value of an integer expression, if its type says what it is at compile time *)
let sail_integer_value (expression : Sail.type_annotation S.exp) : Z.t option =
  let module TypeCheck = Libsail.Type_check
  in
  Option.bind
    (TypeCheck.destruct_atom_nexp (TypeCheck.env_of expression) (TypeCheck.typ_of expression))
    ~f:constant_of_sail_numeric_expression


(* A call to a function, without type annotations (to be type checked) *)
let untyped_sail_call (name : string) (arguments : Libsail.Ast_util.uannot S.exp list) : Libsail.Ast_util.uannot S.exp =
  Libsail.Ast_util.mk_exp (S.E_app (Libsail.Ast_util.mk_id name, arguments))


(*
   Truncation of a bitvector padded with zeros of run-time length.

     truncate(X @ zeros(n), w)

   has an intermediate bitvector of width |X| + n, which is only known at run time when n is,
   and muSail bitvector types need widths known in Rocq. CHERIoT's getCapBoundsBits does this:

     truncate((a_top + c_b) @ c.B @ zeros(E), cap_addr_width)     with E = unsigned(c.E)

   The low w bits of X * 2^n are those of resize(X, w) * 2^n, so this is rewritten to

     sail_shiftleft(resize(X, w), n)

   where resize is truncate if w <= |X| and sail_zero_extend otherwise.
   It requires |X| and w to be known; the concatenation may be nested either way.
   The rewritten expression is type checked again in the environment of the original; if that fails,
   the original is kept.
*)
let rewrite_truncated_zero_padding (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
  let module TypeCheck = Libsail.Type_check
  in
  (* The operands of a (nested) concatenation, most significant first *)
  let rec concatenated (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp list =
    match sail_application_of expression with
    | Some ("bitvector_concat", [ left; right ]) -> concatenated left @ concatenated right
    | Some _                                     -> [ expression ]
    | None                                       -> [ expression ]
  in
  let rewrite
      (unwrapped_expression : Sail.type_annotation S.exp_aux)
      (annotation           : S.l * Sail.type_annotation   ) : Sail.type_annotation S.exp
    =
    let original = S.E_aux (unwrapped_expression, annotation)
    in
    let rewritten : Sail.type_annotation S.exp option =
      let open Monads.OptionNotation
      in
      let=? concatenation, target_width_expression =
        match sail_application_of original with
        | Some ("truncate", [ concatenation; target_width_expression ]) -> Some (concatenation, target_width_expression)
        | Some _ -> None
        | None   -> None
      in
      let=? prefix, padding_length =
        match List.rev (concatenated concatenation) with
        | last :: (_ :: _ as reversed_prefix) -> begin
            match sail_application_of last with
            | Some (("sail_zeros" | "zeros_implicit"), [ padding_length ]) -> begin
                match sail_integer_value padding_length with
                | None   -> Some (List.rev reversed_prefix, padding_length)  (* only when the length is not known *)
                | Some _ -> None
              end
            | Some _ -> None
            | None   -> None
          end
        | [ _ ] -> None
        | []    -> None
      in
      let=? prefix_width =
        Option.map ~f:(List.fold ~init:Z.zero ~f:Z.add) @@ Option.all @@ List.map ~f:sail_bitvector_width prefix
      in
      let=? target_width = sail_integer_value target_width_expression
      in
      let untyped (expression : Sail.type_annotation S.exp) : Libsail.Ast_util.uannot S.exp =
        TypeCheck.strip_exp expression
      and call = untyped_sail_call
      in
      let=? untyped_prefix =
        let rec concatenate (elements : Libsail.Ast_util.uannot S.exp list) : Libsail.Ast_util.uannot S.exp option =
          match elements with
          | []                  -> None
          | [ element ]         -> Some element
          | element :: rest     -> Option.map (concatenate rest) ~f:(fun rest -> call "bitvector_concat" [ element; rest ])
        in
        concatenate (List.map ~f:untyped prefix)
      in
      let resized =
        let resize = if Z.leq target_width prefix_width then "truncate" else "sail_zero_extend"
        in
        call resize [ untyped_prefix; untyped target_width_expression ]
      in
      let replacement = call "sail_shiftleft" [ resized; untyped padding_length ]
      in
      try Some (TypeCheck.check_exp (TypeCheck.env_of_annot annotation) replacement (TypeCheck.typ_of_annot annotation))
      with _ -> None
    in
    Option.value rewritten ~default:original
  in
  Libsail.Rewriter.fold_exp { Libsail.Rewriter.id_exp_alg with e_aux = (fun (unwrapped_expression, annotation) -> rewrite unwrapped_expression annotation) } expression


(*
   A mask of the n low bits of a bitvector, with n known only at run time.

     zero_extend(m, ones(n))          (sail-riscv's zero_extend takes the width first)
     sail_zero_extend(ones(n), m)

   has the intermediate bitvector ones(n), whose width n is only known at run time,
   and muSail bitvector types need widths known in Rocq. CHERIoT's setCapBounds does this:

     maskLo : CapLenBits = zero_extend(ones(e_sat))     with e_sat : range(0, cap_max_E)

   For 0 <= n <= m, the n low bits are set and the others clear in not(ones(m) << n), so this is rewritten to

     not_vec(sail_shiftleft(sail_ones(m), n))

   (sail_ones(m) is written sail_ones(make_the_value(m)), as Sail does; n <= m is a constraint of the zero extension). It requires m to be known.
   ones(n) is ones or sail_ones; zero_extend is sail-riscv's zero_extend or sail_zero_extend.
   The rewritten expression is type checked again in the environment of the original; if that fails,
   the original is kept.
*)
let rewrite_zero_extended_ones (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
  let module TypeCheck = Libsail.Type_check
  in
  let rewrite
      (unwrapped_expression : Sail.type_annotation S.exp_aux)
      (annotation           : S.l * Sail.type_annotation   ) : Sail.type_annotation S.exp
    =
    let original = S.E_aux (unwrapped_expression, annotation)
    in
    let rewritten : Sail.type_annotation S.exp option =
      let open Monads.OptionNotation
      in
      let=? ones, target_width_expression =
        match sail_application_of original with
        | Some ("zero_extend", [ target_width_expression; ones ])      -> Some (ones, target_width_expression)
        | Some ("sail_zero_extend", [ ones; target_width_expression ]) -> Some (ones, target_width_expression)
        | Some _                                                      -> None
        | None                                                        -> None
      in
      (* sail_ones takes its length as make_the_value(n) : itself('n); the length is then n *)
      let rec integer_of_itself (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
        let S.E_aux (unwrapped_expression, _) = expression
        in
        match unwrapped_expression with
        | E_typ (_, inner_expression)   -> integer_of_itself inner_expression
        | E_app (identifier, arguments) -> begin
            match Libsail.Ast_util.string_of_id identifier, arguments with
            | "make_the_value", [ integer ] -> integer
            | _, _                          -> expression
          end
        | E_lit _                       -> expression
        | E_vector _                    -> expression
        | E_block _                     -> expression
        | E_id _                        -> expression
        | E_tuple _                     -> expression
        | E_if (_, _, _)                -> expression
        | E_loop (_, _, _, _)           -> expression
        | E_for (_, _, _, _, _, _)      -> expression
        | E_vector_append (_, _)        -> expression
        | E_list _                      -> expression
        | E_cons (_, _)                 -> expression
        | E_struct (_, _)               -> expression
        | E_struct_update (_, _)        -> expression
        | E_field (_, _)                -> expression
        | E_match (_, _)                -> expression
        | E_let (_, _, _)               -> expression
        | E_assign (_, _)               -> expression
        | E_sizeof _                    -> expression
        | E_return _                    -> expression
        | E_exit _                      -> expression
        | E_config _                    -> expression
        | E_ref _                       -> expression
        | E_throw _                     -> expression
        | E_try (_, _)                  -> expression
        | E_assert (_, _)               -> expression
        | E_var (_, _, _)               -> expression
        | E_undef                       -> expression
        | E_internal_plet (_, _, _)     -> expression
        | E_internal_return _           -> expression
        | E_internal_value _            -> expression
        | E_internal_assume (_, _)      -> expression
        | E_constraint _                -> expression
      in
      let=? mask_length =
        match sail_application_of ones with
        | Some (("ones" | "sail_ones"), [ mask_length ]) -> begin
            let mask_length = integer_of_itself mask_length
            in
            match sail_integer_value mask_length with
            | None   -> Some mask_length  (* only when the length is not known *)
            | Some _ -> None
          end
        | Some _ -> None
        | None   -> None
      in
      let=? _target_width = sail_integer_value target_width_expression
      in
      let untyped (expression : Sail.type_annotation S.exp) : Libsail.Ast_util.uannot S.exp =
        TypeCheck.strip_exp expression
      and call = untyped_sail_call
      in
      let replacement =
        call "not_vec" [
          call "sail_shiftleft" [
            call "sail_ones" [ call "make_the_value" [ untyped target_width_expression ] ];
            untyped mask_length
          ]
        ]
      in
      try Some (TypeCheck.check_exp (TypeCheck.env_of_annot annotation) replacement (TypeCheck.typ_of_annot annotation))
      with _ -> None
    in
    Option.value rewritten ~default:original
  in
  Libsail.Rewriter.fold_exp { Libsail.Rewriter.id_exp_alg with e_aux = (fun (unwrapped_expression, annotation) -> rewrite unwrapped_expression annotation) } expression


(*
   The low bits of a bitvector, zero-extended, with the number of bits known only at run time.

     zero_extend(m, v[hi .. 0])          (sail-riscv's zero_extend takes the width first)
     sail_zero_extend(v[hi .. 0], m)

   has the intermediate bitvector v[hi .. 0], whose width hi + 1 is only known at run time,
   and muSail bitvector types need widths known in Rocq. sail-riscv's writeCSR does this:

     vstart = zero_extend(16, value[vstart_length - 1 .. 0])     with value : xlenbits

   The hi + 1 low bits of v are kept and the others cleared by v & not(ones(w) << (hi + 1)), w = |v|, so this is rewritten to

     resize(and_vec(v, not_vec(sail_shiftleft(sail_ones(w), hi + 1))), m)

   where resize is truncate if m <= w and sail_zero_extend otherwise (hi + 1 <= m is a constraint
   of the zero extension, so truncating keeps every kept bit). It requires w and m to be known.
   v[hi .. 0] is subrange_bits(v, hi, 0); zero_extend is sail-riscv's zero_extend or sail_zero_extend.
   The rewritten expression is type checked again in the environment of the original; if that fails,
   the original is kept.
*)
let rewrite_zero_extended_low_slice (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
  let module TypeCheck = Libsail.Type_check
  in
  let rewrite
      (unwrapped_expression : Sail.type_annotation S.exp_aux)
      (annotation           : S.l * Sail.type_annotation   ) : Sail.type_annotation S.exp
    =
    let original = S.E_aux (unwrapped_expression, annotation)
    in
    let rewritten : Sail.type_annotation S.exp option =
      let open Monads.OptionNotation
      in
      let=? slice, target_width_expression =
        match sail_application_of original with
        | Some ("zero_extend", [ target_width_expression; slice ])      -> Some (slice, target_width_expression)
        | Some ("sail_zero_extend", [ slice; target_width_expression ]) -> Some (slice, target_width_expression)
        | Some _                                                       -> None
        | None                                                         -> None
      in
      let=? bitvector, high_index =
        match sail_application_of slice with
        | Some ("subrange_bits", [ bitvector; high_index; low_index ]) -> begin
            match sail_integer_value high_index, sail_integer_value low_index with
            | None  , Some low when Z.equal low Z.zero -> Some (bitvector, high_index)  (* only when the high index is not known *)
            | None  , Some _                           -> None
            | None  , None                             -> None
            | Some _, _                                -> None
          end
        | Some _ -> None
        | None   -> None
      in
      let=? bitvector_width = sail_bitvector_width bitvector
      in
      let=? target_width = sail_integer_value target_width_expression
      in
      let untyped (expression : Sail.type_annotation S.exp) : Libsail.Ast_util.uannot S.exp =
        TypeCheck.strip_exp expression
      and call = untyped_sail_call
      and integer (value : Z.t) : Libsail.Ast_util.uannot S.exp =
        Libsail.Ast_util.mk_lit_exp (S.L_num value)
      in
      let masked =
        call "and_vec" [
          untyped bitvector;
          call "not_vec" [
            call "sail_shiftleft" [
              call "sail_ones" [ call "make_the_value" [ integer bitvector_width ] ];
              call "add_atom" [ untyped high_index; integer Z.one ]
            ]
          ]
        ]
      in
      let replacement =
        let resize = if Z.leq target_width bitvector_width then "truncate" else "sail_zero_extend"
        in
        call resize [ masked; untyped target_width_expression ]
      in
      try Some (TypeCheck.check_exp (TypeCheck.env_of_annot annotation) replacement (TypeCheck.typ_of_annot annotation))
      with _ -> None
    in
    Option.value rewritten ~default:original
  in
  Libsail.Rewriter.fold_exp { Libsail.Rewriter.id_exp_alg with e_aux = (fun (unwrapped_expression, annotation) -> rewrite unwrapped_expression annotation) } expression


(*
   An if whose condition is known at compile time becomes its live branch.

     if gteq_int(32, 64) then A else B          (sail-riscv: if sizeof(xlen) >= 64 then ... in execute_STORE)

   becomes B. Sail's flow typing checks A assuming the condition, so A may be ill-typed once the
   condition is false: in execute_STORE it calls mem_write_value at 8 bytes with a bits(32), and Rocq
   searched for the false width hypothesis for over 45 s. The condition is a literal, an integer
   comparison whose operands' values the types give, or not, and and or of those; value exclusions
   (exclude-functions-as-value: haveZbb() = false) are replaced before, so their conditions fold too.
*)
let rewrite_constant_conditions (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
  let rec value_of (condition : Sail.type_annotation S.exp) : bool option =
    let compare (operator : Z.t -> Z.t -> bool) (left : Sail.type_annotation S.exp) (right : Sail.type_annotation S.exp) : bool option =
      match sail_integer_value left, sail_integer_value right with
      | Some left, Some right -> Some (operator left right)
      | Some _   , None       -> None
      | None     , Some _     -> None
      | None     , None       -> None
    in
    match sail_application_of condition with
    | Some ("eq_int"  , [ left; right ]) -> compare Z.equal left right
    | Some ("neq_int" , [ left; right ]) -> compare (fun x y -> not (Z.equal x y)) left right
    | Some ("lt_int"  , [ left; right ]) -> compare Z.lt left right
    | Some ("lteq_int", [ left; right ]) -> compare Z.leq left right
    | Some ("gt_int"  , [ left; right ]) -> compare Z.gt left right
    | Some ("gteq_int", [ left; right ]) -> compare Z.geq left right
    | Some ("not_bool", [ operand ])     -> Option.map ~f:not (value_of operand)
    | Some ("and_bool", [ left; right ]) -> begin
        match value_of left, value_of right with
        | Some false, _          -> Some false
        | _         , Some false -> Some false
        | Some true , Some true  -> Some true
        | Some true , None       -> None
        | None      , Some true  -> None
        | None      , None       -> None
      end
    | Some ("or_bool" , [ left; right ]) -> begin
        match value_of left, value_of right with
        | Some true , _          -> Some true
        | _         , Some true  -> Some true
        | Some false, Some false -> Some false
        | Some false, None       -> None
        | None      , Some false -> None
        | None      , None       -> None
      end
    | Some _ -> None
    | None   -> begin
        let S.E_aux (unwrapped_condition, _) = condition
        in
        match unwrapped_condition with
        | E_lit (S.L_aux (S.L_true , _)) -> Some true
        | E_lit (S.L_aux (S.L_false, _)) -> Some false
        | _                              -> None
      end
  in
  let rewrite
      (unwrapped_expression : Sail.type_annotation S.exp_aux)
      (annotation           : S.l * Sail.type_annotation   ) : Sail.type_annotation S.exp
    =
    match unwrapped_expression with
    | E_if (condition, when_true, when_false) -> begin
        match value_of condition with
        | Some true  -> when_true
        | Some false -> when_false
        | None       -> S.E_aux (unwrapped_expression, annotation)
      end
    | _ -> S.E_aux (unwrapped_expression, annotation)
  in
  Libsail.Rewriter.fold_exp { Libsail.Rewriter.id_exp_alg with e_aux = (fun (unwrapped_expression, annotation) -> rewrite unwrapped_expression annotation) } expression


(*
   Replaces each call to a function that the configuration excludes with a value
   (exclude-functions-as-value) with that value: a Sail expression, parsed and then
   type-checked at the type of the call, in the call's environment.

     f(x, read_reg())        with  (exclude-functions-as-value '(("f" "None()")))

   becomes

     let excluded_call_arguments = read_reg() in None()

   Arguments with an effect are still computed; the others are dropped.
   A replacement that does not parse or type-check leaves the call as it is;
   its translation then fails with an error (see statement_of_application).
*)
let replace_calls_by_values (expression : Sail.type_annotation S.exp) : Sail.type_annotation S.exp =
  let module TypeCheck = Libsail.Type_check
  in
  let rewrite
      (unwrapped_expression : Sail.type_annotation S.exp_aux     )
      (annotation           : Sail.type_annotation S.annot       ) : Sail.type_annotation S.exp
    =
    let original = S.E_aux (unwrapped_expression, annotation)
    in
    match unwrapped_expression with
    | S.E_app (identifier, arguments) -> begin
        match Configuration.function_exclusion (Libsail.Ast_util.string_of_id identifier) with
        | Some (Value value) -> begin
            try
              let value_expression =
                Libsail.Initial_check.exp_of_string Libsail.Initial_check.initial_ctx value
              in
              let replacement =
                let effectful_arguments =
                  List.filter arguments ~f:(fun argument -> Libsail.Ast_util.effectful (Libsail.Type_check.effect_of argument))
                in
                let untyped = TypeCheck.strip_exp
                (* not a wildcard: the translation does not support let _ = ... (Match.ml) *)
                and unused_binder = Libsail.Ast_util.mk_pat (S.P_id (Libsail.Ast_util.mk_id "excluded_call_arguments"))
                in
                match effectful_arguments with
                | []         -> value_expression
                | [ single ] -> Libsail.Ast_util.mk_exp (S.E_let (unused_binder, untyped single, value_expression))
                | several    -> begin
                    let tuple = Libsail.Ast_util.mk_exp (S.E_tuple (List.map ~f:untyped several))
                    in
                    Libsail.Ast_util.mk_exp (S.E_let (unused_binder, tuple, value_expression))
                  end
              in
              TypeCheck.check_exp (TypeCheck.env_of_annot annotation) replacement (TypeCheck.typ_of_annot annotation)
            with _ -> original
          end
        | Some (NoOp | Failure | Unconstrained) -> original
        | None                                  -> original
      end
    | _ -> original
  in
  Libsail.Rewriter.fold_exp { Libsail.Rewriter.id_exp_alg with e_aux = (fun (unwrapped_expression, annotation) -> rewrite unwrapped_expression annotation) } expression


let extract_function_parts (function_clause : Sail.type_annotation Libsail.Ast.funcl) : sail_function_parts TC.t =
  let S.FCL_aux (S.FCL_funcl (identifier, clause), (_def_annot, _type_annotation)) = function_clause
  in
  let S.Pat_aux (unwrapped_clause, (location, _annotation)) = clause
  in
  match unwrapped_clause with
   | Pat_when (_, _, _) -> TC.not_yet_implemented [%here] location
   | Pat_exp (parameter_bindings, raw_body) -> begin
       let body_without_messages, effectful_message_locations = drop_run_time_assert_messages raw_body
       in
       let* () =
         let warn (message_location : S.l) : unit TC.t =
           let warning_message = lazy begin
             PP.string begin
               Printf.sprintf
                 "Dropped the message of an assert although Sail considers its computation effectful, at %s"
                 (Sail.string_of_location message_location)
             end
           end
           in
           TC.log [%here] Logging.warning warning_message
         in
         TC.iter ~f:warn effectful_message_locations
       in
       let body        = S.anf @@ rewrite_zero_extended_low_slice @@ rewrite_zero_extended_ones @@ rewrite_truncated_zero_padding @@ rewrite_constant_conditions @@ replace_calls_by_values body_without_messages
       and return_type = Libsail.Type_check.typ_of raw_body
       in
       let* locally_bound_identifiers =
         let sail_identifiers =
           Libsail.Ast_compare.IdSet.union
             (Libsail.Ast_util.pat_ids parameter_bindings)
             (locally_bound_sail_identifiers raw_body)
         in
         TC.map ~f:(Identifier.translate_identifier [%here]) (Libsail.Ast_compare.IdSet.elements sail_identifiers)
       in
       TC.return @@ {
         identifier;
         parameter_bindings;
         body;
         return_type;
         locally_bound_identifiers = Ast.Identifier.Set.of_list locally_bound_identifiers;
       }
     end


(*
   When bitvectors are compared using <, >, <= and >=, it is the integer value they represent that is actually being compared.
   However, bitvectors can use signed or unsigned representation.
   In other words, x=0b1111 is greater than y=0b0000 when interpreting them as unsigned integers,
   but x < y if signed representation is used.

   In Sail, the way to specify which way bitvectors should be compared is by using the signed/unsigned helper functions:

     signed(bv1) < signed(bv2)
     unsigned(bv1) >= unsigned(bv2)

   These functions convert bitvectors into integers, which are them compared using the standard integer-comparing functions
   gt_int, lt_int, lteq_int or gteq_int.

   muSail allows to compare bitvector directly using

      <ˢ    <ᵘ
      <=ˢ   <=ᵘ
      >ˢ    >ᵘ
      >=ˢ   >=ᵘ

   Our goal is to translate

      signed(bv1) < signed(bv2)

   into

      bv1 <ˢ bv2


   There is a slight complication, though: Sail rewrites its ASTs into A-normal form, so instead of

      signed(bv1) < signed(bv2)

   we receive

      let g1 = signed(bv1)
      and g2 = signed(bv2)
      in
      g1 < g2

   (For some reason, this transformation isn't visible in the rewrite dumps or string_of; we didn't investigate this peculiarity)

   We could have chosen to detect this pattern *during* the translation into nanosail statements,
   but we felt it was simpler to do it after the nanosail AST has been constructed.
   See the function translate_body.


   This function looks for the following pattern in the AST:

     let binder = f(expr1)
     in
     let binder2 = g(expr2)
     in
     comparison(left_id, right_id)

   where

     (binder, binder2 == left_id, right_id) or (binder, binder2 == right_id, left_id).

   If found, this AST is rewritten as

     comparison(expr1, expr2)

   or

     comparison(expr2, expr1)


   We expect the signedness of both operands to be the same.
   If signed is compared with unsigned (or vice versa), no transformation occurs.
   The resulting code should still be valid in this case,
but the specialized bitvector comparison operators will not be used.
*)
let rewrite_bitvector_comparisons (statement : Ast.Statement.t) : Ast.Statement.t =
  let rec rewrite (statement : Ast.Statement.t) : Ast.Statement.t =
    match (statement : Ast.Statement.t) with
    | Let { binder; binding_statement_type; binding_statement; body_statement } -> begin
        let result : Ast.Statement.t option =
          (* we found a let-construct in the AST; check if it is a bitvector comparison in A-normalized form.
             Statements may be wrapped in Located nodes (carrying Sail source locations);
             we strip those before matching on structure. *)
          match Ast.Statement.strip_location binding_statement, Ast.Statement.strip_location body_statement with
          | Call (f, f_arguments),
            Let {
              binder                 = binder2;
              binding_statement      = inner_binding_statement;
              binding_statement_type = _;
              body_statement         = inner_body_statement;
            } -> begin
              match Ast.Statement.strip_location inner_binding_statement, Ast.Statement.strip_location inner_body_statement with
              | Call (g, g_arguments),
                Expression (BinaryOperation (binary_operator, Variable (left_id, _), Variable (right_id, _))) -> begin
              (* multiple conditions need to be satisfied; relying on option monad to improve readability *)
              let open Monads.OptionNotation
              in
              (*
                 Determine the signedness of both operands.
                 We only supported signed/signed and unsigned/unsigned comparison.
                 We fall back on the default translation if we're dealing with mixed signedness.
              *)
              let=? signedness =
                match Ast.Identifier.to_string f, Ast.Identifier.to_string g with
                | "signed"  , "signed"   -> Some Ast.BinaryOperator.Signedness.Signed
                | "unsigned", "unsigned" -> Some Ast.BinaryOperator.Signedness.Unsigned
                | _                      -> None

              (*
                 We determined that f and g are signed/unsigned converters.
                 Now we check that they receive exactly one argument each.
                 If this is not the case (for some unfathomable reason),
                 we will fall back on the default translation.
              *)
              and=? f_argument =
                match f_arguments with
                | [x] -> Some x
                | _   -> None

              and=? g_argument =
                match g_arguments with
                | [x] -> Some x
                | _   -> None
              in

              (*
                 We check that the binders match the arguments.
                 If not, fall back to default implementation.
                 We do not assume that the binders appear in the same order as the arguments,
                 so

                   let x = signed(bv1) in let y = signed(bv2) = x < y
                   let y = signed(bv1) in let x = signed(bv2) = x < y

                 are both accepted.
              *)
              let=? left_operand, right_operand =
                if
                  Ast.Identifier.equal binder left_id && Ast.Identifier.equal binder2 right_id
                then
                  Some (f_argument, g_argument)
                else if
                  Ast.Identifier.equal binder right_id && Ast.Identifier.equal binder right_id
                then
                  Some (g_argument, f_argument)
                else
                  None
              in
              (*
                 We check that we're indeed dealing with a comparison (<. >, <= or >=),
                 otherwise default translation.
              *)
              let=? comparison =
                match binary_operator with
                | StandardComparison comparison -> Some comparison
                | _                             -> None
              in
              (*
                 All conditions have been satisfied.
                 We can rewrite the AST.
              *)
              Some begin
                Ast.Statement.Expression begin
                  Ast.Expression.BinaryOperation (Ast.BinaryOperator.BitvectorComparison (signedness, comparison), left_operand, right_operand)
                end
              end
                end
              | _ -> None
            end
          | _ -> None
        in
        match result with
        | Some transformation -> transformation
        | None -> begin
            (*
               The let-construct did not follow pattern
               We recursively rewrite but leave this let unchanged otherwise
            *)
            Let {
              binder;
              binding_statement_type;
              binding_statement = rewrite binding_statement;
              body_statement    = rewrite body_statement;
            }
          end
      end
    | Match (MatchList { matched; element_type; when_cons = (head_id, tail_id, when_cons); when_nil }) -> begin
        Match begin
          MatchList {
            matched;
            element_type;
            when_cons = (head_id, tail_id, rewrite when_cons);
            when_nil  = rewrite when_nil;
          }
        end
      end
    | Match (MatchProduct { matched; type_fst; type_snd; id_fst; id_snd; body }) -> begin
        Match begin
          MatchProduct {
            matched;
            type_fst;
            type_snd;
            id_fst;
            id_snd;
            body = rewrite body;
          }
        end
      end
    | Match (MatchTuple { matched; binders; body }) -> begin
        Match begin
          MatchTuple {
            matched;
            binders;
            body = rewrite body;
          }
        end
      end
    | Match (MatchBool { condition; when_true; when_false }) -> begin
        Match begin
          MatchBool {
            condition;
            when_true  = rewrite when_true;
            when_false = rewrite when_false;
          }
        end
      end
    | Match (MatchEnum { matched; matched_type; cases }) -> begin
        Match begin
          MatchEnum {
            matched;
            matched_type;
            cases = Ast.Identifier.Map.map_values ~f:rewrite cases
          }
        end
      end
    | Match (MatchVariant { matched; matched_type; cases }) -> begin
        Match begin
          MatchVariant {
            matched;
            matched_type;
            cases = Ast.Identifier.Map.map_values ~f:(fun (ids, b) -> (ids, rewrite b)) cases;
          }
        end
      end
    | Match (MatchSum { matched; left_type; right_type; when_left; when_right }) -> begin
        let rewrite_case ((binder, clause) : Ast.Identifier.t * Ast.Statement.t) : Ast.Identifier.t * Ast.Statement.t =
          (binder, rewrite clause)
        in
        Match begin
          MatchSum {
            matched;
            left_type;
            right_type;
            when_left  = rewrite_case when_left;
            when_right = rewrite_case when_right;
          }
        end
      end
    | DestructureRecord { record_type_identifier; field_identifiers; binders; destructured_record; body } -> begin
        DestructureRecord {
          record_type_identifier;
          field_identifiers;
          binders;
          destructured_record = rewrite destructured_record;
          body                = rewrite body;
        }
      end
    | Expression _          -> statement
    | Call (_, _)           -> statement
    | Seq (left, right)     -> Seq (rewrite left, rewrite right)
    | ReadRegister _        -> statement
    | WriteRegister _       -> statement
    | AssignVariable { variable_identifier; assigned_value } -> AssignVariable { variable_identifier; assigned_value = rewrite assigned_value }
    | Cast (statement, typ) -> Cast (rewrite statement, typ)
    | Fail (_, _)           -> statement
    | Unconstrained _       -> statement
    | Specialize { equalities; facts; rebinds; result_type; body } -> Specialize { equalities; facts; rebinds; result_type; body = rewrite body }
    | Located (location, inner_statement) -> Located (location, rewrite inner_statement)
  in
  rewrite statement


(*
   Translates a function's body.
*)
let translate_body
    (body      : S.typ S.aexp)
    (body_type : S.typ       ) : Ast.Statement.t TC.t
  =
  (* Step 1: translate from Sail to nanosail *)
  let* translation = statement_of_aexp (Some body_type) body
  in
  (* Step 2: find bitvector comparisons and rewrite them *)
  let translation = rewrite_bitvector_comparisons translation
  in
  TC.return translation


(*
   Sail's merge_function_clauses rewrite turns the clauses of a scattered function into one clause
   whose parameter is a single variable, merge#var, bound to the tuple of the arguments:

     val scatfoo : (MyEnum, int) -> int
     function scatfoo merge#var = match merge#var { (XXX, x) => x, (YYY, x) => x + 1 }

   Its callers pass the arguments separately (scatfoo(XXX, 1)), so the function gets one parameter
   per element, and its body rebuilds the tuple first:

     function scatfoo (merge#var#1, merge#var#2) = let merge#var = (merge#var#1, merge#var#2) in ...

   Only when the val spec declares that many parameters: a function of one tuple keeps it.
*)
let split_tuple_parameter
    (declared_parameter_count : int                             )
    (parameters               : (Ast.Identifier.t * Ast.Type.t) list)
    (body                     : Ast.Statement.t                 ) : (Ast.Identifier.t * Ast.Type.t) list * Ast.Statement.t
  =
  match parameters with
  | [ (identifier, Tuple element_types) ] when declared_parameter_count > 1 && List.length element_types = declared_parameter_count -> begin
      let element_parameters =
        List.mapi element_types ~f:(fun index element_type ->
            (Ast.Identifier.mk @@ Printf.sprintf "%s#%d" (Ast.Identifier.to_string identifier) (index + 1), element_type)
          )
      in
      let tuple =
        (* a pair is muSail's ty.prod, built with bop.pair; longer tuples are ty.tuple *)
        match List.map element_parameters ~f:(fun (element, element_type) -> Ast.Expression.Variable (element, element_type)) with
        | [ first; second ] -> Ast.Expression.BinaryOperation (Pair, first, second)
        | elements          -> Ast.Expression.Tuple elements
      in
      let body =
        Ast.Statement.Let {
          binder                 = identifier;
          binding_statement_type = Tuple element_types;
          binding_statement      = Expression tuple;
          body_statement         = body;
        }
      in
      (element_parameters, body)
    end
  | _ -> (parameters, body)


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
        let* function_name = Identifier.translate_identifier [%here] parts.identifier
        and* parameters    = translate_parameter_bindings parts.parameter_bindings
        and* return_type   = translate_return_type parts.return_type
        and* function_body = TC.with_locally_bound_identifiers parts.locally_bound_identifiers (translate_body parts.body parts.return_type)
        and* type_constraints =
          (* the quantifier of the function's val spec, as Sail's type checker saw it *)
          let environment = Libsail.Type_check.env_of_pat parts.parameter_bindings
          in
          match Libsail.Type_check.Env.get_val_spec_orig parts.identifier environment with
          | (type_quantifier, _)           -> TypeQuantifier.translate_type_quantifier_constraints type_quantifier
          | exception Libsail.Type_error.Type_error _ -> TC.return []
        in
        let* () =
          let S.Typ_annot_opt_aux (unwrapped_tannot_opt, _tannot_location) = _tannot_opt
          in
          let* tannot_opt_message =
            match unwrapped_tannot_opt with
            | Typ_annot_opt_none -> TC.return @@ PP.string "None"
            | Typ_annot_opt_some (type_quantifier, typ) -> begin
                let* type_quantifier' = TypeQuantifier.translate_type_quantifier type_quantifier
                and* typ'             = Type.translate_type typ
                in
                TC.return begin
                  PP.description_list [
                    (
                      PP.string "Type quantifier",
                      FExpr.pp @@ Ast.TypeQuantifier.to_fexpr type_quantifier'
                    );
                    (
                      PP.string "Type",
                      FExpr.pp @@ Ast.Type.to_fexpr typ'
                    )
                  ]
                end
              end
          in
          let message = lazy begin
            let properties =
              PP.description_list [
                (
                  PP.string "Function name",
                  PP.string @@ Ast.Identifier.to_string function_name
                );
                (
                  PP.string "Original",
                  PP.string @@ StringOf.Sail.definition full_sail_definition
                );
                (
                  PP.string "Parameter bindings",
                  PP.string @@ StringOf.Sail.pat parts.parameter_bindings
                );
                (
                  PP.string "type_annotation",
                  PP.string @@ StringOf.Sail.type_annotation type_annotation
                );
                (
                  PP.string "tannot_opt",
                  tannot_opt_message
                );
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
        let* is_function_polymorphic : bool =
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
        let parameters, function_body =
          let declared_parameter_count =
            let environment = Libsail.Type_check.env_of_pat parts.parameter_bindings
            in
            match Libsail.Type_check.Env.get_val_spec_orig parts.identifier environment with
            | (_, S.Typ_aux (Typ_fn (parameter_types, _), _)) -> List.length parameter_types
            | (_, _)                                         -> 1
            | exception Libsail.Type_error.Type_error _      -> 1
          in
          split_tuple_parameter declared_parameter_count parameters function_body
        in
        let function_body =
          Ast.Statement.simplify function_body
        in
        let* monomorphs : Ast.Definition.Function.t list =
          match is_function_polymorphic, Configuration.requested_monomorphizations_for function_name with
          | true, None -> begin
              (* we encountered a polymorphic function, but no monomorphizations were requested *)
              let* () =
                let message = lazy begin
                  PP.vertical [
                    PP.format "Encountered polymorphic function %s" (Ast.Identifier.to_string function_name);
                    PP.format "No monomorphizations were requested";
                  ]
                end
                in
                TC.log [%here] Logging.warning message
              in
              TC.return []
            end
          | false, Some _ -> begin
              (* we encountered a monomorphic function, yet monomorphizations were requested; gently inform user of their foolishness *)
              let* () =
                let message = lazy begin
                  PP.format "Monomorphization was requested for the monomorphic function %s" (Ast.Identifier.to_string function_name)
                end
                in
                TC.log [%here] Logging.error message
              in
              TC.return []
            end
          | false, None -> begin
              (* monomorphic function, no monomorphization requests *)
              TC.return []
            end
          | true, Some monomorphization_requests -> begin
              (* polymorphic function with monomorphization requests *)
              let generate_monomorph (monomorphization_request : Configuration.monomorphization_request) : Ast.Definition.Function.t TC.t =
                let substitution (identifier : Ast.Identifier.t) : Ast.Numeric.Expression.t =
                  match List.Assoc.find monomorphization_request.substitutions ~equal:Ast.Identifier.equal identifier with
                  | Some value -> Ast.Numeric.Expression.Constant (Z.of_int value)
                  | None       -> Ast.Numeric.Expression.Id identifier
                in
                let substituted_parameters =
                  let substitute_parameter
                      (parameter_identifier : Ast.Identifier.t)
                      (parameter_type       : Ast.Type.t      ) : Ast.Identifier.t * Ast.Type.t
                    =
                    (
                      parameter_identifier,
                      Ast.Type.simplify @@ Ast.Type.substitute_numeric_expression_identifier substitution parameter_type
                    )
                  in
                  List.map ~f:(Fn.uncurry substitute_parameter) parameters
                in
                let substituted_return_type =
                  Ast.Type.simplify @@ Ast.Type.substitute_numeric_expression_identifier substitution return_type
                in
                let substituted_function_body =
                  Ast.Statement.simplify @@ Ast.Statement.substitute_numeric_expression_identifier substitution function_body
                in
                let monomorph : Ast.Definition.Function.t =
                  {
                    function_name = monomorphization_request.monomorphization_identifier;
                    function_type = {
                      parameters  = substituted_parameters;
                      return_type = substituted_return_type;
                    };
                    function_body = substituted_function_body;
                    polymorphic   = false;
                    monomorphs    = [];
                    type_constraints = [];
                  }
                in
                let* () =
                  let message = lazy begin
                    PP.vertical [
                      PP.format "Generated monomorph";
                      PP.description_list [
                        (
                          PP.string "Polymorphic function name",
                          PP.string @@ Ast.Identifier.to_string function_name
                        );
                        (
                          PP.string "Applied substitutions",
                          PP.vertical begin
                            List.map
                              monomorphization_request.substitutions
                              ~f:(fun (identifier, value) -> begin
                                    PP.horizontal [
                                      PP.string @@ Ast.Identifier.to_string identifier;
                                      PP.string " -> ";
                                      PP.integer value
                                    ]
                                  end)
                          end
                        );
                        (
                          PP.string "Monomorphization",
                          FExpr.pp @@ Ast.Definition.Function.to_fexpr monomorph;
                        )
                      ]
                    ]
                  end
                  in
                  TC.log [%here] Logging.info message
                in
                TC.return monomorph
              in
              TC.map
                monomorphization_requests
                ~f:generate_monomorph
            end
        in
        TC.return @@ Ast.Definition.FunctionDefinition {
          function_name;
          function_type = {
            parameters;
            return_type;
          };
          function_body;
          polymorphic = is_function_polymorphic;
          monomorphs;
          type_constraints;
        }
      end
    | _ -> TC.not_yet_implemented [%here] definition_annotation.loc
  end
