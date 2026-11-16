open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let pp_expression (expression : Ast.Expression.t) : PP.t GC.t =
  let* pp_expression = Expressions.pp_expression expression
  in
  GC.return @@ PP.annotate [%here] @@ MuSail.Statement.pp_expression pp_expression


let parenthesize =
  GC.lift ~f:(PP.(surround parens))


(*
   Attempts to determine, at generation time, the integer value a statement
   evaluates to.

   Sail's rewrites wrap the interesting expression in layers of Located, Cast and
   calls to make_the_value, and may introduce intermediate let bindings of their
   own; this function looks through all of them.
   Anything else yields None, which simply means "not known at compile time".
*)
let rec extract_compile_time_integer_from_statement (statement : Ast.Statement.t) : Z.t option GC.t =
  match statement with
  | Located (_, inner)    -> extract_compile_time_integer_from_statement inner
  | Cast (inner, _typ)    -> extract_compile_time_integer_from_statement inner
  | Expression expression -> FunctionCalls.extract_compile_time_integer expression
  (*
     make_the_value is introduced by Sail's atoms_to_singletons rewrite, size_itself_int is its inverse;
     as far as the value is concerned, both are the identity.
  *)
  | Call (function_identifier, [ argument ])
    when Ast.Identifier.equal function_identifier (Ast.Identifier.mk "make_the_value")
      || Ast.Identifier.equal function_identifier (Ast.Identifier.mk "size_itself_int") -> begin
      FunctionCalls.extract_compile_time_integer argument
    end
  (*
     A nested let: evaluate the bound value in the current scope, bind the binder,
     and evaluate the body in the extended scope.
  *)
  | Let { binder; binding_statement; body_statement; _ } -> begin
      let* value = extract_compile_time_integer_from_statement binding_statement
      in
      GC.with_compile_time_integer binder value begin
        extract_compile_time_integer_from_statement body_statement
      end
    end
  | _ -> GC.return None


(*
   The statement a match matches on: the matched variable, or the statement that computes it
   when pp_let leaves out a let that only binds the scrutinee (see inlined_scrutinee_match).
*)
let pp_scrutinee
    (matched   : Ast.Identifier.t)
    (scrutinee : PP.t option     ) : PP.t
  =
  match scrutinee with
  | Some scrutinee -> PP.(surround parens) scrutinee
  | None           -> PP.(surround parens) @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_variable @@ Identifier.pp matched


(*
   Whether the cases of a match use the variable (other than as the matched value).
*)
let match_cases_use (variable : Ast.Identifier.t) (match_pattern : Ast.Statement.match_pattern) : bool =
  let uses (statement : Ast.Statement.t) (binders : Ast.Identifier.t list) : bool =
    Ast.Identifier.Set.mem (Ast.Statement.free_variables statement) variable
    && not (List.mem binders variable ~equal:Ast.Identifier.equal)
  in
  match match_pattern with
  | MatchList { when_cons = (id_head, id_tail, when_cons); when_nil; _ } -> uses when_nil [] || uses when_cons [ id_head; id_tail ]
  | MatchProduct { id_fst; id_snd; body; _ }                             -> uses body [ id_fst; id_snd ]
  | MatchTuple { binders; body; _ }                                      -> uses body (List.map ~f:fst binders)
  | MatchBool { when_true; when_false; _ }                               -> uses when_true [] || uses when_false []
  | MatchEnum { cases; _ }                                               -> List.exists (Ast.Identifier.Map.data cases) ~f:(fun clause -> uses clause [])
  | MatchVariant { cases; _ }                                            -> List.exists (Ast.Identifier.Map.data cases) ~f:(fun (binders, clause) -> uses clause binders)
  | MatchSum { when_left = (id_left, when_left); when_right = (id_right, when_right); _ } -> uses when_left [ id_left ] || uses when_right [ id_right ]


(*
   The variable a match matches on.
*)
let matched_variable (match_pattern : Ast.Statement.match_pattern) : Ast.Identifier.t =
  match match_pattern with
  | MatchList { matched; _ }      -> matched
  | MatchProduct { matched; _ }   -> matched
  | MatchTuple { matched; _ }     -> matched
  | MatchBool { condition; _ }    -> condition
  | MatchEnum { matched; _ }      -> matched
  | MatchVariant { matched; _ }   -> matched
  | MatchSum { matched; _ }       -> matched


(*
   In

     let x = S in match x { ... }        (Sail's ANF names every matched value)

   the let can be left out when the cases do not use x: muSail matches on a statement, so this is

     match S { ... }

   which has the same meaning. It matters for Rocq: each let extends the context of everything after it,
   and the decoder (encdec_backwards) is a chain of about 130 such matches, one per encoding, nested in each
   other's last case; with the lets, coqc took 608 s on it.
   A match on unit is left alone: it is printed as its only case, without the matched value.
*)
let rec inlined_scrutinee_match
    (binder         : Ast.Identifier.t)
    (body_statement : Ast.Statement.t ) : Ast.Statement.match_pattern option
  =
  match body_statement with
  | Located (_, inner) -> inlined_scrutinee_match binder inner
  | Match match_pattern -> begin
      let is_unit_match =
        match match_pattern with
        | MatchEnum { matched_type; _ } -> Ast.Identifier.equal matched_type (Ast.Identifier.mk "unit")
        | MatchList _ | MatchProduct _ | MatchTuple _ | MatchBool _ | MatchVariant _ | MatchSum _ -> false
      in
      if
        Ast.Identifier.equal (matched_variable match_pattern) binder
        && not (match_cases_use binder match_pattern)
        && not is_unit_match
      then Some match_pattern
      else None
    end
  | Expression _ | Call (_, _) | Let _ | Seq (_, _) | ReadRegister _ | WriteRegister _ | AssignVariable _
  | DestructureRecord _ | Cast (_, _) | Fail (_, _) | Unconstrained _ | Specialize _ -> None


(*

   Converts the following Sail code

     match <matched> {
       [||]               => when_nil,
       id_head :: id_tail => when_cons_body
     }

*)
let rec pp_match_list
    ?(scrutinee : PP.t option = None                                   )
    ~(matched   : Ast.Identifier.t                                     )
    ~(when_nil  : Ast.Statement.t                                      )
    ~(when_cons : Ast.Identifier.t * Ast.Identifier.t * Ast.Statement.t) : PP.t GC.t
  =
  let id_head, id_tail, when_cons_body = when_cons
  in
  let* pp_matched =
    GC.return @@ pp_scrutinee matched scrutinee
  and* pp_when_nil  = parenthesize @@ pp_statement when_nil
  (* the head and tail binders shadow any outer binding of the same name *)
  and* pp_when_cons =
    GC.with_compile_time_integers [ (id_head, None); (id_tail, None) ] begin
      parenthesize @@ pp_statement when_cons_body
    end
  in
  let pp_id_head = Identifier.pp id_head
  and pp_id_tail = Identifier.pp id_tail
  in
  GC.return begin
    PP.annotate [%here] begin
      MuSail.Statement.Match.pp_list
        ~matched_value:pp_matched
        ~when_nil:pp_when_nil
        ~head_identifier:pp_id_head
        ~tail_identifier:pp_id_tail
        ~when_cons:pp_when_cons
    end
  end

and pp_match_product
    ?(scrutinee : PP.t option = None)
    ~(matched : Ast.Identifier.t)
    ~(id_fst  : Ast.Identifier.t)
    ~(id_snd  : Ast.Identifier.t)
    ~(body    : Ast.Statement.t ) : PP.t GC.t
  =
  let* pp_matched =
    GC.return @@ pp_scrutinee matched scrutinee
  (* the binders shadow any outer binding of the same name *)
  and* pp_body =
    GC.with_compile_time_integers [ (id_fst, None); (id_snd, None) ] begin
      parenthesize @@ pp_statement body
    end
  in
  let pp_id_fst = PP.(surround dquotes) (Identifier.pp id_fst)
  and pp_id_snd = PP.(surround dquotes) (Identifier.pp id_snd)
  in
  GC.return begin
      PP.annotate [%here] begin
          MuSail.Statement.Match.pp_product
            ~matched_value:pp_matched
            ~fst_identifier:pp_id_fst
            ~snd_identifier:pp_id_snd
            ~body:pp_body
        end
    end

and pp_match_tuple
    ?(scrutinee : PP.t option = None                 )
    ~(matched  : Ast.Identifier.t                    )
    ~(binders  : (Ast.Identifier.t * Ast.Type.t) list)
    ~(body     : Ast.Statement.t                     ) : PP.t GC.t
  =
  let* pp_matched =
    GC.return @@ pp_scrutinee matched scrutinee
  (* the binders shadow any outer binding of the same name *)
  and* pp_body =
    GC.with_compile_time_integers (List.map binders ~f:(fun (id, _) -> (id, None))) begin
      parenthesize @@ pp_statement body
    end
  in
  let pp_binders : PP.t list =
    List.map binders ~f:(fun (id, _) -> PP.(surround dquotes) (Identifier.pp id))
  in
  GC.return begin
    PP.annotate [%here] begin
      MuSail.Statement.Match.pp_tuple
        ~matched_value:pp_matched
        ~binders:pp_binders
        ~body:pp_body
    end
  end

and pp_match_bool
    ?(scrutinee  : PP.t option = None)
    ~(condition  : Ast.Identifier.t)
    ~(when_true  : Ast.Statement.t )
    ~(when_false : Ast.Statement.t ) : PP.t GC.t
  =
  let* pp_condition =
    GC.return @@ pp_scrutinee condition scrutinee
  and* pp_when_true  = pp_statement when_true
  and* pp_when_false = pp_statement when_false
  in
  GC.return begin
      PP.annotate [%here] @@ begin
        MuSail.Statement.pp_conditional
          ~condition:pp_condition
          ~when_true:pp_when_true
          ~when_false:pp_when_false
      end
    end


(*
   Translates a match against enums.

   Two translations are supported:
   - pretty printed using the special match notation (see pp_using_match_notation)
   - ugly printed using the raw pp_match_enum
*)
and pp_match_enum
    ?(scrutinee    : PP.t option = None                  )
    ~(matched      : Ast.Identifier.t                    )
    ~(matched_type : Ast.Identifier.t                    )
    ~(cases        : Ast.Statement.t Ast.Identifier.Map.t) : PP.t GC.t
  =
  if
    Ast.Identifier.equal matched_type (Ast.Identifier.mk "unit")
  then
    (* deal with a match against unit separately *)
    match Ast.Identifier.Map.data cases with
    | [ clause ] -> begin
        pp_statement clause
      end
    | _ -> GC.fail [%here] "expected exactly one case for unit-typed matches"
  else begin
    (*
       Reified enum type
    *)
    let pp_matched_type : PP.t =
      PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_enum_name matched_type

    (*
       Statement whose value is being matched
        Nanosail only supports matching against variables, and muSail expects a statement,
       so we start with an identifier, which we turn into an expression, which we
       turn into a statement.
    *)
    and pp_matched_statement : PP.t =
      match scrutinee with
      | Some scrutinee -> PP.annotate [%here] scrutinee
      | None           -> PP.annotate [%here] @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_variable @@ Identifier.pp matched
    in
    (*
       Translates match statement using muSail's special notation.
        For example,
          enum MyEnum = { x, y }

         val foo : MyEnum -> int
         function foo(e) = match e {
           x => 1,
           y => 2
         }

       has its match expression converted to

         match: (stm_exp (exp_var "жmatched0")) in Emyenum with
         | x  =>  stm_exp (exp_int 1%Z)
         | y  =>  stm_exp (exp_int 2%Z)
         end

    *)
    let pp_using_match_notation () =
      let* pp_cases : PP.t list =
        (*
           Converts a match case

             Foo => statement

           to

             | Foo => statement
        *)
        let pp_case
            (constructor_identifier : Ast.Identifier.t)
            (clause_statement       : Ast.Statement.t ) : PP.t GC.t
          =
          let pp_pattern =
            Identifier.pp constructor_identifier
          in
          let* pp_clause =
            pp_statement clause_statement
          in
          GC.return begin
              PP.annotate [%here] begin
                  PP.(separate_horizontally ~separator:space [
                          string "|";
                          pp_pattern;
                          string " => ";
                          pp_clause
                  ])
                end
            end
        in
        GC.map ~f:(Fn.uncurry pp_case) (Ast.Identifier.Map.to_alist cases)
      in
      (*
         Generates final translation of match

           match: <matched> in <type> with
           | c1 -> stm
           | c2 -> stm
           end
      *)
      GC.return begin
          PP.annotate [%here] begin
              PP.vertical begin
                  List.build_list begin fun { add; addall; _ } -> begin
                        add @@ PP.annotate [%here] @@ PP.(separate_horizontally ~separator:space
                                     [
                                       string "match:";
                                       surround parens pp_matched_statement;
                                       string "in";
                                       pp_matched_type;
                                       string "with"
                                     ]
                               );
                        addall pp_cases;
                        add @@ PP.annotate [%here] @@ PP.string "end"
                      end
                    end
                end
            end
        end

    (*
       Alternative match translation function.
       Uses stm_match_enum.

       For example,

         enum MyEnum = { x, y }

         val foo : MyEnum -> int
         function foo(e) = match e {
           x => 1,
           y => 2
         }

       has its match expression converted to

         stm_match_enum Emyenum
                        (stm_exp (exp_var "жmatched0"))
                        (fun K => match K with
                                  | x => stm_exp (exp_int 1%Z)
                                  | y => stm_exp (exp_int 2%Z)
                                  end)
    *)
    and pp_using_stm_match_enum () =
      (*
         todo: should perhaps be a generated unique id
      *)
      let pp_lambda_parameter : PP.t =
        PP.annotate [%here] @@ PP.string "K"
      in
      let* pp_cases : PP.t =
        (*
           Translates a match into a pair of PP's
        *)
        let pp_case
            (constructor_identifier : Ast.Identifier.t)
            (clause_statement       : Ast.Statement.t ) : (PP.t * PP.t) GC.t
          =
          let pp_constructor =
            PP.annotate [%here] @@ Identifier.pp constructor_identifier
          in
          let* pp_clause =
            pp_statement clause_statement
          in
          GC.return (pp_constructor, pp_clause)
        in
        (*
           Generates the body of the lambda
        *)
        let* pp_lambda_body =
          let* pp_match_cases =
            GC.map ~f:(Fn.uncurry pp_case) @@ Ast.Identifier.Map.to_alist cases
          in
          (* Generate Coq match expression *)
          GC.return @@ PP.annotate [%here] @@ Coq.pp_match pp_lambda_parameter pp_match_cases
        in
        (* Wrap everything in a lambda *)
        GC.return @@ PP.annotate [%here] @@ Coq.pp_lambda pp_lambda_parameter pp_lambda_body
      in
      (*
         Puts everything together

           stm_match_enum <type> <matched> <lambda>
      *)
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_hanging_application
                (PP.string "stm_match_enum")
                [
                  pp_matched_type;
                  PP.(surround parens) pp_matched_statement;
                  PP.(surround parens) pp_cases;
                ]
            end
        end
    in
    (*
       Pick between pretty and ugly printing
       Note that pretty printing is only available for up to 14 cases
       (notations are hardcoded in Katamaran codebase up to 14 cases)
    *)
    if Configuration.(get pretty_print_match_enum) && Ast.Identifier.Map.length cases <= 14
    then GC.pp_annotate [%here] @@ pp_using_match_notation ()
    else GC.pp_annotate [%here] @@ pp_using_stm_match_enum ()
  end


(*
   Pretty prints a match where the matched value has a union/variant type.

   stm_match_union_alt_list <reified union type>
                            <statement evaluating to matched value>
                            [ existT <reified union constructor1> (MkAlt <identifiers1> <clause1 statement>);
                              existT <reified union constructor2> (MkAlt <identifiers2> <clause2 statement>);
                              ... ]
                            Logic.I
*)
and pp_match_variant
    ?(scrutinee    : PP.t option = None                                            )
    ~(matched      : Ast.Identifier.t                                              )
    ~(matched_type : Ast.Identifier.t                                              )
    ~(cases        : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t) : PP.t GC.t
  =
  (*
     Reified union type
  *)
  let* pp_matched_type = GC.lift ~f:(PP.annotate [%here]) @@ Expressions.pp_union_tag matched_type
  in
   (*
     Statement whose value is being matched

     Nanosail only supports matching against variables (i.e., not arbitrary expressions), and muSail expects a statement,
     so we start with an identifier, which we turn into an expression, which we
     turn into a statement.
  *)
  let pp_matched_statement =
    PP.annotate [%here] @@ pp_scrutinee matched scrutinee
  in

  (* List of match cases *)
  let* pp_cases : (PP.t * PP.t list * PP.t) list =
    let pp_case
        (constructor_id : Ast.Identifier.t     )
        (bindings       : Ast.Identifier.t list)
        (body           : Ast.Statement.t      ) : (PP.t * PP.t list * PP.t) GC.t
      =
      let pp_constructor =
        PP.annotate [%here] begin
          Identifier.pp @@ Identifier.reified_variant_constructor_name constructor_id
        end
      and pp_bindings =
        List.map ~f:Identifier.pp bindings
      in
      (*
         The bindings shadow whatever was in scope; since their values are not
         known at compile time, bind them to None so that an outer binding of the
         same name cannot leak into the body.
      *)
      let* pp_body =
        GC.with_compile_time_integers (List.map bindings ~f:(fun id -> (id, None))) begin
          pp_statement body
        end
      in
      GC.return (pp_constructor, pp_bindings, pp_body)
    in
    GC.map ~f:(fun (constructor, (pattern_ids, clause_statement)) -> pp_case constructor pattern_ids clause_statement) @@ Ast.Identifier.Map.to_alist cases
  in
  GC.return begin
    PP.annotate [%here] begin
      MuSail.Statement.Match.pp_variant
        ~matched_type:pp_matched_type
        ~matched_value:pp_matched_statement
        ~clauses:pp_cases
    end
  end


and pp_match_sum
    ?(scrutinee  : PP.t option = None                 )
    ~(matched    : Ast.Identifier.t                   )
    ~(when_left  : Ast.Identifier.t * Ast.Statement.t)
    ~(when_right : Ast.Identifier.t * Ast.Statement.t) : PP.t GC.t
  =
  let pp_matched_statement =
    PP.annotate [%here] @@ pp_scrutinee matched scrutinee
  in
  let pp_case ((binder, clause) : Ast.Identifier.t * Ast.Statement.t) : (PP.t * PP.t) GC.t =
    (* the binder shadows any outer binding of the same name *)
    let* pp_clause =
      GC.with_compile_time_integers [ (binder, None) ] begin
        parenthesize @@ pp_statement clause
      end
    in
    GC.return (Identifier.pp binder, pp_clause)
  in
  let* pp_left_identifier,  pp_when_left  = pp_case when_left
  and* pp_right_identifier, pp_when_right = pp_case when_right
  in
  GC.return begin
    PP.annotate [%here] begin
      MuSail.Statement.Match.pp_sum
        ~matched_value:pp_matched_statement
        ~left_identifier:pp_left_identifier
        ~when_left:pp_when_left
        ~right_identifier:pp_right_identifier
        ~when_right:pp_when_right
    end
  end


and pp_match
    ?(scrutinee     : PP.t option = None         )
    (match_pattern : Ast.Statement.match_pattern) : PP.t GC.t
  =
  match match_pattern with
  | MatchList { matched; when_nil; when_cons; _ }     -> pp_match_list ~scrutinee ~matched ~when_nil ~when_cons
  | MatchProduct { matched; id_fst; id_snd; body; _ } -> pp_match_product ~scrutinee ~matched ~id_fst ~id_snd ~body
  | MatchBool { condition; when_true; when_false }    -> pp_match_bool ~scrutinee ~condition ~when_true ~when_false
  | MatchEnum { matched; matched_type; cases }        -> pp_match_enum ~scrutinee ~matched ~matched_type ~cases
  | MatchVariant { matched; matched_type; cases }     -> pp_match_variant ~scrutinee ~matched ~matched_type ~cases
  | MatchTuple { matched; binders; body }             -> pp_match_tuple ~scrutinee ~matched ~binders ~body
  | MatchSum { matched; when_left; when_right; _ }    -> pp_match_sum ~scrutinee ~matched ~when_left ~when_right


and pp_call
      ~(function_identifier : Ast.Identifier.t     )
      ~(arguments           : Ast.Expression.t list) : PP.t GC.t
  =
  GC.pp_annotate [%here] begin
      GC.with_context (Printf.sprintf "call to %s" @@ Ast.Identifier.to_string function_identifier) begin
          FunctionCalls.translate function_identifier arguments
        end
    end


and pp_let
      ~(binder                 : Ast.Identifier.t)
      ~(binding_statement_type : Ast.Type.t      )
      ~(binding_statement      : Ast.Statement.t )
      ~(body_statement         : Ast.Statement.t ) : PP.t GC.t
  =
  let  pp_binder                 = PP.annotate [%here] @@ Identifier.pp binder
  in
  (*
     The bound value and its type belong to the enclosing scope: the binder is not
     yet visible while they are being processed. This is what makes

       let x = 1 in let x = x in ...

     resolve the right hand x to the outer one.
  *)
  let* pp_binding_statement      = pp_statement binding_statement
  in
  (* a width family's match has the width as _ in its union tag: only the let's type determines it *)
  let is_width_family =
    match Ast.Type.strip_aliases binding_statement_type with
    | Application (Variant _, [ NumericExpression _ ]) -> true
    | Application (_, _) | Int _ | Bool | String | Bit | List _ | Unit | Enum _ | Bitvector _ | Tuple _ | Sum (_, _)
    | Variant _ | Record _ | Range (_, _) | Function _ | TypeVariable _ | Nat | Vector (_, _) | Implicit _ | Alias (_, _) -> false
  in
  match if is_width_family then None else inlined_scrutinee_match binder body_statement with
  | Some match_pattern -> pp_match ~scrutinee:(Some pp_binding_statement) match_pattern
  | None ->
  let* pp_binding_statement_type = Type.pp_type binding_statement_type
  and* compile_time_value        =
    (* A mutable variable (one the body assigns to) has no fixed value *)
    if
      Ast.Identifier.Set.mem (Ast.Statement.assigned_variables body_statement) binder
    then
      GC.return None
    else
      extract_compile_time_integer_from_statement binding_statement
  in
  (*
     The body, on the other hand, does see the binder. Binding it to None when its
     value is unknown is deliberate: doing so shadows any outer binding of the same
     name, which would otherwise leak a stale value into the body.
  *)
  let* pp_body_statement =
    GC.with_compile_time_integer binder compile_time_value begin
      pp_statement body_statement
    end
  in
  if
    Configuration.(get pretty_print_let)
  then
    GC.return begin
        PP.annotate [%here] begin
            MuSail.Statement.pp_let_use_notation
              ~bound_identifier:pp_binder
              ~bound_value_type:pp_binding_statement_type
              ~bound_value:pp_binding_statement
              ~body:pp_body_statement
          end
      end
  else
    GC.return begin
        PP.annotate [%here] begin
            MuSail.Statement.pp_let
              ~bound_identifier:pp_binder
              ~bound_value_type:pp_binding_statement_type
              ~bound_value:pp_binding_statement
              ~body:pp_body_statement
          end
      end


and pp_sequence
      (left  : Ast.Statement.t)
      (right : Ast.Statement.t) : PP.t GC.t
  =
  let* pp_left  = parenthesize @@ pp_statement left
  and* pp_right = parenthesize @@ pp_statement right
  and* discarded_type_is_ambiguous = calls_function_with_ambiguous_result_type left
  in
  GC.return begin
      PP.annotate [%here] begin
          if
            discarded_type_is_ambiguous
          then
            (* the value is discarded and nothing determines its type: any type will do *)
            MuSail.Statement.pp_sequence ~left_type:(Some (PP.string "ty.unit")) pp_left pp_right
          else
            MuSail.Statement.pp_sequence pp_left pp_right
        end
    end


(*
   Whether the statement is a call to a function whose result type mentions a type variable that
   its parameter types do not (such as internal_error : forall ('a : Type). (string, int, string) -> 'a).
   Rocq cannot infer the type of such a call when its value is discarded (see TypeLevelParameters).
*)
and calls_function_with_ambiguous_result_type (statement : Ast.Statement.t) : bool GC.t =
  match statement with
  | Located (_, inner) -> calls_function_with_ambiguous_result_type inner
  (* lets that compute the call's arguments first: internal_error(__FILE__, __LINE__, "..." ^ BitStr(s)) *)
  | Let { body_statement; _ } -> calls_function_with_ambiguous_result_type body_statement
  | Call (function_identifier, _) -> begin
      let* function_definition = GC.lookup_definition_opt Ast.Definition.Select.(without_sail_definition @@ function_definition_named function_identifier)
      in
      match function_definition with
      | None                     -> GC.return false
      | Some function_definition -> begin
          let in_parameters = TypeLevelParameters.of_types @@ List.map ~f:snd function_definition.function_type.parameters
          and in_result     = TypeLevelParameters.of_type function_definition.function_type.return_type
          in
          GC.return @@ List.exists in_result ~f:(fun (identifier, _) ->
              not (List.exists in_parameters ~f:(fun (identifier', _) -> Ast.Identifier.equal identifier identifier'))
            )
        end
    end
  | Expression _ | Match _ | Seq _ | ReadRegister _ | WriteRegister _ | AssignVariable _
  | DestructureRecord _ | Cast _ | Fail _ | Unconstrained _ | Specialize _ -> GC.return false


and pp_read_register (register_identifier : Ast.Identifier.t) : PP.t GC.t =
  GC.return begin
      PP.annotate [%here] begin
          MuSail.Statement.pp_read_register @@ Identifier.pp register_identifier
        end
    end


and pp_write_register_statement
      ~(register_identifier : Ast.Identifier.t)
      ~(written_value       : Ast.Identifier.t) : PP.t GC.t
  =
  let pp_register_identifier = Identifier.pp register_identifier
  and pp_written_value = Identifier.pp written_value
  in
  GC.return begin
      PP.annotate [%here] begin
          MuSail.Statement.pp_write_register
            ~register_identifier:pp_register_identifier
            ~value_identifier:pp_written_value
        end
  end


(*
   Assignment to a mutable local variable:

     stm_assign "x" (s)
*)
and pp_assign_variable
      ~(variable_identifier : Ast.Identifier.t)
      ~(assigned_value      : Ast.Statement.t ) : PP.t GC.t
  =
  let* pp_assigned_value = pp_statement assigned_value
  in
  GC.return begin
      PP.annotate [%here] begin
          MuSail.Statement.pp_assign
            ~variable_identifier:(Identifier.pp variable_identifier)
            ~assigned_value:pp_assigned_value
        end
  end


and pp_destructure_record
      ~(record_type_identifier : Ast.Identifier.t     )
      ~(field_identifiers      : Ast.Identifier.t list)
      ~(binders                : Ast.Identifier.t list)
      ~(destructured_record    : Ast.Statement.t      )
      ~(body                   : Ast.Statement.t      ) : PP.t GC.t
  =
  let* pp_matched_type =
    Expressions.pp_record_tag record_type_identifier
  in
  let* pp_matched_value =
    pp_statement destructured_record
  in
  let pp_bindings =
    let pairs =
      List.zip_exn field_identifiers binders
    in
    List.map
      ~f:(fun (field, binder) -> (Identifier.pp field, Identifier.pp binder))
      pairs
  in
  (* the binders shadow any outer binding of the same name *)
  let* pp_body =
    GC.with_compile_time_integers (List.map binders ~f:(fun id -> (id, None))) begin
      pp_statement body
    end
  in
  GC.return begin
      PP.annotate [%here] begin
          MuSail.Statement.Match.pp_record
            ~matched_type:pp_matched_type
            ~matched_value:pp_matched_value
            ~bindings:pp_bindings
            ~body:pp_body
        end
    end


and pp_cast
    (statement_to_be_cast : Ast.Statement.t)
    (_target_type         : Ast.Type.t     ) : PP.t GC.t
  =
  let* () = GC.log [%here] Logging.warning @@ lazy (PP.string "Ignored cast")
  in
  GC.pp_annotate [%here] @@ pp_statement statement_to_be_cast


and pp_fail
    (typ     : Ast.Type.t)
    (message : string    ) : PP.t GC.t
  =
  let pp_message = Coq.pp_string message
  in
  let* pp_typ = Type.pp_type typ
  in
  GC.return @@ PP.annotate [%here] @@ MuSail.Statement.pp_fail pp_typ pp_message


(*
   An unconstrained value of type σ is a call to the foreign function (unconstrained σ),
   which the generated program declares (see ProgramModule.FunDeclKit and ForeignKit):

     stm_foreign (unconstrained (ty.bvec 32)) env.nil
*)
and pp_unconstrained (typ : Ast.Type.t) : PP.t GC.t =
  let* pp_typ = Type.pp_type typ
  in
  GC.return @@ PP.annotate [%here] @@ MuSail.Statement.pp_unconstrained pp_typ


(*
   A branch in which Sail's flow typing fixes size variables (see Ast.Statement.Specialize):

     match PeanoNat.Nat.eq_dec kid__p8_times_n 32 with
     | left Heq_kid__p8_times_n  => stm_let "data" (ty.bvec 32)
                                      (stm_exp (sail_cast (fun kid__p8_times_n => ty.bvec kid__p8_times_n) Heq_kid__p8_times_n (exp_var "data")))
                                      body
     | right _                   => stm_fail τ "unreachable: ..."
     end

   Only the variables that are parameters of the function (the type-level identifiers in scope)
   are decided: the others appear in no muSail type. The body is already substituted.
   If the result type mentions a decided variable, the body's value is cast back.
*)
and pp_specialize
    ~(equalities  : (Ast.Identifier.t * Z.t) list   )
    ~(facts       : Ast.Numeric.Constraint.t list   )
    ~(rebinds     : (Ast.Identifier.t * Ast.Type.t) list)
    ~(result_type : Ast.Type.t                      )
    ~(body        : Ast.Statement.t                 ) : PP.t GC.t
  =
  let* bound = GC.get_bound_type_level_identifiers
  in
  let decided : (Ast.Identifier.t * Z.t) list =
    match bound with
    | None       -> []
    | Some bound -> List.filter equalities ~f:(fun (identifier, _) -> Ast.Identifier.Set.mem bound identifier)
  in
  (* the facts over parameters only, and not closed (after the substitution of the equalities) *)
  let decided_facts : Ast.Numeric.Constraint.t list =
    match bound with
    | None       -> []
    | Some bound -> begin
        List.filter facts ~f:(fun fact ->
            match TypeLevelParameters.constraint_variables fact with
            | Some (_ :: _ as variables) -> List.for_all variables ~f:(Ast.Identifier.Set.mem bound)
            | Some [] | None             -> false
          )
      end
  in
  let mentions (typ : Ast.Type.t) (identifier : Ast.Identifier.t) : bool =
    List.exists (TypeLevelParameters.of_type typ) ~f:(fun (identifier', _) -> Ast.Identifier.equal identifier identifier')
  in
  let substitute_one ((identifier, value) : Ast.Identifier.t * Z.t) (typ : Ast.Type.t) : Ast.Type.t =
    Ast.Type.simplify @@ Ast.Type.substitute_numeric_expression_identifier (Ast.Numeric.size_variable_substitution [ (identifier, value) ]) typ
  in
  let proof_name (identifier : Ast.Identifier.t) : PP.t =
    PP.string @@ "Heq_" ^ Ast.Identifier.to_string identifier
  in
  (*
     Casts expression, of type typ, to typ with the decided variables replaced by their values,
     one variable at a time (backwards: the other way around, with eq_sym):

       sail_cast (fun kid__n => <typ, with the earlier variables replaced>) Heq_kid__n expression
  *)
  let pp_cast_chain ~(backwards : bool) (typ : Ast.Type.t) (expression : PP.t) : PP.t GC.t =
    let steps : (Ast.Identifier.t * Ast.Type.t) list =
      let _, steps =
        List.fold decided ~init:(typ, []) ~f:(fun (partially_substituted, steps) (identifier, value) ->
            let steps =
              if mentions partially_substituted identifier
              then (identifier, partially_substituted) :: steps
              else steps
            in
            (substitute_one (identifier, value) partially_substituted, steps)
          )
      in
      if backwards then steps else List.rev steps
    in
    GC.fold_left steps ~init:expression ~f:(fun expression (identifier, partially_substituted) ->
        let* pp_family = Type.pp_type partially_substituted
        in
        let pp_proof =
          if backwards
          then PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [ PP.string "eq_sym"; proof_name identifier ]
          else proof_name identifier
        in
        GC.return @@ PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [
          PP.string "sail_cast";
          PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [ PP.string "fun"; Identifier.pp identifier; PP.string "=>"; pp_family ];
          pp_proof;
          expression;
        ]
      )
  in
  let substitute_all (typ : Ast.Type.t) : Ast.Type.t =
    List.fold decided ~init:typ ~f:(fun typ equality -> substitute_one equality typ)
  in
  let pp_let_statement ~(binder : PP.t) ~(typ : Ast.Type.t) ~(bound_value : PP.t) ~(body : PP.t) : PP.t GC.t =
    let* pp_typ = Type.pp_type typ
    in
    GC.return @@ MuSail.Statement.pp_let ~bound_identifier:binder ~bound_value_type:pp_typ ~bound_value ~body
  in
  let* pp_body = pp_statement body
  in
  (* the value of the body, cast back if the result type mentions a decided variable *)
  let* pp_body =
    if
      List.exists decided ~f:(fun (identifier, _) -> mentions result_type identifier)
    then
      let binder = PP.string "ж_specialized"
      in
      let* pp_cast = pp_cast_chain ~backwards:true result_type (PP.(surround parens) @@ MuSail.Expression.pp_variable binder)
      in
      pp_let_statement ~binder ~typ:(substitute_all result_type) ~bound_value:pp_body ~body:(MuSail.Statement.pp_expression pp_cast)
    else
      GC.return pp_body
  in
  (* the variables whose type mentions a decided variable, rebound at the substituted type *)
  let* pp_body =
    GC.fold_left (List.rev rebinds) ~init:pp_body ~f:(fun pp_body (identifier, typ) ->
        if
          List.exists decided ~f:(fun (identifier', _) -> mentions typ identifier')
        then
          let binder = Identifier.pp identifier
          in
          let* pp_cast = pp_cast_chain ~backwards:false typ (PP.(surround parens) @@ MuSail.Expression.pp_variable binder)
          in
          pp_let_statement ~binder ~typ:(substitute_all typ) ~bound_value:(MuSail.Statement.pp_expression pp_cast) ~body:pp_body
        else
          GC.return pp_body
      )
  in
  let* pp_fail =
    let* pp_result_type = Type.pp_type result_type
    in
    let message =
      match decided with
      | [] -> "unreachable: Sail's typing ensures this branch's constraints"
      | _  -> begin
          Printf.sprintf "unreachable: Sail's typing fixes %s here"
            (String.concat ~sep:", " @@ List.map decided ~f:(fun (identifier, value) -> Printf.sprintf "%s = %s" (Ast.Identifier.to_string identifier) (Z.to_string value)))
        end
    in
    GC.return @@ MuSail.Statement.pp_fail pp_result_type (Coq.pp_string message)
  in
  (* the facts, decided as one conjunction *)
  let pp_body =
    match decided_facts with
    | []     -> pp_body
    | _ :: _ -> begin
        PP.(surround parens) begin
          Coq.pp_match
            (PP.separate_horizontally ~separator:PP.space [ PP.string "sail_decide"; TypeLevelParameters.pp_conjunction decided_facts ])
            [
              (PP.string "left Hfact", pp_body);
              (PP.string "right _", pp_fail);
            ]
        end
      end
  in
  GC.return begin
    PP.annotate [%here] begin
      List.fold_right decided ~init:pp_body ~f:(fun (identifier, value) pp_inner ->
          PP.(surround parens) begin
            Coq.pp_match
              (PP.separate_horizontally ~separator:PP.space [ PP.string "PeanoNat.Nat.eq_dec"; Identifier.pp identifier; PP.string (Z.to_string value) ])
              [
                (PP.separate_horizontally ~separator:PP.space [ PP.string "left"; proof_name identifier ], pp_inner);
                (PP.string "right _", pp_fail);
              ]
          end
        )
    end
  end


and pp_statement (statement : Ast.Statement.t) : PP.t GC.t =
  GC.pp_annotate [%here] begin
    match statement with
    | Expression expression                     -> pp_expression expression
    | Match match_pattern                       -> pp_match match_pattern
    | Call (function_identifier, arguments)     -> pp_call ~function_identifier ~arguments
    | Let
        { binder;
          binding_statement_type;
          binding_statement;
          body_statement }                      -> pp_let
                                                     ~binder
                                                     ~binding_statement_type
                                                     ~binding_statement
                                                     ~body_statement
    | Seq (s1, s2)                              -> pp_sequence s1 s2
    | ReadRegister register_identifier          -> pp_read_register register_identifier
    | WriteRegister { register_identifier;
                      written_value }           -> pp_write_register_statement
                                                     ~register_identifier
                                                     ~written_value
    | AssignVariable { variable_identifier;
                       assigned_value }         -> pp_assign_variable
                                                     ~variable_identifier
                                                     ~assigned_value
    | DestructureRecord
        { record_type_identifier;
          field_identifiers;
          binders;
          destructured_record;
          body }                                -> pp_destructure_record
                                                     ~record_type_identifier
                                                     ~field_identifiers
                                                     ~binders
                                                     ~destructured_record
                                                     ~body
    | Cast (statement_to_be_cast, target_type)  -> pp_cast statement_to_be_cast target_type
    | Fail (typ, message)                       -> pp_fail typ message
    | Unconstrained typ                         -> pp_unconstrained typ
    | Specialize { equalities;
                   facts;
                   rebinds;
                   result_type;
                   body }                       -> pp_specialize ~equalities ~facts ~rebinds ~result_type ~body
    | Located (location, inner_statement)       -> GC.with_sail_location location @@ pp_statement inner_statement
  end
