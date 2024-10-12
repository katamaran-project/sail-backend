open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let rec pp_statement (statement : Ast.Statement.t) : PP.document GC.t =
  let pp_expression_statement (expression : Ast.Expression.t) : PP.document GC.t =
    let* pp_expression = GC.pp_annotate [%here] @@ Expressions.pp_expression expression
    in
    GC.return @@ PP.annotate [%here] @@ MuSail.Statement.pp_expression pp_expression

  and pp_match_statement (match_pattern : Ast.Statement.match_pattern) : PP.document GC.t =
    (*

       Converts the following Sail code

         match <matched> {
           [||]               => when_nil,
           id_head :: id_tail => when_cons_body
         }

    *)
    let pp_match_list
        (matched   : Ast.Statement.t                                      )
        (when_nil  : Ast.Statement.t                                      )
        (when_cons : Ast.Identifier.t * Ast.Identifier.t * Ast.Statement.t) : PP.document GC.t
      =
      let id_head, id_tail, when_cons_body = when_cons
      in
      let* pp_matched   = GC.pp_annotate [%here] @@ pp_par_statement matched
      and* pp_when_nil  = GC.pp_annotate [%here] @@ pp_par_statement when_nil
      and* pp_when_cons = GC.pp_annotate [%here] @@ pp_par_statement when_cons_body
      in
      let pp_id_head = Identifier.pp id_head
      and pp_id_tail = Identifier.pp id_tail
      in
      GC.return begin
        PP.annotate [%here] begin
          MuSail.Statement.pp_match_list
            ~matched_value:pp_matched
            ~when_nil:pp_when_nil
            ~head_identifier:pp_id_head
            ~tail_identifier:pp_id_tail
            ~when_cons:pp_when_cons
        end
      end

    and pp_match_product
        (matched : Ast.Statement.t )
        (id_fst  : Ast.Identifier.t)
        (id_snd  : Ast.Identifier.t)
        (body    : Ast.Statement.t ) : PP.document GC.t
      =
      let* pp_matched = GC.pp_annotate [%here] @@ pp_par_statement matched
      and* pp_body    = GC.pp_annotate [%here] @@ pp_par_statement body
      in
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_hanging_application
                (PP.string "stm_match_prod")
                [
                  pp_matched;
                  PP.(surround dquotes) (Identifier.pp id_fst);
                  PP.(surround dquotes) (Identifier.pp id_snd);
                  pp_body;
                ]
            end
        end

    and pp_match_bool
        (condition  : Ast.Statement.t)
        (when_true  : Ast.Statement.t)
        (when_false : Ast.Statement.t) : PP.document GC.t
      =
      let* pp_condition  = GC.pp_annotate [%here] @@ pp_par_statement condition
      and* pp_when_true  = GC.pp_annotate [%here] @@ pp_par_statement when_true
      and* pp_when_false = GC.pp_annotate [%here] @@ pp_par_statement when_false
      in
      GC.return begin
          PP.annotate [%here] @@ begin
              Coq.pp_hanging_application
                (PP.string "stm_if")
                [
                  pp_condition;
                  pp_when_true;
                  pp_when_false
                ]
            end
        end


    (*
       Translates a match against enums.

       Two translations are supported:
       - pretty printed using the special match notation (see pp_using_match_notation)
       - ugly printed using the raw pp_match_enum
    *)
    and pp_match_enum
        (matched      : Ast.Identifier.t                    )
        (matched_type : Ast.Identifier.t                    )
        (cases        : Ast.Statement.t Ast.Identifier.Map.t) : PP.document GC.t
      =
      if
        Ast.Identifier.equal matched_type (Ast.Identifier.mk "unit")
      then
        (* deal with a match against unit separately *)
        match Ast.Identifier.Map.data cases with
        | [ clause ] -> begin
            GC.pp_annotate [%here] @@ pp_statement clause
          end
        | _ -> GC.fail "expected exactly one case for unit-typed matches"
      else begin
        (*
           Reified enum type
        *)
        let pp_matched_type : PP.document =
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_enum_name matched_type

        (*
           Statement whose value is being matched

           Nanosail only supports matching against variables, and muSail expects a statement,
           so we start with an identifier, which we turn into an expression, which we
           turn into a statement.
        *)
        and pp_matched_statement : PP.document =
          PP.annotate [%here] @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_variable @@ Identifier.pp matched
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
          let* pp_cases : PP.document list =
            (*
               Converts a match case

                 Foo => statement

               to

                 | Foo => statement
            *)
            let pp_case
                (constructor_identifier : Ast.Identifier.t)
                (clause_statement       : Ast.Statement.t ) : PP.document GC.t
              =
              let pp_pattern =
                PP.annotate [%here] @@ Identifier.pp constructor_identifier
              in
              let* pp_clause =
                GC.pp_annotate [%here] @@ pp_statement clause_statement
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
            GC.map ~f:(Auxlib.uncurry pp_case) (Ast.Identifier.Map.to_alist cases)
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
                      Auxlib.build_list begin fun { add; addall; _ } -> begin
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
          let pp_lambda_parameter : PP.document =
            PP.annotate [%here] @@ PP.string "K"
          in
          let* pp_cases : PP.document =
            (*
               Translates a match into a pair of PP.documents
            *)
            let pp_case
                (constructor_identifier : Ast.Identifier.t)
                (clause_statement       : Ast.Statement.t ) : (PP.document * PP.document) GC.t
              =
              let pp_constructor =
                PP.annotate [%here] @@ Identifier.pp constructor_identifier
              in
              let* pp_clause =
                GC.pp_annotate [%here] @@ pp_statement clause_statement
              in
              GC.return (pp_constructor, pp_clause)
            in
            (*
               Generates the body of the lambda
            *)
            let* pp_lambda_body =
              let* pp_match_cases =
                GC.map ~f:(Auxlib.uncurry pp_case) @@ Ast.Identifier.Map.to_alist cases
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
        (matched      : Ast.Identifier.t                                              )
        (matched_type : Ast.Identifier.t                                              )
        (cases        : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t) : PP.document GC.t
      =
      (*
         Reified union type
      *)
      let pp_matched_type =
        PP.annotate [%here] @@ Identifier.pp @@ Configuration.reified_variant_name matched_type

      (*
         Statement whose value is being matched

         Nanosail only supports matching against variables, and muSail expects a statement,
         so we start with an identifier, which we turn into an expression, which we
         turn into a statement.
      *)
      and pp_matched_statement =
        PP.annotate [%here] begin
          PP.(surround parens) @@ MuSail.Statement.pp_expression @@ MuSail.Expression.pp_variable @@ Identifier.pp matched
        end
      in

      (* List of match cases *)
      let* pp_cases =
        let pp_case
            (constructor_id : Ast.Identifier.t     )
            (bindings       : Ast.Identifier.t list)
            (clause         : Ast.Statement.t      ) : PP.document GC.t
          =
          let pp_constructor =
            PP.annotate [%here] @@ Identifier.pp @@ Configuration.reified_variant_constructor_name constructor_id
          (*

             The pattern's translation depends on how many fields the union cases carries with it.

             Zero : never occurs.

                        union Foo { Bar : unit }

                    In this example, Bar has one field of type unit.

             One : becomes pat_var "<id>"

             Two : becomes pat_pair "<id1>" "<id2>"

             Three or more : becomes pat_tuple ("<id1>", "<id2>", ...)

          *)
          and pp_pattern =
            match bindings with
            | [] -> failwith "Should not occur: zero parameters are actually represented using a single unit parameters"
            | [x] -> begin
                PP.annotate [%here] begin
                  PP.(surround parens) begin
                    MuSail.Pattern.pp_variable (Identifier.pp x)
                  end
                end
              end
            | [x; y] -> begin
                PP.annotate [%here] begin
                  PP.(surround parens) begin
                    MuSail.Pattern.pp_pair (Identifier.pp x) (Identifier.pp y)
                  end
                end
              end
            | ids -> begin
                let pp_variable_tuple =
                  let pp_quoted_identifiers =
                    List.map ~f:(Fn.compose PP.(surround dquotes) Identifier.pp) ids
                  in
                  let pp_comma_separated_quoted_identifiers =
                    PP.separate_horizontally ~separator:(PP.string ", ") pp_quoted_identifiers
                  in
                  PP.annotate [%here] @@ PP.(surround parens) pp_comma_separated_quoted_identifiers
                in
                PP.annotate [%here] @@ PP.(surround parens) @@ Coq.pp_application (PP.string "pat_tuple") [ pp_variable_tuple ]
              end
          in
          let* pp_clause =
            GC.pp_annotate [%here] @@ pp_statement clause
          in
          (*
             Construct output for single case

               existT <constructor> (MkAlt <pattern> <clause>)
          *)
          GC.return begin
              PP.annotate [%here] begin
                  Coq.pp_application
                    (PP.string "existT")
                    [
                      pp_constructor;
                      PP.(surround parens) begin
                          Coq.pp_application (PP.string "MkAlt") [
                              pp_pattern;
                              PP.(surround parens) pp_clause;
                            ]
                        end
                    ]
                end
            end
        in
        GC.map ~f:(fun (constructor, (pattern_ids, clause_statement)) -> pp_case constructor pattern_ids clause_statement) @@ Ast.Identifier.Map.to_alist cases
      in
      (*
         Constructor final output for match

           stm_match_union_alt_list <type>
                                    <matched>
                                    [<case1>; <case2>; ...]
                                    Logic.I
      *)
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_hanging_application
                (PP.string "stm_match_union_alt_list")
                [
                  pp_matched_type;
                  pp_matched_statement;
                  Coq.pp_list pp_cases;
                  PP.string "Logic.I"
                ]
            end
      end
    in
    match match_pattern with
    | MatchList { matched; when_nil; when_cons }     -> GC.pp_annotate [%here] @@ pp_match_list matched when_nil when_cons
    | MatchProduct { matched; id_fst; id_snd; body } -> GC.pp_annotate [%here] @@ pp_match_product matched id_fst id_snd body
    | MatchBool { condition; when_true; when_false } -> GC.pp_annotate [%here] @@ pp_match_bool condition when_true when_false
    | MatchEnum { matched; matched_type; cases }     -> GC.pp_annotate [%here] @@ pp_match_enum matched matched_type cases
    | MatchVariant { matched; matched_type; cases }  -> GC.pp_annotate [%here] @@ pp_match_variant matched matched_type cases

  and pp_call_statement
      (function_identifier : Ast.Identifier.t     )
      (arguments           : Ast.Expression.t list) : PP.document GC.t
    =
    let* pretty_printed_arguments =
      GC.map ~f:(fun e -> GC.lift ~f:PP.(surround parens) @@ Expressions.pp_expression e) arguments
    in
    GC.pp_annotate [%here] begin
        FunctionCalls.translate function_identifier pretty_printed_arguments
      end

  and pp_let_statement
        ~(variable_identifier    : Ast.Identifier.t)
        ~(binding_statement_type : Ast.Type.t      )
        ~(binding_statement      : Ast.Statement.t )
        ~(body_statement         : Ast.Statement.t ) : PP.document GC.t
    =
    let  pp_variable_identifier    = PP.annotate [%here] @@ Identifier.pp variable_identifier
    in
    let* pp_binding_statement      = GC.pp_annotate [%here] @@ pp_statement binding_statement
    and* pp_binding_statement_type = GC.pp_annotate [%here] @@ Nanotype.pp_nanotype binding_statement_type
    and* pp_body_statement         = GC.pp_annotate [%here] @@ pp_statement body_statement
    in
    if
      Configuration.(get pretty_print_let)
    then
      GC.return begin
          PP.annotate [%here] begin
              PP.(
              separate_horizontally ~separator:space [
                  separate_horizontally ~separator:space [
                      string "let:";
                      surround dquotes pp_variable_identifier;
                      string "::";
                      pp_binding_statement_type;
                      string ":="];
                  pp_binding_statement;
                  string "in";
                  pp_body_statement
                ]
              )
            end
        end
    else
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_hanging_application
                (PP.string "stm_let")
                [
                  PP.(surround dquotes) pp_variable_identifier;
                  PP.(surround parens) pp_binding_statement_type;
                  PP.(surround parens) pp_binding_statement;
                  PP.(surround parens) pp_body_statement;
                ]
            end
        end


  and pp_sequence_statement
      (left  : Ast.Statement.t)
      (right : Ast.Statement.t) : PP.document GC.t
    =
      let* pp_left  = GC.pp_annotate [%here] @@ pp_par_statement left
      and* pp_right = GC.pp_annotate [%here] @@ pp_par_statement right
      in
      GC.return begin
          PP.annotate [%here] begin
              Coq.pp_hanging_application
                (PP.string "stm_seq")
                [
                  pp_left;
                  pp_right;
                ]
            end
        end

  and pp_read_register_statement (register_identifier : Ast.Identifier.t) : PP.document GC.t =
    GC.return begin
        PP.annotate [%here] begin
            Coq.pp_application
              (PP.string "stm_read_register")
              [ Identifier.pp register_identifier ]
          end
      end

  and pp_write_register_statement
        ~(register_identifier : Ast.Identifier.t)
        ~(written_value       : Ast.Identifier.t) : PP.document GC.t
    =
    let pp_register_identifier = Identifier.pp register_identifier
    and pp_written_value = Identifier.pp written_value
    in
    GC.return begin
        PP.annotate [%here] begin
            Coq.pp_application
              (PP.string "stm_write_register")
              [
                pp_register_identifier;
                PP.(surround parens @@ Coq.pp_application
                                         (string "exp_var")
                                         [ surround dquotes pp_written_value ]);
              ]
          end
    end

  and pp_destructure_record_statement
        ~(record_type_identifier : Ast.Identifier.t     )
        ~(field_identifiers      : Ast.Identifier.t list)
        ~(variable_identifiers   : Ast.Identifier.t list)
        ~(destructured_record    : Ast.Statement.t      )
        ~(body                   : Ast.Statement.t      ) : PP.document GC.t
    =
    let pattern =
      let pairs =
        List.zip_exn field_identifiers variable_identifiers
      in
      let build acc (field_identifier, variable_identifier) =
        PP.(surround parens (Coq.pp_application
                               (PP.annotate [%here] @@ string "recordpat_snoc")
                               [
                                 PP.annotate [%here] @@ acc;
                                 PP.annotate [%here] @@ surround dquotes @@ Identifier.pp field_identifier;
                                 PP.annotate [%here] @@ surround dquotes @@ Identifier.pp variable_identifier;
                               ]))
      in
      List.fold_left pairs ~init:(PP.string "recordpat_nil") ~f:build
    in
    let* destructured_record' =
      GC.pp_annotate [%here] @@ pp_statement destructured_record
    and* body' =
      GC.pp_annotate [%here] @@ pp_statement body
    in
    GC.return begin
        PP.annotate [%here] begin
            Coq.pp_application
              (PP.string "stm_match_record")
              [
                Identifier.pp @@ Configuration.reified_record_name record_type_identifier;
                PP.(surround parens) destructured_record';
                pattern;
                PP.(surround parens) body'
              ]
          end
      end

  and pp_cast_statement
      (statement_to_be_cast : Ast.Statement.t)
      (_target_type         : Ast.Type.t     ) : PP.document GC.t
    =
    Logging.info @@ lazy "Warning: ignored cast";
    GC.pp_annotate [%here] @@ pp_statement statement_to_be_cast

  and pp_fail_statement (message : string) : PP.document GC.t =
    GC.return @@ PP.annotate [%here] @@ Coq.pp_application (PP.string "fail") [ PP.string message ]

  in
  match statement with
  | Expression expression                     -> GC.pp_annotate [%here] @@ pp_expression_statement expression
  | Match match_pattern                       -> GC.pp_annotate [%here] @@ pp_match_statement match_pattern
  | Call (function_identifier, arguments)     -> GC.pp_annotate [%here] @@ pp_call_statement function_identifier arguments
  | Let
      { variable_identifier;
        binding_statement_type;
        binding_statement;
        body_statement }                      -> GC.pp_annotate [%here] @@ pp_let_statement
                                                                             ~variable_identifier
                                                                             ~binding_statement_type
                                                                             ~binding_statement
                                                                             ~body_statement
  | Seq (s1, s2)                              -> GC.pp_annotate [%here] @@ pp_sequence_statement s1 s2
  | ReadRegister register_identifier          -> GC.pp_annotate [%here] @@ pp_read_register_statement register_identifier
  | WriteRegister { register_identifier;
                    written_value }           -> GC.pp_annotate [%here] @@ pp_write_register_statement
                                                                             ~register_identifier
                                                                             ~written_value
  | DestructureRecord
      { record_type_identifier;
        field_identifiers;
        variable_identifiers;
        destructured_record;
        body }                                -> GC.pp_annotate [%here] @@ pp_destructure_record_statement
                                                                             ~record_type_identifier
                                                                             ~field_identifiers
                                                                             ~variable_identifiers
                                                                             ~destructured_record
                                                                             ~body
  | Cast (statement_to_be_cast, target_type)  -> GC.pp_annotate [%here] @@ pp_cast_statement statement_to_be_cast target_type
  | Fail message                              -> GC.pp_annotate [%here] @@ pp_fail_statement message

and pp_par_statement s =
  let* s' = pp_statement s
  in
  GC.return @@ PP.(surround parens) s'
