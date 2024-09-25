open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let rec pp_statement (statement : Ast.Statement.t) : PP.document GC.t =
  let pp_expression_statement (expression : Ast.Expression.t) : PP.document GC.t =
    let* expression' = Expressions.pp_expression expression
    in
    GC.return @@ PPSail.pp_statement_of_expression expression'

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
      let* matched'   = pp_par_statement matched
      and* when_nil'  = pp_par_statement when_nil
      and* when_cons' = pp_par_statement when_cons_body
      in
      GC.return @@ PP.(simple_app [
          string "stm_match_list";
          matched';
          when_nil';
          dquotes @@ Identifier.pp id_head;
          dquotes @@ Identifier.pp id_tail;
          when_cons';
        ])

    and pp_match_product
        (matched : Ast.Statement.t )
        (id_fst  : Ast.Identifier.t)
        (id_snd  : Ast.Identifier.t)
        (body    : Ast.Statement.t ) : PP.document GC.t
      =
      let* matched' = pp_par_statement matched
      and* body'    = pp_par_statement body
      in
      GC.return @@ PP.(simple_app [
          string "stm_match_prod";
          matched';
          dquotes (Identifier.pp id_fst);
          dquotes (Identifier.pp id_snd);
          body';
        ])

    and pp_match_bool
        (condition  : Ast.Statement.t)
        (when_true  : Ast.Statement.t)
        (when_false : Ast.Statement.t) : PP.document GC.t
      =
      let* condition'  = pp_par_statement condition
      and* when_true'  = pp_par_statement when_true
      and* when_false' = pp_par_statement when_false
      in
      GC.return @@ PP.(simple_app [
          string "stm_if";
          condition';
          when_true';
          when_false'
        ])

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
            pp_statement clause
          end
        | _ -> GC.fail "expected exactly one case for unit-typed matches"
      else begin
        (*
           Reified enum type
        *)
        let pp_matched_type : PP.document =
          Identifier.pp @@ Identifier.reified_enum_name matched_type

        (*
           Statement whose value is being matched

           Nanosail only supports matching against variables, and muSail expects a statement,
           so we start with an identifier, which we turn into an expression, which we
           turn into a statement.
        *)
        and pp_matched_statement : PP.document =
          PPSail.pp_statement_of_expression @@ PPSail.pp_expression_of_identifier @@ Identifier.pp matched
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
                Identifier.pp constructor_identifier
              in
              let* pp_clause =
                pp_statement clause_statement
              in
              GC.return @@ PP.(separate space [
                  string "|";
                  pp_pattern;
                  string " => ";
                  pp_clause
                ])
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
          GC.return @@ PP.vertical @@ Auxlib.build_list @@ fun { add; addall; _ } -> begin
            add @@ PP.(separate space [ string "match:"; parens pp_matched_statement; string "in"; pp_matched_type; string "with" ]);
            addall pp_cases;
            add @@ PP.string "end"
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
          let pp_lambda_parameter : PP.document =
            PP.string "K"
          in
          let* pp_cases : PP.document =
            let pp_case
                (constructor_identifier : Ast.Identifier.t)
                (clause_statement       : Ast.Statement.t ) : (PP.document * PP.document) GC.t
              =
              let pp_constructor =
                Identifier.pp constructor_identifier
              in
              let* pp_clause =
                pp_statement clause_statement
              in
              GC.return (pp_constructor, pp_clause)
            in
            let* pp_lambda_body =
              let* pp_match_cases =
                GC.map ~f:(Auxlib.uncurry pp_case) @@ Ast.Identifier.Map.to_alist cases
              in
              GC.return @@ Coq.pp_match pp_lambda_parameter pp_match_cases
            in
            GC.return @@ Coq.pp_lambda pp_lambda_parameter pp_lambda_body
          in
          GC.return @@ PP.hanging_list
            (PP.string "stm_match_enum")
            [
              pp_matched_type;
              PP.parens pp_matched_statement;
              PP.parens pp_cases;
            ]
        in
        if Configuration.(get pretty_print_match_enum) && Ast.Identifier.Map.length cases <= 14
        then pp_using_match_notation ()
        else pp_using_stm_match_enum ()
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
        Identifier.pp @@ Configuration.reified_variant_name matched_type

      (*
         Statement whose value is being matched

         Nanosail only supports matching against variables, and muSail expects a statement,
         so we start with an identifier, which we turn into an expression, which we
         turn into a statement.
      *)
      and pp_matched_statement =
        PP.parens @@ PPSail.pp_statement_of_expression @@ PPSail.pp_expression_of_identifier @@ Identifier.pp matched

      in

      (* List of match cases *)
      let* pp_cases =
        let pp_case
            (constructor_id : Ast.Identifier.t     )
            (bindings       : Ast.Identifier.t list)
            (clause         : Ast.Statement.t      ) : PP.document GC.t
          =
          let pp_constructor =
            Identifier.pp @@ Configuration.reified_variant_constructor_name constructor_id
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
                PP.parens @@ PP.simple_app [
                  PP.string "pat_var";
                  PP.dquotes @@ Identifier.pp x
                ]
              end
            | [x; y] -> begin
                PP.parens @@ PP.simple_app [
                  PP.string "pat_pair";
                  PP.dquotes @@ Identifier.pp x;
                  PP.dquotes @@ Identifier.pp y
                ]
              end
            | ids -> begin
                let pp_variable_tuple =
                  let pp_quoted_identifiers =
                    List.map ~f:(Fn.compose PP.dquotes Identifier.pp) ids
                  in
                  let pp_comma_separated_quoted_identifiers =
                    PP.separate (PP.string ", ") pp_quoted_identifiers
                  in
                  PP.parens pp_comma_separated_quoted_identifiers
                in
                PP.parens @@ PP.simple_app @@ [ PP.string "pat_tuple"; pp_variable_tuple ]
              end
          in
          let* pp_clause =
            pp_statement clause
          in
          (*
             Construct output for single case

               existT <constructor> (MkAlt <pattern> <clause>)
          *)
          GC.return @@ PP.simple_app [
            PP.string "existT";
            pp_constructor;
            PP.parens begin
              PP.simple_app [
                PP.string "MkAlt";
                pp_pattern;
                PP.parens pp_clause;
              ]
            end
          ]
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
        PP.hanging_list
          (PP.string "stm_match_union_alt_list")
          [
            pp_matched_type;
            pp_matched_statement;
            Coq.pp_list pp_cases;
            PP.string "Logic.I"
          ]
      end
    in
    match match_pattern with
    | MatchList { matched; when_nil; when_cons }     -> pp_match_list matched when_nil when_cons
    | MatchProduct { matched; id_fst; id_snd; body } -> pp_match_product matched id_fst id_snd body
    | MatchBool { condition; when_true; when_false } -> pp_match_bool condition when_true when_false
    | MatchEnum { matched; matched_type; cases }     -> pp_match_enum matched matched_type cases
    | MatchVariant { matched; matched_type; cases }  -> pp_match_variant matched matched_type cases

  and pp_call_statement
      (function_identifier : Ast.Identifier.t     )
      (arguments           : Ast.Expression.t list) : PP.document GC.t
    =
    let* pretty_printed_arguments = GC.map ~f:(fun e -> GC.lift ~f:PP.parens @@ Expressions.pp_expression e) arguments
    in
    FunctionCalls.translate function_identifier pretty_printed_arguments

  and pp_let_statement (args : Ast.Statement.let_arguments) : PP.document GC.t
    =
    let {
        variable_identifier;
        binding_statement_type;
        binding_statement;
        body_statement
      } : Ast.Statement.let_arguments = args
    in
    let  variable_identifier'    = Identifier.pp variable_identifier in
    let* binding_statement'      = pp_statement binding_statement
    and* binding_statement_type' = Nanotype.pp_nanotype binding_statement_type
    and* body_statement'         = pp_statement body_statement
    in
    if
      Configuration.(get pretty_print_let)
    then
      GC.return @@ PP.(
        simple_app [
            separate space [
                string "let:";
                dquotes variable_identifier';
                string "::";
                binding_statement_type';
                string ":="];
            binding_statement';
            string "in";
            body_statement'
          ]
        )
    else
      GC.return @@ PP.(
        simple_app [
            string "stm_let";
            dquotes variable_identifier';
            parens binding_statement_type';
            parens binding_statement';
            parens body_statement';
          ]
        )

  and pp_sequence_statement
      (left  : Ast.Statement.t)
      (right : Ast.Statement.t) : PP.document GC.t
    =
      let* pp_left  = pp_par_statement left
      and* pp_right = pp_par_statement right
      in
      GC.return @@ PP.(simple_app [ string "stm_seq"; pp_left; pp_right ])

  and pp_read_register_statement (register_identifier : Ast.Identifier.t) : PP.document GC.t =
    GC.return @@ PP.(simple_app [ string "stm_read_register"; Identifier.pp register_identifier ])

  and pp_write_register_statement (args : Ast.Statement.write_register_arguments) : PP.document GC.t =
    let register_identifier = Identifier.pp args.register_identifier
    and rhs = Identifier.pp args.written_value
    in
    GC.return begin
      PP.simple_app [
        Identifier.pp @@ Ast.Identifier.mk "stm_write_register";
        register_identifier;
        PP.(parens @@ simple_app [string "exp_var"; dquotes rhs]);
      ]
    end

  and pp_destructure_record_statement (args: Ast.Statement.destructure_record_arguments) : PP.document GC.t =
    let {
      record_type_identifier;
      field_identifiers;
      variable_identifiers;
      destructured_record;
      body
    } : Ast.Statement.destructure_record_arguments = args
    in
    let pattern =
      let pairs = List.zip_exn field_identifiers variable_identifiers
      in
      let build acc (field_identifier, variable_identifier) =
        PP.(parens (simple_app [
            string "recordpat_snoc";
            acc;
            dquotes @@ Identifier.pp field_identifier;
            Identifier.pp variable_identifier
          ]))
      in
      List.fold_left pairs ~init:(PP.string "recordpat_nil") ~f:build
    in
    let* destructured_record' = pp_statement destructured_record
    and* body' = pp_statement body
    in
    GC.return @@ PP.simple_app [
      Identifier.pp @@ Ast.Identifier.mk "stm_match_record";
      Identifier.pp record_type_identifier;
      PP.parens destructured_record';
      pattern;
      body'
    ]

  and pp_cast_statement
      (statement_to_be_cast : Ast.Statement.t)
      (_target_type         : Ast.Type.t     ) : PP.document GC.t
    =
    Stdio.printf "Warning: ignored cast\n";
    pp_statement statement_to_be_cast

  and pp_fail_statement (message : string) : PP.document GC.t =
    GC.return @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "fail"; PP.string message ]

  in
  match statement with
  | Expression expression                     -> pp_expression_statement expression
  | Match match_pattern                       -> pp_match_statement match_pattern
  | Call (function_identifier, arguments)     -> pp_call_statement function_identifier arguments
  | Let let_data                              -> pp_let_statement let_data
  | Seq (s1, s2)                              -> pp_sequence_statement s1 s2
  | ReadRegister register_identifier          -> pp_read_register_statement register_identifier
  | WriteRegister args                        -> pp_write_register_statement args
  | DestructureRecord destructure_record      -> pp_destructure_record_statement destructure_record
  | Cast (statement_to_be_cast, target_type)  -> pp_cast_statement statement_to_be_cast target_type
  | Fail message                              -> pp_fail_statement message

and pp_par_statement s =
  let* s' = pp_statement s
  in
  GC.return @@ PP.parens s'
