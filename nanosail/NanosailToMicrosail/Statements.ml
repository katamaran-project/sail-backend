open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let rec pp_statement (statement : Ast.Statement.t) : PPrint.document GC.t =
  let pp_expression_statement (expression : Ast.Expression.t) : PPrint.document GC.t =
    let* expression' = Expressions.pp_par_expression expression
    in
    GC.return @@ PP.(simple_app [string "stm_exp"; expression'])

  and pp_match_statement (match_pattern : Ast.Statement.match_pattern) : PPrint.document GC.t =
    match match_pattern with
    | List { matched; when_nil; when_cons } -> begin
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
            dquotes (Identifier.pp id_head);
            dquotes (Identifier.pp id_tail);
            when_cons';
          ])
      end

    | Product { matched; id_fst; id_snd; body } -> begin
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
      end

    | Bool { condition; when_true; when_false } -> begin
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
      end

    | Enum { matched; matched_type; cases } -> begin
        if Ast.Identifier.equal matched_type (Ast.Identifier.mk "unit")
        then
          match Ast.Identifier.Map.data cases with
          | [ clause ] -> begin
              pp_statement clause
            end
          | _      -> GC.fail "expected exactly one case for unit-typed matches"
        else begin
          let translate_case ~(key : Ast.Identifier.t) ~(data : Ast.Statement.t) (acc : PP.document list GC.t) =
            let* acc
            and* pattern = GC.return @@ Identifier.pp key
            and* clause = pp_statement data
            in
            GC.return @@ PP.(separate space [
                string "|";
                pattern;
                string " => ";
                clause
              ] :: acc)
          in
          let* matched' = pp_par_statement matched
          and* cases' = Ast.Identifier.Map.fold cases ~init:(GC.return []) ~f:translate_case
          in
          let matched_type =
            Identifier.pp @@ Identifier.reified_enum_name matched_type
          in
          GC.return @@ PP.separate PP.hardline @@ Auxlib.build_list @@ fun { add; addall; _ } -> begin
            add @@ PP.(separate space [ string "match:"; matched'; string "in"; matched_type; string "with" ]);
            addall cases';
            add @@ PP.string "end"
          end
        end
      end
        
    | Variant { matched; cases } -> begin
        let _ = matched
        and _ = cases
        in
        GC.not_yet_implemented [%here]
      end

  and pp_call_statement
      (function_identifier : Ast.Identifier.t     )
      (arguments           : Ast.Expression.t list) : PPrint.document GC.t
    =
    let* pretty_printed_arguments = GC.map ~f:Expressions.pp_par_expression arguments
    in
    FunctionCalls.translate function_identifier pretty_printed_arguments

  and pp_let_statement (let_data : Ast.Statement.let_data) : PPrint.document GC.t
    =
    let {
        variable_identifier;
        binding_statement_type;
        binding_statement;
        body_statement
      } : Ast.Statement.let_data = let_data
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
      (right : Ast.Statement.t) : PPrint.document GC.t
    =
      let* left'  = pp_par_statement left
      and* right' = pp_par_statement right
      in
      GC.return @@ PP.(simple_app [ string "stm_seq"; left'; right' ])

  and pp_read_register_statement (register_identifier : Ast.Identifier.t) : PPrint.document GC.t =
    GC.return @@ PP.(simple_app [ string "stm_read_register"; Identifier.pp register_identifier ])

  and pp_write_register_statement
      (register_identifier : Ast.Identifier.t)
      (rhs                 : Ast.Statement.t ) : PPrint.document GC.t
    =
    let* rhs' = pp_statement rhs
    in
    GC.return @@ PP.simple_app [
      Identifier.pp @@ Ast.Identifier.mk "stm_write_register";
      Identifier.pp register_identifier;
      PP.parens rhs'
    ]

  and pp_destructure_record_statement (destructure_record : Ast.Statement.destructure_record) : PPrint.document GC.t =
    let {
      record_type_identifier;
      field_identifiers;
      variable_identifiers;
      destructured_record;
      body
    } : Ast.Statement.destructure_record = destructure_record
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
      (_target_type         : Ast.Type.t     ) : PPrint.document GC.t
    =
    Stdio.printf "Warning: ignored cast\n";
    pp_statement statement_to_be_cast

  and pp_fail_statement (message : string) : PPrint.document GC.t =
    GC.return @@ PP.simple_app [ Identifier.pp @@ Ast.Identifier.mk "fail"; PP.string message ]

  in
  match statement with
  | Expression expression                     -> pp_expression_statement expression
  | Match match_pattern                       -> pp_match_statement match_pattern
  | Call (function_identifier, arguments)     -> pp_call_statement function_identifier arguments
  | Let let_data                              -> pp_let_statement let_data
  | Seq (s1, s2)                              -> pp_sequence_statement s1 s2
  | ReadRegister register_identifier          -> pp_read_register_statement register_identifier
  | WriteRegister (register_identifier, rhs)  -> pp_write_register_statement register_identifier rhs
  | DestructureRecord destructure_record      -> pp_destructure_record_statement destructure_record
  | Cast (statement_to_be_cast, target_type)  -> pp_cast_statement statement_to_be_cast target_type
  | Fail message                              -> pp_fail_statement message

and pp_par_statement s =
  let* s' = pp_statement s
  in
  GC.return @@ PP.parens s'
