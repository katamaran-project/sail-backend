open Base
open Ast
open Auxlib
open Monads.Notations.Star(AnnotationContext)
open Expressions
open Identifier

module AC = AnnotationContext


let rec pp_statement (statement : statement) : PPrint.document AC.t =
  let pp_expression_statement (expression : expression) : PPrint.document AC.t =
    let* expression' = pp_par_expression expression
    in
    AC.return @@ PP.(simple_app [string "stm_exp"; expression'])

  and pp_match_statement (match_pattern : match_pattern) : PPrint.document AC.t =
    match match_pattern with
    | MP_list { matched; when_nil; when_cons } -> begin
        let id_head, id_tail, when_cons_body = when_cons
        in
        let* matched'   = pp_par_statement matched
        and* when_nil'  = pp_par_statement when_nil
        and* when_cons' = pp_par_statement when_cons_body
        in
        AC.return @@ PP.(simple_app [
            string "stm_match_list";
            matched';
            when_nil';
            dquotes (pp_identifier id_head);
            dquotes (pp_identifier id_tail);
            when_cons';
          ])
      end
      
    | MP_product { matched; id_fst; id_snd; body } -> begin
        let* matched' = pp_par_statement matched
        and* body'    = pp_par_statement body
        in
        AC.return @@ PP.(simple_app [
            string "stm_match_prod";
            matched';
            dquotes (pp_identifier id_fst);
            dquotes (pp_identifier id_snd);
            body';
          ])
      end
      
    | MP_bool { condition; when_true; when_false } -> begin
        let* condition'  = pp_par_statement condition
        and* when_true'  = pp_par_statement when_true
        and* when_false' = pp_par_statement when_false
        in
        AC.return @@ PP.(simple_app [
            string "stm_if";
            condition';
            when_true';
            when_false'
          ])
      end
      
    | MP_enum { matched; cases } -> begin
        let translate_case ~(key:identifier) ~(data:statement) (acc : PP.document list AC.t) =
          let* acc
          and* pattern = AC.return @@ pp_identifier key
          and* clause = pp_statement data
          in
          AC.return @@ PP.(separate space [
              string "|";
              pattern;
              string " => ";
              clause
            ] :: acc)
        in
        let* matched' = pp_par_statement matched
        and* cases' = IdentifierMap.fold cases ~init:(AC.return []) ~f:translate_case
        in
        AC.return @@ PP.separate PP.hardline @@ build_list @@ fun { add; addall; _ } -> begin
          add @@ Coq.comment @@ PP.string "TODO Fix this";
          add @@ PP.(separate space [ string "match"; matched'; string "with" ]);
          addall cases'
        end
      end
      
    | MP_variant { matched; cases } -> begin
        let _ = matched
        and _ = cases
        in
        AC.not_yet_implemented [%here]
      end

  in
  match statement with
  | Stm_exp e -> pp_expression_statement e
  | Stm_match match_pattern -> pp_match_statement match_pattern
  | Stm_call (function_identifier, arguments) -> begin
      let* pretty_printed_arguments = AC.map ~f:pp_par_expression arguments
      in
      FunctionCalls.translate function_identifier pretty_printed_arguments
    end

  | Stm_let (variable_identifier, s1, s2) -> begin
      let* s1' = pp_statement s1
      and* s2' = pp_statement s2
      in
      AC.return @@ PP.(
          simple_app [
            separate space [string "let:"; dquotes @@ pp_identifier variable_identifier; string ":="];
            s1';
            string "in";
            s2'
          ]
        )
    end

  | Stm_seq (s1, s2) -> begin
      let* s1' = pp_par_statement s1
      and* s2' = pp_par_statement s2
      in
      AC.return @@ PP.(simple_app [ string "stm_seq"; s1'; s2' ])
    end

  | Stm_read_register register_identifier -> begin
      AC.return @@ PP.(simple_app [ string "stm_read_register"; pp_identifier register_identifier ])
    end

  | Stm_write_register (register_identifier, rhs) -> begin
      let* rhs' = pp_statement rhs
      in
      AC.return @@ PP.simple_app [
          pp_identifier @@ Id.mk "stm_write_register";
          pp_identifier register_identifier;
          rhs'
        ]
    end

  | Stm_destructure_record { record_type_identifier;
                             field_identifiers;
                             variable_identifiers;
                             destructured_record;
                             body } -> begin
      let pattern =
        let pairs = List.zip_exn field_identifiers variable_identifiers
        in
        let build acc (field_identifier, variable_identifier) =
          PP.(parens (simple_app [
                          string "recordpat_snoc";
                          acc;
                          dquotes @@ pp_identifier field_identifier;
                          pp_identifier variable_identifier
                        ]))
        in
        List.fold_left pairs ~init:(PP.string "recordpat_nil") ~f:build
      in
      let* destructured_record' = pp_statement destructured_record
      and* body' = pp_statement body
      in
      AC.return @@ PP.simple_app [
                       pp_identifier @@ Id.mk "stm_match_record";
                       pp_identifier record_type_identifier;
                       PP.parens destructured_record';
                       pattern;
                       body'
                    ]
    end

  | Stm_cast (statement_to_be_cast, _target_type) -> begin
      Stdio.printf "Warning: ignored cast\n";
      pp_statement statement_to_be_cast
    end

  | Stm_fail message -> begin
      AC.return @@ PP.simple_app [ pp_identifier @@ Id.mk "fail"; PP.string message ]
    end

and pp_par_statement s =
  let* s' = pp_statement s
  in
  AC.return @@ PP.parens s'
