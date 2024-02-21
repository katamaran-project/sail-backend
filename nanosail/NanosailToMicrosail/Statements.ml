open Base
open PP
open Ast
open Auxlib
open Monads.Notations.Star(AnnotationContext)
open Expressions

module AC = AnnotationContext


let rec pp_statement statement =
  match statement with
  | Stm_exp e ->
     let* e' = pp_par_expression e
     in
     AC.return @@ simple_app [string "stm_exp"; e']

  | Stm_match match_pattern -> begin
      match match_pattern with
      | MP_list { matched; when_nil; when_cons } -> begin
          let id_head, id_tail, when_cons_body = when_cons
          in
          let* matched'   = pp_par_statement matched
          and* when_nil'  = pp_par_statement when_nil
          and* when_cons' = pp_par_statement when_cons_body
          in
          AC.return @@ simple_app [
            string "stm_match_list";
            matched';
            when_nil';
            dquotes (string id_head);
            dquotes (string id_tail);
            when_cons';
          ]
        end

      | MP_product { matched; id_fst; id_snd; body } -> begin
          let* matched' = pp_par_statement matched
          and* body'    = pp_par_statement body
          in
          AC.return @@ simple_app [
            string "stm_match_prod";
            matched';
            dquotes (string id_fst);
            dquotes (string id_snd);
            body';
          ]
        end

      | MP_bool { condition; when_true; when_false } -> begin
          let* condition'  = pp_par_statement condition
          and* when_true'  = pp_par_statement when_true
          and* when_false' = pp_par_statement when_false
          in
          AC.return @@ simple_app [
            string "stm_if";
            condition';
            when_true';
            when_false'
          ]
        end

      | MP_enum { matched; cases } -> begin
          let translate_case ~(key:string) ~(data:statement) (acc : document list AC.t) =
            let* acc
            and* pattern = AC.return @@ string key
            and* clause = pp_statement data
            in
            AC.return @@ separate space [
              string "|";
              pattern;
              string " => ";
              clause
            ] :: acc
          in
          let* matched' = pp_par_statement matched
          and* cases' = StringMap.fold cases ~init:(AC.return []) ~f:translate_case
          in
          AC.return @@ separate hardline @@ build_list @@ fun { add; addall; _ } -> begin
            add @@ Coq.comment @@ string "TODO Fix this";
            add @@ separate space [ string "match"; matched'; string "with" ];
            addall cases'
          end
        end
    end

  | Stm_call (function_identifier, arg_list) -> begin
      let* arg_list' = AC.map ~f:pp_par_expression arg_list
      in
      let default_translation () =
        let terms =
          build_list @@ fun { add; addall; _ } -> begin
                            add @@ string "call";
                            add @@ string function_identifier;
                            addall @@ arg_list'
                          end
        in
        AC.return @@ simple_app terms
      in
      match function_identifier with
      | "add_bits_int" -> begin
          match arg_list' with
          | [x; y] -> AC.return @@ PP.parens (x ^^ string "+" ^^ y)
          | _      -> default_translation ()
        end
      | _ -> default_translation ()
    end

  | Stm_let (v, s1, s2) -> begin
      let* s1' = pp_statement s1
      and* s2' = pp_statement s2
      in
      AC.return @@
        simple_app [
            string ("let: \"" ^ v ^ "\" :=");
            s1';
            string "in";
            s2'
          ]
    end

  | Stm_seq (s1, s2) -> begin
      let* s1' = pp_par_statement s1
      and* s2' = pp_par_statement s2
      in
      AC.return @@ simple_app [ string "stm_seq"; s1'; s2' ]
    end

  | Stm_read_register register_identifier -> begin
      AC.return @@ simple_app [ string "stm_read_register"; string register_identifier ]
    end

  | Stm_write_register (register_identifier, rhs) -> begin
      let* rhs' = pp_statement rhs
      in
      AC.return @@ simple_app [
          string "stm_write_register";
          string register_identifier;
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
                          dquotes @@ string field_identifier;
                          string variable_identifier
                        ]))
        in
        List.fold_left pairs ~init:(string "recordpat_nil") ~f:build
      in
      let* destructured_record' = pp_statement destructured_record
      and* body' = pp_statement body
      in
      AC.return @@ simple_app [
                       string "stm_match_record";
                       string record_type_identifier;
                       PP.parens destructured_record';
                       pattern;
                       body'
                    ]
    end

  | Stm_cast (statement_to_be_cast, _target_type) -> begin
      Stdio.printf "Warning: ignored cast\n";
      pp_statement statement_to_be_cast
    end

and pp_par_statement s =
  let* s' = pp_statement s
  in
  AC.return @@ parens s'
