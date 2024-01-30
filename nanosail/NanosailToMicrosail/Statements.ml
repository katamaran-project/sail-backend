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

  | Stm_call (f, arg_list) ->
     let* arg_list' = AC.map ~f:pp_par_expression arg_list
     in
     AC.return @@ simple_app @@ string "call" :: string f :: arg_list'

  | Stm_let (v, s1, s2) ->
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

  | Stm_seq (s1, s2) ->
     let* s1' = pp_par_statement s1
     and* s2' = pp_par_statement s2
     in
     AC.return @@ simple_app [ string "stm_seq"; s1'; s2' ]

and pp_par_statement s =
  let* s' = pp_statement s
  in
  AC.return @@ parens s'
