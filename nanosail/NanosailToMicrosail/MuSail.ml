open Base


module Pattern = struct
  (*
     pat_var "<identifier>"
  *)
  let pp_variable (identifier : PP.document) : PP.document =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "pat_var")
        [ PP.(surround dquotes) identifier ]
    end


  (*
     pat_pair "<first>" "<second>"
  *)
  let pp_pair
      (first_identifier  : PP.document)
      (second_identifier : PP.document) : PP.document
    =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "pat_pair")
        [
          PP.(surround dquotes) first_identifier;
          PP.(surround dquotes) second_identifier
        ]
    end


  let pp_tuple (identifiers : PP.document list) : PP.document =
    PP.annotate [%here] begin
      let pp_variable_tuple =
        let quoted_identifiers =
          List.map ~f:PP.(surround dquotes) identifiers
        in
        let comma_separatoed =
          PP.separate_horizontally ~separator:(PP.string ", ") quoted_identifiers
        in
        let parenthesized =
          PP.(surround parens) comma_separatoed
        in
        PP.separate_horizontally
          ~separator:(PP.string ", ")
          [ parenthesized ]
      in
      Coq.pp_application (PP.string "pat_tuple") [ pp_variable_tuple ]
    end
end


module Expression = struct
  (*
     exp_true
  *)
  let pp_true () =
    PP.annotate [%here] @@ PP.string "exp_true"


  (*
     exp_false
  *)
  let pp_false () =
    PP.annotate [%here] @@ PP.string "exp_false"


  (*
     exp_int <value>
  *)
  let pp_integer (value : PP.document) =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "exp_int")
        [
          value
        ]
    end


  (*
     exp_string <str>
  *)
  let pp_string (str : PP.document) =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "exp_string")
        [
          str
        ]
    end


  (*
     exp_val <typ> <value>
  *)
  let pp_value
      ~(typ   : PP.document)
      ~(value : PP.document)
    =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "exp_val")
        [
          PP.(surround parens) typ;
          PP.(surround parens) value;
        ]
    end

  
  (*
     exp_val ty.unit tt
  *)
  let pp_unit () =
    let typ =
      PP.string "ty.unit"
    and value =
      PP.string "tt"
    in
    PP.annotate [%here] begin
      pp_value ~typ ~value
    end


  (*
     exp_var "<identifier>"
  *)
  let pp_variable (identifier : PP.document) =
    PP.annotate [%here] begin
      PP.separate_horizontally
        ~separator:PP.space
        [
          PP.string "exp_var";
          PP.surround PP.dquotes identifier
        ]
    end
end


module Statement = struct
  (*
     "Upgrades" expression to statements
     
       stm_exp (<expression>)
  *)
  let pp_expression (expression : PP.document) : PP.document =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "stm_exp")
        [ PP.(surround parens) expression ]
    end


  (*
     stm_match_list <matched_value>
                    <when_nil>
                    "<head_identifier>"
                    "<tail_identifier>"
                    <when_cons>
  *)
  let pp_match_list
      ~(matched_value   : PP.document)
      ~(when_nil        : PP.document)
      ~(head_identifier : PP.document)
      ~(tail_identifier : PP.document)
      ~(when_cons       : PP.document)
    =
    PP.annotate [%here] begin
      Coq.pp_hanging_application
        (PP.string "stm_match_list")
        [
          matched_value;
          when_nil;
          PP.(surround dquotes) head_identifier;
          PP.(surround dquotes) tail_identifier;
          when_cons;
        ]
    end


  let pp_match_variant
      ~(matched_type : PP.document)
      ~(matched_value : PP.document)
      ~(clauses : (PP.document * PP.document list * PP.document) list) : PP.document
    =
    let pp_cases =
      let pp_case
          (constructor : PP.document)
          (bindings : PP.document list)
          (body : PP.document) : PP.document
        =
        let pattern =
          match bindings with
          | [] -> failwith "Should not occur: zero parameters are actually represented using a single unit parameters"
          | [x] -> begin
              PP.annotate [%here] begin
                PP.(surround parens) begin
                  Pattern.pp_variable x
                end
              end
            end
          | [x; y] -> begin
              PP.annotate [%here] begin
                PP.(surround parens) begin
                  Pattern.pp_pair x y
                end
              end
            end
          | ids -> begin
              PP.annotate [%here] begin
                PP.(surround parens) @@ Pattern.pp_tuple ids
              end
            end
        in
        PP.annotate [%here] begin
          Coq.pp_application
            (PP.string "existT")
            [
              constructor;
              PP.(surround parens) begin
                Coq.pp_application (PP.string "MkAlt") [
                  pattern;
                  PP.(surround parens) body;
                ]
              end
            ]
        end
      in
      Coq.pp_list @@ List.map ~f:(fun (constructor, pattern_ids, body) -> pp_case constructor pattern_ids body) clauses
    in
    PP.annotate [%here] begin
      Coq.pp_hanging_application
        (PP.string "stm_match_union_alt_list")
        [
          matched_type;
          matched_value;
          pp_cases;
          PP.string "Logic.I"
        ]
    end
      

  
  (*
     (call <function_identifier> <arguments[0]> <arguments[1]> ...)%exp
  *)
  let pp_call
      (function_identifier : Ast.Identifier.t)
      (arguments           : PP.document list) : PP.document
    =
    PP.annotate [%here] begin
      Coq.pp_scope (PP.string "exp") begin
        Coq.pp_application
          (PP.string "call")
          (Identifier.pp function_identifier :: arguments)
      end
    end
end


(*
   "<argument>" ∷ <typ>
*)
let pp_bind
    (argument : PP.document)
    (typ      : PP.document) : PP.document
  =
  PP.annotate [%here] begin
      PP.separate_horizontally
        ~separator:PP.space
        [
          PP.surround PP.dquotes argument;
          PP.string " ∷ ";
          typ
        ]
    end
