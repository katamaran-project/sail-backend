open Base


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
          typ;
          value;
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
end
