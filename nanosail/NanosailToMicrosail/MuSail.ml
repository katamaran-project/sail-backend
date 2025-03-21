(*

   Contains functionality to generate muSail output.

*)
open ExtBase


module Value = struct
  let pp_integer (value : Z.t) : PP.t =
    Coq.pp_z_integer value

  let pp_bit (value : bool) : PP.t =
    Coq.pp_bool value


  let pp_bitvector (size : int) (value : Z.t) : PP.t =
    Coq.pp_explicit_application
      (PP.string "bv.mk")
      [
        PP.integer size;
        PP.string @@ Z.to_string value;
        PP.string "I";
      ]
end


module Operator = struct
  module Infix = struct
    module Bitvector = struct
      let signed_less_than                  = PP.string "<ˢ"
      let signed_less_than_or_equal_to      = PP.string "<=ˢ"
      let signed_greater_than               = PP.string ">ˢ"
      let signed_greater_than_or_equal_to   = PP.string ">=ˢ"
      let unsigned_less_than                = PP.string "<ᵘ"
      let unsigned_less_than_or_equal_to    = PP.string "<=ᵘ"
      let unsigned_greater_than             = PP.string ">ᵘ"
      let unsigned_greater_than_or_equal_to = PP.string ">=ᵘ"

      let addition                          = PP.string "+ᵇ"
      let subtraction                       = PP.string "-ᵇ"
      let equality                          = PP.string "="
      let inequality                        = PP.string "!="
    end

    let bit_equality                        = PP.string "="

    let bool_equality                       = PP.string "="
    let bool_inequality                     = PP.string "!="
  end

  module Name = struct
    let bitvector_addition                  = PP.string "bop.bvadd"
    let bitvector_subtraction               = PP.string "bop.bvsub"
    let bitvector_conjunction               = PP.string "bop.bvand"
    let bitvector_disjunction               = PP.string "bop.bvor"
    let bitvector_xor                       = PP.string "bop.bvxor"

    let bool_equality                       = PP.(surround parens) @@ Coq.pp_application (PP.string "bop.relop") [ PP.string "bop.eq" ]
    let bool_inequality                     = PP.(surround parens) @@ Coq.pp_application (PP.string "bop.relop") [ PP.string "bop.neq" ]
  end
end


module Pattern = struct
  (*
    pat_var "<identifier>"

    Consider using smart_pp instead
  *)
  let pp_variable (identifier : PP.t) : PP.t =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "pat_var")
        [ PP.(surround dquotes) identifier ]
    end


  (*
    pat_pair "<first>" "<second>"

    Consider using smart_pp instead
  *)
  let pp_pair
      (first_identifier  : PP.t)
      (second_identifier : PP.t) : PP.t
    =
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "pat_pair")
        [
          PP.(surround dquotes) first_identifier;
          PP.(surround dquotes) second_identifier
        ]
    end


  (*
    pat_tuple [ "<id1>", "<id2>", ... ]

    Consider using smart_pp instead
  *)
  let pp_tuple (identifiers : PP.t list) : PP.t =
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

  (*
     Depending on the number of identifiers,
     picks the right way to represent the binding.
  *)
  let smart_pp (identifiers : PP.t list) : PP.t =
    match identifiers with
    | []     -> raise @@ Invalid_argument "smart_pp expects at least one identifier"
    | [x]    -> pp_variable x
    | [x; y] -> pp_pair x y
    | _      -> pp_tuple identifiers
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
  let pp_integer (value : PP.t) =
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
  let pp_string (str : PP.t) =
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
      ~(typ   : PP.t)
      ~(value : PP.t)
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
  let pp_variable (identifier : PP.t) =
    PP.annotate [%here] begin
      PP.separate_horizontally
        ~separator:PP.space
        [
          PP.string "exp_var";
          PP.surround PP.dquotes identifier
        ]
    end


  (*
     exp_val (bvec <size>) [bv <value>]
  *)
  let pp_bitvector
      ~(size  : int)
      ~(value : Z.t) : PP.t
    =
    let pp_type =
      PP.separate_horizontally ~separator:PP.space [ PP.string "ty.bvec"; PP.string @@ Int.to_string size ]
    in
    let pp_literal =
      PP.(surround brackets) begin
          Coq.pp_application
            (PP.string "bv")
            [PP.string @@ Z.to_string value]
      end
    in
    PP.annotate [%here] begin
      pp_value ~typ:pp_type ~value:pp_literal
    end


  (*
    exp_val (ty.bvec 32) (@Bitvector.bv.zero 32)
  *)
  let pp_zero_bitvector_using_function (number_of_bits : int) : PP.t =
    PP.annotate [%here] begin
      let typ =
        Coq.pp_application (PP.string "ty.bvec") [ PP.integer number_of_bits ]
      and value =
        Coq.pp_explicit_application (PP.string "Bitvector.bv.zero") [ PP.integer number_of_bits ]
      in
      pp_value ~typ ~value
    end


  (*
     exp_val (ty.bvec 32) ([bv 0])
  *)
  let pp_zero_bitvector_using_literal (number_of_bits : int) : PP.t =
    PP.annotate [%here] begin
      pp_bitvector ~size:number_of_bits ~value:Z.zero
    end


  (*
    exp_val (ty.bvec 32) (@Bitvector.bv.one 32)
  *)
  let pp_ones_bitvector_using_function (number_of_bits : int) : PP.t =
    PP.annotate [%here] begin
      let typ =
        Coq.pp_application (PP.string "ty.bvec") [ PP.integer number_of_bits ]
      and value =
        Coq.pp_explicit_application (PP.string "Bitvector.bv.one") [ PP.integer number_of_bits ]
      in
      pp_value ~typ ~value
    end


  let pp_ones_bitvector_using_literal (number_of_bits : int) : PP.t =
    PP.annotate [%here] begin
      let value = Z.sub (Z.shift_left Z.one number_of_bits) Z.one
      in
      pp_bitvector ~size:number_of_bits ~value
    end
end


module Statement = struct
  (*
     "Upgrades" expression to statements

       stm_exp (<expression>)
  *)
  let pp_expression (expression : PP.t) : PP.t =
    PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "stm_exp")
          [ PP.(surround parens) expression ]
      end


  let pp_fail (typ : PP.t) (message : PP.t) : PP.t =
    Coq.pp_application (PP.string "stm_fail") [ PP.(surround parens) typ; message ]


  let pp_assert
        ~(condition : PP.t)
        ~(message   : PP.t) : PP.t
    =
    Coq.pp_application
      (PP.string "stm_assert")
      [ PP.(surround parens) condition; PP.(surround parens) message ]


  (* No-op *)
  let pp_nop =
    pp_expression @@ Expression.pp_unit ()


  module Match = struct
    (*
       stm_match_list <matched_value>
                      <when_nil>
                      "<head_identifier>"
                      "<tail_identifier>"
                      <when_cons>
    *)
    let pp_list
        ~(matched_value   : PP.t)
        ~(when_nil        : PP.t)
        ~(head_identifier : PP.t)
        ~(tail_identifier : PP.t)
        ~(when_cons       : PP.t)
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


    (*
      stm_match_union_alt_list <matched_type>
                               <matched_value>
                               [
                                 existT <clauses[i][0]> (MkAlt (<pattern for clauses[i][1]>) (<clauses[i][2]>);
                                 ...
                               ]
    *)
    let pp_variant
        ~(matched_type  : PP.t                          )
        ~(matched_value : PP.t                          )
        ~(clauses       : (PP.t * PP.t list * PP.t) list) : PP.t
      =
      let pp_cases =
        let pp_case
            (constructor : PP.t     )
            (bindings    : PP.t list)
            (body        : PP.t     ) : PP.t
          =
          let pattern =
            PP.(surround parens) begin
              Pattern.smart_pp bindings
            end
          in
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
      stm_match_prod <matched_value>
                     <fst_identifier>
                     <snd_identifier>
                     <body>
    *)
    let pp_product
          ~(matched_value  : PP.t)
          ~(fst_identifier : PP.t)
          ~(snd_identifier : PP.t)
          ~(body           : PP.t) : PP.t
      =
      PP.annotate [%here] begin
          Coq.pp_hanging_application
            (PP.string "stm_match_prod")
            [
              matched_value;
              fst_identifier;
              snd_identifier;
              body
            ]
        end


    let pp_tuple
        ~(matched_value : PP.t     )
        ~(binders       : PP.t list)
        ~(body          : PP.t     ) : PP.t
      =
      let binders =
        let rec build_binder_list elements =
          match elements with
          | []        -> PP.string "tuplepat_nil"
          | elt::elts -> Coq.pp_application
                           (PP.string "tuplepat_snoc")
                           [
                             PP.(surround parens) @@ build_binder_list elts;
                             elt
                           ]
        in
        build_binder_list binders
      in
      PP.annotate [%here] begin
          Coq.pp_hanging_application
            (PP.string "stm_match_tuple")
            [
              matched_value;
              PP.(surround parens) binders;
              body;
            ]
        end


    let pp_record
          ~(matched_type  : PP.t              )
          ~(matched_value : PP.t              )
          ~(bindings      : (PP.t * PP.t) list)
          ~(body          : PP.t              ) : PP.t
      =
      let record_pattern =
        let build acc (field_identifier, binder) =
          PP.(surround parens) begin
              Coq.pp_application
                (PP.string "recordpat_snoc")
                [
                  acc;
                  PP.(surround dquotes) field_identifier;
                  PP.(surround dquotes) binder;
                ]
            end
        in
        List.fold_left bindings ~init:(PP.string "recordpat_nil") ~f:build
      in
      PP.annotate [%here] begin
          Coq.pp_hanging_application
            (PP.string "stm_match_record")
            [
              matched_type;
              PP.(surround parens) matched_value;
              record_pattern;
              PP.(surround parens) body
            ]
        end
  end

  (*
     (call <function_name> <arguments[0]> <arguments[1]> ...)%exp
  *)
  let pp_call_using_notation
      (function_name : PP.t     )
      (arguments     : PP.t list) : PP.t
    =
    PP.annotate [%here] begin
      Coq.pp_scope ~scope:(PP.string "exp") begin
        Coq.pp_application
          (PP.string "call")
          (function_name :: arguments)
      end
    end


  (*
     stm_call <function_name> (env.snoc (env.snoc (env.snoc env.nil (_::_) arg1) (_::_) arg2) (_::_) arg3)
  *)
  let pp_call
      (function_name : PP.t     )
      (arguments     : PP.t list) : PP.t
    =
    let pp_arguments =
      let add_snoc tail argument =
        Coq.pp_hanging_application
          (PP.string "env.snoc")
          [
            PP.(surround parens) tail;
            PP.string "(_::_)";
            Coq.pp_scope ~scope:(PP.string "exp") argument
          ]
      in
      List.fold_left
        arguments
        ~init:(PP.string "env.nil")
        ~f:add_snoc
    in
    PP.annotate [%here] begin
      Coq.pp_application
        (PP.string "stm_call")
        [
          function_name;
          PP.(surround parens) pp_arguments
        ]
    end


  (*
    stm_if (<condition>)
           (<when_true>)
           (<when_false>)
  *)
  let pp_conditional
        ~(condition  : PP.t)
        ~(when_true  : PP.t)
        ~(when_false : PP.t) : PP.t
    =
    PP.annotate [%here] @@ begin
        Coq.pp_hanging_application
          (PP.string "stm_if")
          [
            PP.(surround parens) condition;
            PP.(surround parens) when_true;
            PP.(surround parens) when_false
          ]
      end

  (*
    stm_seq <left>
            <right>
  *)
  let pp_sequence
        (left  : PP.t)
        (right : PP.t) : PP.t
    =
    PP.annotate [%here] begin
        Coq.pp_hanging_application
          (PP.string "stm_seq")
          [
            left;
            right;
          ]
      end


  (*
    stm_read_register <register_identifier>
  *)
  let pp_read_register (register_identifier : PP.t) : PP.t =
    PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "stm_read_register")
          [ register_identifier ]
      end


  (*
    Note: our AST currently restricts the source
    of the value to be written: it has to be stored
    in a variable, i.e., arbitrary expressions
    are not supported.

    stm_write_register <register_identifier>
                       (exp_var "<value_identifier>")
  *)
  let pp_write_register
        ~(register_identifier : PP.t)
        ~(value_identifier    : PP.t) : PP.t
    =
    PP.annotate [%here] begin
        Coq.pp_application
          (PP.string "stm_write_register")
          [
            register_identifier;
            PP.(surround parens @@ Coq.pp_application
                                     (string "exp_var")
                                     [ surround dquotes value_identifier ]);
          ]
      end


  (*
    stm_let "<bound_identifier>"
            (<bound_value_type>)
            (<bound_value>)
            (<body>)
  *)
  let pp_let
        ~(bound_identifier : PP.t)
        ~(bound_value_type : PP.t)
        ~(bound_value      : PP.t)
        ~(body             : PP.t) : PP.t
    =
    PP.annotate [%here] begin
        Coq.pp_hanging_application
          (PP.string "stm_let")
          [
            PP.(surround dquotes) bound_identifier;
            PP.(surround parens) bound_value_type;
            PP.(surround parens) bound_value;
            PP.(surround parens) body;
          ]
      end


  (*
    let: "<bound_identifier>" :: <bound_value_type> := bound_value
    in
      <body>
  *)
  let pp_let_use_notation
        ~(bound_identifier : PP.t)
        ~(bound_value_type : PP.t)
        ~(bound_value      : PP.t)
        ~(body             : PP.t) : PP.t
    =
    PP.annotate [%here] begin
        PP.(
        vertical [
            separate_horizontally ~separator:space [
                string "let:";
                surround dquotes bound_identifier;
                string "::";
                bound_value_type;
                string ":=";
                bound_value];
            string "in";
            indent body
          ]
        )
      end
end


(*
   "<argument>" ∷ <typ>
*)
let pp_bind
    (argument : PP.t)
    (typ      : PP.t) : PP.t
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
