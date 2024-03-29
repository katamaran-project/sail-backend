open Nanosail.Ast

module Id = Nanosail.Id


let var s = Exp_var (Id.mk s)

let fun_is_empty = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l");
      when_nil  = Stm_exp (Exp_val (Val_bool true));
      when_cons = (Id.mk "h", Id.mk "t", Stm_exp (Exp_val (Val_bool false)))
    }
  end

let fun_empty = Stm_exp (Exp_list [])

let fun_onetwothree = Stm_exp (Exp_list [Exp_val (Val_int (Big_int.of_int 1));
                                        Exp_val (Val_int (Big_int.of_int 2));
                                        Exp_val (Val_int (Big_int.of_int 3));])

let fun_last = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l");
      when_nil  = Stm_exp (Exp_val (Val_prod (Val_int (Big_int.of_int 0), Val_bool false)));
      when_cons = (
        Id.mk "h",
        Id.mk "t",
        Stm_match begin
          MP_list {
            matched   = Stm_exp (var "t");
            when_nil  = Stm_exp (Exp_binop (Pair, var "h", Exp_val (Val_bool true)));
            when_cons = (Id.mk "h'", Id.mk "t'", Stm_call (Id.mk "last", [var "t"]))
          }
        end
      )
    }
  end

let fun_append = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l1");
      when_nil  = Stm_exp (var "l2");
      when_cons = (Id.mk "h", Id.mk "t", Stm_let (Id.mk "r", Stm_call (Id.mk "append", [var "t"; var "l2"]), Stm_exp (Exp_binop (Cons, var "h", var "r"))))
    }
  end

let fun_length = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l");
      when_nil  = Stm_exp (Exp_val (Val_int (Big_int.of_int 0)));
      when_cons = (Id.mk "h", Id.mk "t", Stm_let (Id.mk "n", Stm_call (Id.mk "length", [var "t"]), Stm_exp (Exp_binop (Plus, var "n", Exp_val (Val_int (Big_int.of_int 1))))))
    }
  end

let fun_reverse_aux = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l");
      when_nil  = Stm_exp (var "acc");
      when_cons = (Id.mk "h", Id.mk "t", Stm_call (Id.mk "reverse_aux", [var "t"; Exp_binop (Cons, var "h", var "acc")]))
    }
  end

let fun_reverse = Stm_call (Id.mk "reverse_aux", [var "l"; Exp_list []])

let fun_reverse_bis = Stm_match begin
    MP_list {
      matched   = Stm_exp (var "l");
      when_nil  = Stm_exp (Exp_list []);
      when_cons = (Id.mk "h", Id.mk "t", Stm_let (Id.mk "r", Stm_call (Id.mk "reverse_bis", [var "t"]), Stm_exp (Exp_binop (Append, var "r", Exp_list [var "h"]))))
    }
  end


(******************************************************************************)
(* Intermediate Representation Lists *)


let list_of t =
  Ty_list t

let product_of ts =
  Ty_tuple ts

let funDefList = [
  {
    function_name = Id.mk "is_empty";
    function_type = {
      arg_types   = [(Id.mk "l", list_of Ty_int)];
      ret_type    = Ty_bool
    };
    function_body = fun_is_empty
  };
  {
    function_name = Id.mk "empty";
    function_type = {
      arg_types   = [(Id.mk "tt", Ty_unit)];
      ret_type    = list_of Ty_int
    };
    function_body = fun_empty
  };
  {
    function_name = Id.mk "onetwothree";
    function_type = {
      arg_types   = [(Id.mk "tt", Ty_unit)];
      ret_type    = list_of Ty_int
    };
    function_body = fun_onetwothree
  };
  {
    function_name = Id.mk "last";
    function_type = {
      arg_types   = [(Id.mk "l", list_of Ty_int)];
      ret_type    = product_of [Ty_int; Ty_bool]
    };
    function_body = fun_last
  };
  {
    function_name = Id.mk "append";
    function_type = {
      arg_types   = [(Id.mk "l1", list_of Ty_int); (Id.mk "l2", list_of Ty_int)];
      ret_type    = list_of Ty_int
    };
    function_body = fun_append
  };
  { function_name = Id.mk "length";
    function_type = {
      arg_types   = [(Id.mk "l", list_of Ty_int)];
      ret_type    = Ty_int
    };
    function_body = fun_length
  };
  {
    function_name = Id.mk "reverse_aux";
    function_type = {
      arg_types   = [(Id.mk "l", list_of Ty_int); (Id.mk "acc", list_of Ty_int)];
      ret_type    = list_of Ty_int
    };
    function_body = fun_reverse_aux
  };
  {
    function_name = Id.mk "reverse";
    function_type = {
      arg_types = [(Id.mk "l", list_of Ty_int)];
      ret_type = list_of Ty_int
    };
    function_body = fun_reverse
  };
  {
    function_name = Id.mk "reverse_bis";
    function_type = {
      arg_types   = [(Id.mk "l", list_of Ty_int)];
      ret_type    = list_of Ty_int
    };
    function_body = fun_reverse_bis
  };
]

let program_name = "Lists"

let ir : program = {
    program_name = program_name;
    definitions  = List.map (fun x -> (Util.dummy_sail_def, FunctionDefinition x)) funDefList;
  }
