open Nanosail.Ast

module Id = Nanosail.Id


let var s = Exp_var (Id.mk s)

(******************************************************************************)
(* Functions bodies *)

let fun_ex_prod = Stm_exp (Exp_val (Val_prod (Val_prod (
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one"),
  Val_prod (Val_int (Big_int.of_int 2), Val_string "two")),
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one")
)))

let fun_switch = Stm_match begin
    MP_product {
      matched = Stm_exp (var "p");
      id_fst  = Id.mk "l";
      id_snd  = Id.mk "r";
      body    = Stm_exp (Exp_binop (Pair, var "r", var "l"));
    }
  end


(******************************************************************************)
(* Intermediate Representation Lists *)

let product_of ts =
  Ty_tuple ts


let funDefList = [
  {
    function_name = Id.mk "ex_prod";
    function_type = {
        arg_types = [ (Id.mk "tt", Ty_unit); (Id.mk "tt", Ty_unit) ];
        ret_type  = product_of
            [
              product_of [
                product_of [Ty_int; Ty_string];
                product_of [Ty_int; Ty_string]
              ];
              product_of [Ty_int; Ty_string]
            ]
      };
    function_body = fun_ex_prod;
  };
  {
    function_name = Id.mk "switch";
    function_type = {
      arg_types   = [ (Id.mk "p", product_of [Ty_int; Ty_bool]) ];
      ret_type    =  product_of [Ty_bool; Ty_int];
    };
    function_body = fun_switch;
  };
]

let program_name = "Prod"

let ir : program = {
    program_name = program_name;
    definitions  = List.map (fun x -> (Util.dummy_sail_def, FunctionDefinition x)) funDefList;
  }

