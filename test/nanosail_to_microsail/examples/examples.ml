open Nanosail.Ast

let all_ir =
  let function_definitions =
    Lists.funDefList @ 
      Prod.funDefList @
        Expr.funDefList @
          Long.funDefList
  and program_name = "All"
  in
  make_ir_t ~function_definitions:function_definitions program_name

let ir_assoc_list = [
    ("lists", Lists.ir);
    ("long", Long.ir);
    ("prod", Prod.ir);
    ("expr", Expr.ir);
    ("all", all_ir)
]

let find_ir name = List.assoc name ir_assoc_list
let find_ir_opt name = List.assoc_opt name ir_assoc_list

let ir_names = List.map fst ir_assoc_list
