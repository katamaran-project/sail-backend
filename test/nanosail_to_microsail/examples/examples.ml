open Nanosail.Ast

let all_ir = { 
  program_name = "All";
  funDefList = Lists.funDefList @
               Prod.funDefList @
               Expr.funDefList @
               Long.funDefList
}

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