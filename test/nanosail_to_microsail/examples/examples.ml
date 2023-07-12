open Nanosail.Ast

module M = Map.Make(String)

let all_ir = { 
  program_name = "All";
  funDefList = Lists.funDefList @
               Prod.funDefList @
               Expr.funDefList @
               Long.funDefList
}

let ir_map = M.of_seq (List.to_seq ([
    ("lists", Lists.ir);
    ("long", Long.ir);
    ("prod", Prod.ir);
    ("expr", Expr.ir);
    ("all", all_ir)
]))

let find_ir name = M.find name ir_map
let find_ir_opt name = M.find_opt name ir_map

let ir_names = List.map fst (M.bindings ir_map)