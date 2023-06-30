open Lib.IR
open ListsIR
open LongIR
open ProdIR
open ExprIR


(******************************************************************************)
(* Intermediate Representation Lists *)


let ir = { 
  program_name = "All";
  funDefList = listsFunDefList @
               prodFunDefList @
               exprFunDefList @
               longFunDefList
}
