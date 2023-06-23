open List
open PPrint
open IR

(******************************************************************************)
(* Utility definitions *)

let small_indent = nest 2
let big_indent = nest 4
let small_step = twice hardline
let big_step = twice small_step

(* Create a block surrounded by begin_str and end_str with nice alignment for 
   a list of document separated by set_str.
   with_spc set to true will add a breakable space after begin_str and before 
   end_str *)
let align_block with_spc begin_str end_str sep_str doc_list = 
  let spc = if with_spc then 1 else 0 in
    group (align (
      string begin_str ^^ blank spc ^^ align (
        separate (string sep_str ^^ break 1) doc_list
      ) ^^ break spc ^^ string end_str
    ))

(******************************************************************************)
(* Heading pretty printing *)

let require_import src names = nest 5 (
  group (
    string ("From " ^ src ^ " Require Import") ^^
    break 1 ^^
    separate_map hardline string names ^^
    dot
  )
)

let import names = nest 7 (
  group (
    string "Import " ^^
    separate_map hardline string names ^^
    dot
  )
)

let heading = separate small_step [
  require_import "Coq" ["Strings.String"];
  require_import "Katamaran" ["Semantics.Registers"; "Program"];
  import ["ctx.notations"; "ctx.resolution"];
  string "Local Open Scope string_scope."
]

(******************************************************************************)
(* Base pretty printing *)

let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* FunDeclKit pretty printing *)

let rec fromTy = function
  | Int    -> string "ty.int"
  | List t -> parens (string "ty.list " ^^ fromTy t)
  | Bool   -> string "ty.bool"


let fromBind (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ fromTy t

let funDecl funDef = group (big_indent (
  string ("| " ^ funDef.name ^ " :") ^^ break 1 ^^
  group (
    string "Fun " ^^ align_block true "[" "]" ";" 
        (map fromBind funDef.funType.arg_types)
    ^^ group (break 1 ^^ fromTy funDef.funType.ret_type)
  )
))

let funDeclKit funDefList =
  small_indent (
    separate hardline [
      string "Section FunDeclKit.";
      string "Inductive Fun : PCtx -> Ty -> Set :=";
      separate_map hardline funDecl funDefList ^^ dot;
      empty;
      utf8string "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
      utf8string "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
      utf8string "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set."
    ]
  ) ^^ hardline ^^ string "End FunDeclKit." 

(******************************************************************************)
(* FunDefKit pretty printing *)

let funBody funDef =
  (*let body = funDef.funBody in*)
  group (big_indent (
    string ("Definition fun_" ^ funDef.name ^ " :") ^^ break 1 ^^
    group (
      string "Stm " ^^ align_block true "[" "]" ";" 
          (map fromBind funDef.funType.arg_types)
      ^^ group (break 1 ^^ fromTy funDef.funType.ret_type)
    )
  )) ^^ small_indent (
    hardline ^^ string "$$$" ^^ dot
  )

let funDefKit funDefList =
  let name_binding funDef = group (big_indent (
    string ("| " ^ funDef.name ^ " =>") ^^ break 1 ^^
    string ("fun_" ^ funDef.name)
  )) in
  small_indent (separate small_step [
    string "Section FunDefKit.";
    separate_map small_step funBody funDefList;
    small_indent (separate  hardline [
      utf8string "Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=";
      utf8string "match f in Fun Δ τ return Stm Δ τ with";
      separate_map hardline name_binding funDefList;
      string "end."
    ])
  ]) ^^ small_step ^^ string "End FunDefKit." 
   

(******************************************************************************)
(* Program pretty printing *)

let program_module program_name base_name funDefList =
  small_indent (
    separate small_step [
      string ("Module Import " ^ program_name ^
              "Program <: Program " ^ base_name ^ "Base.");
      funDeclKit funDefList;
      string ("Include FunDeclMixin " ^ base_name ^ "Base.");
      funDefKit funDefList
    ]
  ) ^^ small_step ^^ string ("End " ^ program_name ^ "Program.")

(******************************************************************************)

let fromIR ir =
  let types =
    separate small_step [
      string "(*** TYPES ***)";
      defaultBase
    ]
  in let program = 
    separate small_step [
      string "(*** PROGRAM ***)";
      program_module ir.program_name "Default" ir.funDefList
    ]
  in separate big_step [
    heading;
    types;
    program
  ]

let pretty_print len out doc =
  ToChannel.pretty 1. len out (doc ^^ small_step)
