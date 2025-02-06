open Base
open ExtBase


type t = TypeQuantifier of (Identifier.t * Kind.t) list


let to_fexpr (type_quantifier : t) : FExpr.t =
  let item_to_fexpr
        (id   : Identifier.t)
        (kind : Kind.t      ) : FExpr.t
    =
    FExpr.mk_list [ Identifier.to_fexpr id; Kind.to_fexpr kind ]
  in
  let TypeQuantifier items = type_quantifier
  in
  let positional =
    List.map ~f:(Fn.uncurry item_to_fexpr) items
  in
  FExpr.mk_application ~positional "TypeQuantifier"
