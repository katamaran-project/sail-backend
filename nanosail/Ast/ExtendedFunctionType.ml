open! ExtBase


type t = {
  extended_parameter_types : ExtendedType.Parameter.t list;
  extended_return_type     : ExtendedType.ReturnValue.t
}


let to_fexpr (extended_function_type : t) : FExpr.t =
  let parameters' =
    FExpr.mk_list @@ List.map ~f:ExtendedType.Parameter.to_fexpr extended_function_type.extended_parameter_types
  and return_type' =
    ExtendedType.ReturnValue.to_fexpr extended_function_type.extended_return_type
  in
  let keyword =
    [
      ("parameter_types", parameters');
      ("return_type", return_type')
    ]
  in
  FExpr.mk_application ~keyword "ExtFuncType"
