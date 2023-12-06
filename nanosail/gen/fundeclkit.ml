open PPrint
open Ast
open Util
open Monad


let generate function_definitions =
  let pp_function_declaration function_definition =
    let name = string function_definition.funName in
    let* function_type =
      let* parameter_types =
        let* ps = seqmap (List.map Sail.pp_bind function_definition.funType.arg_types)
        in
        generate (Coq.list ps)
      in
      let* return_type = Sail.pp_ty function_definition.funType.ret_type
      in
      generate (
          concat [
              string "Fun";
              space;
              align (
                  group (
                      concat [
                          parameter_types;
                          break 1;
                          return_type
                        ]
                    )
                )
            ]
        )
    in
    generate (name, function_type)
  in
  let inductive_type_declaration =
    let name = string "Fun"
    and typ = string "PCtx -> Ty -> Set"
    in
    Coq.mbuild_inductive_type name typ (fun add_constructor ->
        iter
          (fun function_definition ->
            let* name, function_type = pp_function_declaration function_definition
            in
            add_constructor ~typ:function_type name
          )
          function_definitions
      )
  in
  let contents =
    separate small_step [
        Coq.annotate inductive_type_declaration;
        separate_map hardline utf8string [
            "Definition 𝑭  : PCtx -> Ty -> Set := Fun.";
            "Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.";
            "Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.";
          ]
      ]
  in
  Coq.section "FunDeclKit" contents
