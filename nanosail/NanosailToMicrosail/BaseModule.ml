let pp_typedeclkit () =
  PP.(separate hardline [
          string "#[export] Instance typedeclkit : TypeDeclKit :=";
          string "  {| enumi   := Enums;";
          string "     unioni  := Unions;";
          string "     recordi := Records;";
          string "  |}.";
        ]
  )


let pp_base_module () =
  let base_module_name = "UntitledBase"
  and flag = Coq.Export
  and includes = [ "Base" ]
  and contents = PP.(separate hardline [ pp_typedeclkit () ])
  in
  Coq.module' ~flag ~includes base_module_name contents
