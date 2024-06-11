let pp_base_module () =
  let base_module_name = "UntitledBase"
  in
  PP.(separate hardline [
          string "Module Export " ^^ string base_module_name ^^ string " <: Base.";
          string "#[export] Instance typedeclkit : TypeDeclKit :=";
          string "  {| enumi   := Enums;";
          string "     unioni  := Unions;";
          string "     recordi := Records;";
          string "  |}.";
          string "End " ^^ string base_module_name ^^ dot;
  ])
