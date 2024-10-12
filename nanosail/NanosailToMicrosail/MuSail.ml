open Base


module Expression = struct
  (*
     exp_true
  *)
  let pp_true () =
    PP.annotate [%here] @@ PP.string "exp_true"


  (*
     exp_false
  *)
  let pp_false () =
    PP.annotate [%here] @@ PP.string "exp_false"


  (*
     exp_int <value>
  *)
  let pp_integer (value : Z.t) =
    Coq.pp_application
      (PP.string "exp_int")
      [ Coq.pp_integer value ]


  (*
     exp_string "<value>"
  *)
  let pp_string (str : string) =
    Coq.pp_application
      (PP.string "exp_string")
      [ PP.(surround dquotes @@ string str) ]


  (*
     exp_val ty.unit tt
  *)
  let pp_unit () =
      Coq.pp_application
      (PP.string "exp_val")
      [ PP.string "ty.unit"; PP.string "tt" ]
end
