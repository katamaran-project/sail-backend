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
  let pp_integer (value : PP.document) =
    Coq.pp_application
      (PP.string "exp_int")
      [
        value
      ]


  (*
     exp_string <str>
  *)
  let pp_string (str : PP.document) =
    Coq.pp_application
      (PP.string "exp_string")
      [
        str
      ]


  (*
     exp_val ty.unit tt
  *)
  let pp_unit () =
    Coq.pp_application
      (PP.string "exp_val")
      [ PP.string "ty.unit"; PP.string "tt" ]
end
