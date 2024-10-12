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
     exp_val <typ> <value>
  *)
  let pp_value
      ~(typ   : PP.document)
      ~(value : PP.document)
    =
    Coq.pp_application
      (PP.string "exp_val")
      [
        typ;
        value;
      ]

  (*
     exp_val ty.unit tt
  *)
  let pp_unit () =
    let typ =
      PP.string "ty.unit"
    and value =
      PP.string "tt"
    in
    pp_value ~typ ~value
end
