type t = {
  (*
     List of all translated definitions.
     The definitions appear in the same order as their corresponding Sail-definitions were provided by Sail.
  *)
  definitions : (Sail.sail_definition * Definition.t) list;

  (*
     This map keeps track of the types of the arguments passed in calls to polymorphic functions.

     For example, take the polymorphic function

       val xor : forall 'n. bits('n) -> bits('n) -> bits('n)

     and it is called with the following arguments:

       xor(0b0, 0b0)
       xor(0b00, 0b00)

     then this map would contain

       xor -> [ [ Bitvector(1); Bitvector(1) ];
                [ Bitvector(2); Bitvector(2) ] ]
  *)
  polymorphic_argtypes : Type.t list list Identifier.Map.t;
}


let empty =
  {
    definitions          = [];
    polymorphic_argtypes = Identifier.Map.empty;
  }
