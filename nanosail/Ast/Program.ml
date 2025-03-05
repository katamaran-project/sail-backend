type t = {
  definitions          : (Sail.sail_definition * Definition.t) list;  (* list of all translated definitions; original order preserved *)
  polymorphic_argtypes : Type.t list list Identifier.Map.t;           (* maps polymorphic functions to the argument types *)
}


let empty =
  {
    definitions          = [];
    polymorphic_argtypes = Identifier.Map.empty;
  }
