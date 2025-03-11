(*
   Environments keep track of bindings:
   they map identifiers (strings) to values (Value.t).

   To prevent cyclical dependencies, the environment
   does not know about values and instead uses some 'a type.
   This is an acceptable solution since environments
   never need to interact with values and are happy
   to deal with them as opaque little boxes.

   If added environment functionality were to
   actually need to understand what values are,
   we would have to move this module into Recursive.
*)
open ExtBase

(* Polymorphic so as to prevent cyclic dependencies *)
type 'a t = (string, 'a, String.comparator_witness) Map.t


let empty = Map.empty (module String)


let bind
    (environment : 'a t  )
    (identifier  : string)
    (value       : 'a    ) : 'a t
  =
  Map.update environment identifier ~f:(Fn.const value)


let lookup
    (environment : 'a t  )
    (identifier  : string) : 'a option
  =
  Map.find environment identifier


let bind_many
    (environment : 'a t              )
    (pairs       : (string * 'a) list) : 'a t
  =
  List.fold_left ~f:(fun acc (id, v) -> bind acc id v) ~init:environment pairs
