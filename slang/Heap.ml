open Base


type address = Address of int

(* Polymorphic so as to prevent cyclic dependencies *)
type 'a t = Heap of (Int.t, 'a, Int.comparator_witness) Map.t

let empty = Map.empty (module Int)

let read (Heap heap : 'a t) (Address address : address) : 'a =
  Map.find_exn heap address
    
let write (Heap heap : 'a t) (Address address : address) (value : 'a) : 'a t =
  Heap (Map.update heap address ~f:(Fn.const value))
