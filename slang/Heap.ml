open Base


(* Polymorphic so as to prevent cyclic dependencies *)
type 'a t = Heap of (Int.t, 'a, Int.comparator_witness) Map.t * int

let empty : 'a t = Heap (Map.empty (module Int), 0)

let read (Heap (heap, _) : 'a t) (address : Address.t) : 'a =
  Map.find_exn heap @@ Address.to_int address
    
let write (Heap (heap, n) : 'a t) (address : Address.t) (value : 'a) : 'a t =
  Heap (Map.update heap (Address.to_int address) ~f:(Fn.const value), n)
