open ExtBase


(* Polymorphic so as to prevent cyclic dependencies *)
type 'a t = Heap of (Int.t, 'a, Int.comparator_witness) Map.t * int

let empty : 'a t = Heap (Map.empty (module Int), 0)


let read (Heap (map, _) : 'a t) (address : Address.t) : 'a option =
  Map.find map @@ Address.to_int address


let write (Heap (map, n) : 'a t) (address : Address.t) (value : 'a) : 'a t =
  let updated_map =
    Map.update map (Address.to_int address) ~f:(Fn.const value)
  in
  Heap (updated_map, n)


let allocate (Heap (map, n) : 'a t) (value : 'a) =
  let updated_map =
    match Map.add map ~key:n ~data:value with
    | `Duplicate -> failwith "Should never occur; bug in heap code"
    | `Ok map -> map
  in
  let updated_n =
    n + 1
  in
  (Heap (updated_map, updated_n), Address.mk n)
