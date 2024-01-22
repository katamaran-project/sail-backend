open Base


type 'a t = (string, 'a, String.comparator_witness) Map.t

let empty = Map.empty(module String)

let add  = Map.add

let fold = Map.fold

let to_list = Map.to_alist
