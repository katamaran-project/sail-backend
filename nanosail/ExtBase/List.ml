include Base.List


let find_index_of
    ~(f : 'a -> bool)
    (xs : 'a list   ) : int option
  =
  let rec aux index xs =
    match xs with
    | []    -> None
    | x::xs -> if f x then Some index else aux (index + 1) xs
  in
  aux 0 xs
