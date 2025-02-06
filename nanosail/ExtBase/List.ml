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


type 'a list_builder = {
    add    : 'a        -> unit;
    addall : 'a list   -> unit;
    addopt : 'a option -> unit;
  }

let build_list f =
  let list_under_construction = ref []
  in
  let add item = list_under_construction := item :: !list_under_construction
  in
  let addall items = iter ~f:add items
  and addopt item =
    match item with
    | Some item -> add item
    | None      -> ()
  in
  let context = { add; addall; addopt }
  in
  f context;
  rev !list_under_construction


let rec unordered_pairs xs =
  match xs with
  | []    -> []
  | x::xs -> (map ~f:(fun y -> (x, y)) xs) @ (unordered_pairs xs)


let split_last xs =
  match rev xs with
  | []    -> None
  | x::xs -> Some (rev xs, x)


(* performs a left fold using first element as init *)
let reduce ~(f: 'a -> 'a -> 'a) (list : 'a list) : 'a =
  match list with
  | []    -> failwith "cannot reduce empty list"
  | x::xs -> fold_left xs ~init:x ~f


let rec consecutive_overlapping_pairs (xs : 'a list) : ('a * 'a) list =
  match xs with
  | x::y::xs -> (x, y) :: consecutive_overlapping_pairs (y :: xs)
  | _        -> []


let rec repeat
    (n : int)
    (x : 'a ) : 'a list
  =
  if n > 0
  then x :: repeat (n-1) x
  else []


let zip_indices (xs : 'a list) : (int * 'a) list =
  let rec aux
      (acc   : (int * 'a) list)
      (xs    : 'a list        )
      (index : int            ) : (int * 'a) list
    =
    match xs with
    | []    -> rev acc
    | x::xs -> aux ((index, x) :: acc) xs (index + 1)
  in
  aux [] xs 0


let indices (xs : 'a list) : int list =
  range ~stop:`exclusive 0 (length xs)


let rec drop_nth
    (xs    : 'a list)
    (index : int    ) : 'a list
  =
  match xs with
  | []    -> failwith "invalid index"
  | x::xs -> begin
      if
        Int.equal index 0
      then
        xs
      else
        x :: drop_nth xs (index - 1)
    end        


let rec permutations (xs : 'a list) : 'a list list =
  match xs with
  | [] -> [[]]
  | _  -> begin
      concat_map (indices xs) ~f:(fun index ->
          let x = nth_exn xs index
          in
          map
            ~f:(fun ys -> x :: ys)
            (permutations @@ drop_nth xs index)
        )
    end


let rec permutation_functions (size : int) : ('a list -> 'a list) list =
  let permutations : int list list =
    permutations (List.range ~stop:`exclusive 0 size)
  in
  let create_permutation_function (permutation : int list) : 'a list -> 'a list =
    fun (xs : 'a list) -> begin
        if
          not @@ Int.equal size @@ List.length xs
        then
          failwith "permutation function accepts only lists of a specific length"
        else
          List.map ~f:(fun index -> nth_exn xs index) permutation
      end
  in
  List.map ~f:create_permutation_function permutations
