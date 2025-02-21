include Base.Sequence


let cons
    (x  : 'a  )
    (xs : 'a t) : 'a t
  =
  append (return x) xs


let rec permutations (xs : 'a list) : 'a list t =
  match xs with
  | [] -> return []
  | _  -> begin
      concat_map (range ~stop:`exclusive 0 @@ List.length xs) ~f:(fun index ->
          let x = List.nth_exn xs index
          in
          map
            ~f:(fun ys -> x :: ys)
            (permutations @@ List.drop_nth xs index)
        )
    end
