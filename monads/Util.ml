module Make (M : Sig.Monad) = struct
  let rec collect n f =
    if n <= 0
    then M.return []
    else
      M.bind f (fun r -> M.bind (collect (n-1) f) (fun rs -> M.return @@ r::rs))

  let rec repeat n f =
    if n <= 0
    then M.return ()
    else M.bind f (fun _ -> repeat (n - 1) f)

  let map f xs =
    let rec aux xs acc =
      match xs with
      | []    -> M.return @@ List.rev acc
      | x::xs -> M.bind (f x) (fun x' -> aux xs (x' :: acc))
    in
    aux xs []

  let rec iter f xs =
    match xs with
    | []    -> M.return ()
    | x::xs -> M.bind (f x) (fun _ -> iter f xs)

  let lift f x =
    M.bind x (fun x -> M.return @@ f x)

  let rec fold_left f acc xs =
    match xs with
    | []    -> M.return acc
    | x::xs -> M.bind (f acc x) (fun acc' -> fold_left f acc' xs)

  let rec exists predicate xs =
    match xs with
    | x::xs -> begin
        M.bind
          (predicate x)
          (fun b ->
             if b
             then M.return true
             else exists predicate xs)
      end
    | [] -> M.return false

  let rec forall predicate xs =
    match xs with
    | x::xs -> begin
        M.bind
          (predicate x)
          (fun b ->
             if b
             then forall predicate xs
             else M.return false)
      end
    | [] -> M.return true
end
