module Make (M : Sig.Monad) = struct
  let rec repeat n ~f =
    if n <= 0
    then M.return []
    else
      M.bind f (fun r -> M.bind (repeat (n-1) ~f) (fun rs -> M.return @@ r::rs))

  
  let map ~f xs =
    let rec aux xs acc =
      match xs with
      | []    -> M.return @@ List.rev acc
      | x::xs -> M.bind (f x) (fun x' -> aux xs (x' :: acc))
    in
    aux xs []

  
  let flatmap ~(f : 'a -> 'b list M.t) (xs : 'a list) : 'b list M.t =
    let rec aux xs acc =
      match xs with
      | []    -> M.return @@ List.concat @@ List.rev acc
      | x::xs -> M.bind (f x) (fun x' -> aux xs (x' :: acc))
    in
    aux xs []

  
  let filter_map ~(f : 'a -> 'b option M.t) (xs : 'a list) : 'b list M.t =
    let rec aux xs acc =
      match xs with
      | []    -> M.return @@ List.rev acc
      | x::xs -> M.bind (f x) (fun x' -> aux xs @@ match x' with | Some r -> r::acc | None -> acc)
    in
    aux xs []


  let rec iter ~f xs =
    match xs with
    | []    -> M.return ()
    | x::xs -> M.bind (f x) (fun _ -> iter ~f xs)


  let lift ~f x =
    M.bind x (fun x -> M.return @@ f x)


  let rec fold_left ~f:f ~init:acc xs =
    match xs with
    | []    -> M.return acc
    | x::xs -> M.bind (f acc x) (fun acc' -> fold_left ~f ~init:acc' xs)


  let rec exists ~f xs =
    match xs with
    | x::xs -> begin
        M.bind
          (f x)
          (fun b ->
             if b
             then M.return true
             else exists ~f xs)
      end
    | [] -> M.return false


  let rec forall ~f xs =
    match xs with
    | x::xs -> begin
        M.bind
          (f x)
          (fun b ->
             if b
             then forall ~f xs
             else M.return false)
      end
    | [] -> M.return true


  let sequence xs =
    let rec aux acc xs =
      match xs with
      | []    -> M.return @@ List.rev acc
      | x::xs -> M.bind x @@ fun x' -> aux (x' :: acc) xs
    in
    aux [] xs


  let compose (g : 'b -> 'c M.t) (f : 'a -> 'b M.t) (x : 'a) =
    M.bind (f x) g


  let ignore f =
    M.bind f @@ fun _ -> M.return ()


  let lift_option (x : 'a M.t option) : 'a option M.t =
    match x with
    | None   -> M.return None
    | Some f -> M.bind f (fun x -> M.return @@ Some x)
end
