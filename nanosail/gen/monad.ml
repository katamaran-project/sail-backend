module MetadataMap = Map.Make(Int)

type 'a gen_state =
  {
    next_id : int;
    metadata : 'a MetadataMap.t
  }

type ('a, 'r) gen_monad =
  | GenMonad of ('a gen_state -> ('a gen_state * 'r))

let empty_state = { next_id = 1; metadata = MetadataMap.empty }

let run (GenMonad f) state = f state

let create_annotation metadatum =
  GenMonad (fun { next_id; metadata } ->
      let metadata' = MetadataMap.add next_id metadatum metadata
      and next_id' = next_id + 1
      in
      (
        { next_id=next_id'; metadata=metadata' },
        next_id
      )
    )

let generate result =
  GenMonad (fun state -> (state, result))

let (let*) m g =
       GenMonad (fun state ->
           let (state', result) = run m state
           in
           run (g result) state')

let not_yet_implemented (filename, line_number, _start_column, _end_column) =
  let annotation_doc =
    PPrint.string (Printf.sprintf "%s line %d" filename line_number)
  in
  let* id = create_annotation annotation_doc
  in
  generate (PPrint.string (Printf.sprintf "NYI[%d]" id))

let rec seqmap fs =
  match fs with
  | []    -> generate []
  | f::fs -> let* r = f in
             let* rs = seqmap fs
             in
             generate (r :: rs)

let rec iter f xs =
  match xs with
  | []    -> generate ()
  | x::xs -> let* _ = f x in
             iter f xs

