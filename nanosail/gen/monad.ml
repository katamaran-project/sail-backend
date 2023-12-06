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

let genid metadatum =
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

let add_annotations f =
  let (state, result) = run f empty_state
  in
  let annotations =
    List.map (fun (key, value) ->
        PPrint.(string (string_of_int key) ^^ string " : " ^^ value)
      ) (MetadataMap.bindings state.metadata)
  in
  PPrint.(Coq.comment (separate hardline annotations) ^^ hardline ^^ result)
