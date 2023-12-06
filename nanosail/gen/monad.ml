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

let add_annotations f =
  let (state, result) = run f empty_state
  in
  let annotations = MetadataMap.bindings state.metadata
  in
  let pp_annotations =
    let pp_annotation index doc =
      PPrint.(string (string_of_int index) ^^ string " : " ^^ align doc)
    in
    List.map (Auxlib.uncurry pp_annotation) annotations
  in
  PPrint.(separate hardline
            (Auxlib.build_list (fun { add; _ } ->
                 if not (List.is_empty annotations)
                 then add (Coq.comment (separate hardline pp_annotations));
                 add result)))
