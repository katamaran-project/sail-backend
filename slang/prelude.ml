open Base


exception TypeError
  

let as_int (value : Value.t) =
  match value with
  | Value.Integer n -> n
  | _               -> raise TypeError


let addition args =
  let ns = List.map ~f:as_int args
  in
  let result = List.fold_left ~f:Int.(+) ~init:0 ns
  in
  Value.Integer result


let subtraction args =
  let ns = List.map ~f:as_int args
  in
  let result = match ns with
    | []    -> 0
    | [n]   -> -n
    | n::ns -> List.fold_left ~f:Int.(-) ~init:n ns
  in
  Value.Integer result


let multiplication args =
  let ns = List.map ~f:as_int args
  in
  let result = List.fold_left ~f:Int.( * ) ~init:1 ns
  in
  Value.Integer result


let division args =
  let ns = List.map ~f:as_int args
  in
  let result = match ns with
    | []    -> raise TypeError
    | [_]   -> raise TypeError
    | n::ns -> List.fold_left ~f:Int.(/) ~init:n ns
  in
  Value.Integer result


type extend_environment_context = {
  extend          : (Value.t Environment.t -> Value.t Environment.t) -> unit;
  native_function : string -> Value.native_function -> unit;
}

let extend_environment env (f : extend_environment_context -> unit) =
  let current = ref env
  in
  let update env =
    current := env
  in
  let native_function identifier f =
    update @@ Environment.bind !current identifier @@ Value.NativeFunction f
  and extend f =
    update @@ f !current
  in
  let context = {
    extend          = extend;
    native_function = native_function;
  }
  in
  f context;
  !current


let arithmetic env =
  extend_environment env (fun { native_function; _ } ->
      native_function "+" addition;
      native_function "-" subtraction;
      native_function "*" multiplication;
      native_function "/" division;
    )


let prelude =
  extend_environment Environment.empty (fun { extend; _ } ->
      extend arithmetic
    )
