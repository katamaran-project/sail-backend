open Base


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
