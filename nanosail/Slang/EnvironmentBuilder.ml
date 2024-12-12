type extend_environment_context = {
  extend    : (Value.t Environment.t -> Value.t Environment.t) -> unit;
  callable  : string -> Value.callable -> unit;
}

let extend_environment env (f : extend_environment_context -> unit) =
  let current               = ref env
  in
  let update env            = current := env
  in
  let callable identifier f = update @@ Environment.bind !current identifier @@ Value.Callable f
  and extend   f            = update @@ f !current
  in
  let context = { extend; callable; }
  in
  f context;
  !current
