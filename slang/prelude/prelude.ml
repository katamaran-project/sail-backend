open Util
    

let prelude =
  extend_environment Environment.empty (fun { extend; _ } ->
      extend Arithmetic.library
    )
