val process_lines :
  (unit        -> string option) ->
  (string      -> bool         ) ->
  (string      -> bool         ) ->
  (string      -> unit         ) ->
  (string list -> unit         ) -> unit
