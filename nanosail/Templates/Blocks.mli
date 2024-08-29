val process_lines :
  (unit   -> string option) ->
  (string -> bool         ) ->
  (string -> bool         ) ->
  (string -> unit         ) ->
  (string -> unit         ) -> unit
