val process_lines :
  next_line                 : (unit        -> string option) ->
  is_block_entry            : (string      -> bool         ) ->
  is_block_exit             : (string      -> bool         ) ->
  process_out_of_block_line : (string      -> unit         ) ->
  process_block             : (string list -> unit         ) -> unit
