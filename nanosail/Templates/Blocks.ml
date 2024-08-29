open Base


type categorized_line =
  | BlockEntry
  | BlockExit
  | Line of string
  

(*

   next_line
     Returns the next line in the input
   is_block_entry
     Predicate that checks whether a given line represents the opening of a new block
   is_block_exit
     Predicate that checks whether a given line represents the closing a a block
   process_out_of_block_line
     Called with each line that appears outside a block
   process_block_line
     Called with each line that appears inside a block
   
 *)
let process_lines
    (next_line                 : unit   -> string option)
    (is_block_entry            : string -> bool         )
    (is_block_exit             : string -> bool         )
    (process_out_of_block_line : string -> unit         )
    (process_block_line        : string -> unit         )
  =
  let categorize_line (line : string) : categorized_line =
    if is_block_entry line
    then BlockEntry
    else if is_block_exit line
    then BlockExit
    else Line line
  in
  let rec process_lines
      (inside_block : bool)
      (line_index   : int )
    =
    (* called when end of input is reached; ensures that no blocks are left open *)        
    let end_of_input_reached () =
      if inside_block
      then failwith "Block was not closed correctly"
      else ()

    (* called when a new block is opened; checks that there are no nested blocks *)
    and block_entered () =
      if
        inside_block
      then
        let error_message =
          Printf.sprintf "No nested blocks allowed; see line %d" line_index
        in
        failwith error_message
      else
        process_lines true (line_index + 1)

    (* called when block is closed; checks that we were indeed inside a block *)
    and block_exited () =
      if
        inside_block
      then
        process_lines false (line_index + 1)
      else
        let error_message =
          Printf.sprintf "Block exit encountered on line %d while not in a block" line_index
        in
        failwith error_message

    (* called when a regular line (= not a block entry/exit line) is encountered *)
    and process_regular_line line =
      begin (* I'd rather be explicit about operator precedence *)
        if inside_block
        then process_block_line line
        else process_out_of_block_line line
      end;
      process_lines inside_block (line_index + 1)
    in
    
    let process_line line =
      match categorize_line line with
      | BlockEntry -> block_entered ()
      | BlockExit  -> block_exited ()
      | Line line  -> process_regular_line line
    in
    match next_line () with
    | None      -> end_of_input_reached ()
    | Some line -> process_line line
  in
  process_lines false 1
