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
   process_block
     Called with lines making up a block
   
 *)
let process_lines
    ~(next_line                 : unit        -> string option)
    ~(is_block_entry            : string      -> bool         )
    ~(is_block_exit             : string      -> bool         )
    ~(process_out_of_block_line : string      -> unit         )
    ~(process_block             : string list -> unit         ) : unit
  =
  let categorize_line (line : string) : categorized_line =
    if is_block_entry line
    then BlockEntry
    else if is_block_exit line
    then BlockExit
    else Line line
  in
  
  let categorize_next_line () : categorized_line option =
    Option.map (next_line ()) ~f:categorize_line
  in
  
  let rec process_line_outside_block (line_index : int) : unit =
    match categorize_next_line () with
    | Some BlockEntry  -> process_line_inside_block [] (line_index + 1)
    | Some BlockExit   -> failwith @@ Printf.sprintf "Exited block without being inside block (line %d)" line_index
    | Some (Line line) -> begin
        process_out_of_block_line line;
        process_line_outside_block (line_index + 1)
      end
    | None             -> ()

  and process_line_inside_block
      (acc        : string list)
      (line_index : int        ) : unit
    =
    match categorize_next_line () with
    | Some BlockEntry  -> failwith @@ Printf.sprintf "Cannot have nested blocks (line %d)" line_index
    | Some BlockExit   -> begin
        process_block @@ List.rev acc;
        process_line_outside_block (line_index + 1)
      end
    | Some (Line line) -> process_line_inside_block (line :: acc) (line_index + 1)
    | None             -> failwith "End of input while still inside block"
  in

  process_line_outside_block 1