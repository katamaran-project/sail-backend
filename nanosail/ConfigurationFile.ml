open ExtBase


module Location = struct
  type t = { file : string; line : int }

  let equal (location_1 : t) (location_2 : t) : bool =
    String.equal location_1.file location_2.file && Int.equal location_1.line location_2.line

  let to_string (location : t) : string =
    Printf.sprintf "%s:%d" location.file location.line
end


module Node = struct
  type t =
    | Atom of { text : string; quoted : bool; location : Location.t }
    | List of { elements : t list; location : Location.t }

  let rec equal (node_1 : t) (node_2 : t) : bool =
    match node_1, node_2 with
    | Atom atom_1, Atom atom_2 -> begin
        String.equal atom_1.text atom_2.text
        && Bool.equal atom_1.quoted atom_2.quoted
        && Location.equal atom_1.location atom_2.location
      end
    | List list_1, List list_2 -> begin
        List.equal equal list_1.elements list_2.elements
        && Location.equal list_1.location list_2.location
      end
    | Atom _, List _
    | List _, Atom _ -> false

  let location (node : t) : Location.t =
    match node with
    | Atom { location; _ } -> location
    | List { location; _ } -> location

  let rec to_string (node : t) : string =
    match node with
    | Atom { text; quoted = true; _ }  -> Printf.sprintf "%S" text
    | Atom { text; quoted = false; _ } -> text
    | List { elements; _ }             -> "(" ^ String.concat ~sep:" " (List.map elements ~f:to_string) ^ ")"
end


let error (location : Location.t) (message : string) : 'a =
  failwith @@ Printf.sprintf "%s: %s" (Location.to_string location) message


let parse_string
    ~(file    : string)
    (contents : string) : Node.t list
  =
  let location_of (range : Parsexp.Positions.range) : Location.t =
    { file; line = range.start_pos.line }
  in
  let rec of_cst (cst : Parsexp.Cst.t) : Node.t =
    match cst with
    | Atom { loc; atom; unescaped = _ } -> begin
        (* atom is the value; for a quoted atom, unescaped holds its source text, quotes included *)
        let quoted =
          loc.start_pos.offset < String.length contents && Char.equal contents.[loc.start_pos.offset] '"'
        in
        Atom { text = atom; quoted; location = location_of loc }
      end
    | List { loc; elements } -> List { elements = sexps_of elements; location = location_of loc }
  and sexps_of (elements : Parsexp.Cst.t_or_comment list) : Node.t list =
    List.filter_map elements ~f:(fun (element : Parsexp.Cst.t_or_comment) ->
        match element with
        | Sexp cst  -> Some (of_cst cst)
        | Comment _ -> None)
  in
  match Parsexp.Many_cst.parse_string contents with
  | Ok forms   -> sexps_of forms
  | Error message -> begin
      let position = Parsexp.Parse_error.position message
      in
      error { file; line = position.line } (Parsexp.Parse_error.message message)
    end


let read (path : string) : Node.t list =
  let rec read_file
      (including : string list)
      (path      : string     ) : Node.t list
    =
    if List.mem including path ~equal:String.equal
    then failwith @@ Printf.sprintf "%s includes itself (via %s)" path (String.concat ~sep:" -> " (List.rev (path :: including)))
    else begin
      let forms = parse_string ~file:path (Stdio.In_channel.read_all path)
      in
      let expand (form : Node.t) : Node.t list =
        match form with
        | List { elements = [ Atom { text = "include"; quoted = false; _ }; Atom { text = included; _ } ]; _ } -> begin
            let included_path =
              if Filename.is_relative included
              then Filename.concat (Filename.dirname path) included
              else included
            in
            read_file (path :: including) included_path
          end
        | List { elements = Atom { text = "include"; quoted = false; _ } :: _; location } -> begin
            error location "include takes one path: (include <path>)"
          end
        | Atom _ | List _ -> [ form ]
      in
      List.concat_map forms ~f:expand
    end
  in
  read_file [] path
