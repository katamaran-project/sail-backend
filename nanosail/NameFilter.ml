open ExtBase


module Group = struct
  type t =
    {
      patterns   : string list;
      exceptions : string list;
    }

  let equal (group_1 : t) (group_2 : t) : bool =
    List.equal String.equal group_1.patterns   group_2.patterns
    && List.equal String.equal group_1.exceptions group_2.exceptions
end


(* A name is selected if one of the groups selects it *)
type t = Group.t list


let equal (filter_1 : t) (filter_2 : t) : bool =
  List.equal Group.equal filter_1 filter_2


let empty : t = []


let everything : t = [ { patterns = [ "*" ]; exceptions = [] } ]


let of_patterns
    ~(patterns   : string list)
    ~(exceptions : string list) : t
  =
  [ { patterns; exceptions } ]


let union (filter_1 : t) (filter_2 : t) : t =
  filter_1 @ filter_2


(*
   The pattern is split at its *s into literal parts. The name must start with the first part
   and end with the last; the parts in between must occur in order, each as early as possible
   (the earliest occurrence leaves the most room for the parts after it).
*)
let pattern_matches
    ~(pattern : string)
    (name     : string) : bool
  =
  match String.split pattern ~on:'*' with
  | []            -> String.is_empty name
  | [ literal ]   -> String.equal literal name
  | first :: rest -> begin
      let last, middle =
        match List.rev rest with
        | last :: reversed_middle -> last, List.rev reversed_middle
        | []                      -> "", []
      in
      let fits_ends =
        String.is_prefix name ~prefix:first
        && String.is_suffix name ~suffix:last
        && String.length first + String.length last <= String.length name
      in
      let rec middle_parts_fit
          (parts    : string list)
          (position : int        ) : bool
        =
        match parts with
        | []            -> true
        | part :: parts -> begin
            let limit = String.length name - String.length last
            in
            match String.substr_index name ~pos:position ~pattern:part with
            | Some index when index + String.length part <= limit -> middle_parts_fit parts (index + String.length part)
            | Some _ | None                                       -> false
          end
      in
      fits_ends && middle_parts_fit middle (String.length first)
    end


let matches (filter : t) (name : string) : bool =
  let group_selects (group : Group.t) : bool =
    List.exists group.patterns ~f:(fun pattern -> pattern_matches ~pattern name)
    && not (List.exists group.exceptions ~f:(fun pattern -> pattern_matches ~pattern name))
  in
  List.exists filter ~f:group_selects
