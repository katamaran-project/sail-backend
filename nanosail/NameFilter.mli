(*
   A set of names given by patterns, as in the configuration's ignore lists:

     (functions "*_of_num" "num_of_*" log2 (except num_of_ExceptionType))

   A pattern matches a name exactly, except that each * in it matches any text (also none).
   The patterns of one list can have exceptions: names (or patterns) that the list does not
   select even though one of its patterns matches them. An exception applies only to the
   patterns given with it; a filter built from several lists selects a name if one of the
   lists does.
*)
type t

val equal : t -> t -> bool

(* Selects no name *)
val empty : t

(* Selects every name *)
val everything : t

(* The patterns of one list, and their exceptions *)
val of_patterns : patterns:string list -> exceptions:string list -> t

(* Selects the names that either filter selects *)
val union : t -> t -> t

val matches : t -> string -> bool

(* Whether the pattern (with * as wildcard) matches the name *)
val pattern_matches : pattern:string -> string -> bool
