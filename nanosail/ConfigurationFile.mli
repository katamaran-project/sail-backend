(*
   Reading configuration files: S-expressions as data (docs/usage.md#configuration-file).

   The forms of a file are read in order; a form (include <path>) is replaced by the forms of
   that file, with the path relative to the directory of the file that includes it.
   Every node remembers where it was read, for error messages.
*)

module Location : sig
  type t = { file : string; line : int }

  val equal     : t -> t -> bool
  val to_string : t -> string
end

module Node : sig
  type t =
    | Atom of { text : string; quoted : bool; location : Location.t }   (* text: the value, unquoted and unescaped *)
    | List of { elements : t list; location : Location.t }

  val equal     : t -> t -> bool
  val location  : t -> Location.t
  val to_string : t -> string
end

(* Raises Failure with the location of the error *)
val error : Location.t -> string -> 'a

(* The forms of a string of S-expressions; file is the name used in locations *)
val parse_string : file:string -> string -> Node.t list

(* The forms of the file, with includes expanded *)
val read : string -> Node.t list
