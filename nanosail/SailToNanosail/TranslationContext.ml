open Base


module Context = struct
  type t =
    {
      definitions   : Ast.Definition.t list;  (* list of definitions                          *)
      next_id_index : int;                    (* counter used to generate unique identifiers  *)
    }

  let empty : t =
    {
      definitions = [];
      next_id_index = 0;
    }

  (*
     Accessors
  *)
  let definitions =
    let get (context : t) : Ast.Definition.t list = context.definitions
    and set (context : t) (definitions : Ast.Definition.t list) : t = { context with definitions }
    in
    (get, set)

  let next_id_index =
    let get (context : t) : int = context.next_id_index
    and set (context : t) (next_id_index : int) : t = { context with next_id_index }
    in
    (get, set)
end


module Error = struct
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | AssertionFailure  of Lexing.position * string
end


module Monad     = Monads.StateResult.Make(Context)(Error)
module MonadUtil = Monads.Util.Make(Monad)

include MonadUtil
open Monads.Notations.Star(Monad)


type 'a t      = 'a Monad.t
type 'a result = 'a Monad.result = Success of 'a     (* explicitly enumerating cases here prevents result from becoming abstract *)
                                 | Failure of Error.t

let return     = Monad.return
let error      = Monad.fail
let bind       = Monad.bind
let recover    = Monad.recover
let run f      = Monad.run f Context.empty


let string_of_error (error : Error.t) : string =
  let string_of_ocaml_location (ocaml_location : Lexing.position) =
    Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
  in
  match error with
  | NotYetImplemented (ocaml_position, sail_location, message) -> begin
      let message =
        match message with
        | Some message -> message
        | None         -> "<no message>"
      in
      Printf.sprintf "NotYetImplemented(%s, ocaml:%s, sail:%s)" message (StringOf.OCaml.position ocaml_position) (StringOf.Sail.location sail_location)
    end
  | AssertionFailure (ocaml_location, message) -> Printf.sprintf "AssertionFailure(%s at %s)" message (string_of_ocaml_location ocaml_location)


(*
  If an error occurs while executing f, print it out and 'reraise' it.
  In other words, this function has no impact on the execution,
  except that it might print out an error.
*)
let debug_error f =
  let show e =
    Stdio.printf "%s\n" (string_of_error e);
    error e
  in
  recover f show


let not_yet_implemented ?(message = "") ocaml_position sail_position =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let check ocaml_position condition message =
  if not condition
  then Monad.fail @@ AssertionFailure (ocaml_position, Lazy.force message)
  else Monad.return ()


let fail ocaml_position message =
  Monad.fail @@ AssertionFailure (ocaml_position, message)


let register (definition : Ast.Definition.t) =
  let* old_definitions = Monad.get Context.definitions
  in
  let new_definitions = definition :: old_definitions
  in
  Monad.put Context.definitions new_definitions


(*
   Looks up a type definition based on the name of the type.

   The extractor (see Ast.Extract) can be used to get a specific kind of type
*)
let lookup_type_of_kind
      (extractor  : Ast.Definition.Type.t -> 'a option)
      (identifier : Ast.Identifier.t                  ) : 'a option t
  =
  let predicate (definition : Ast.Definition.t) : Ast.Definition.Type.t option =
    match definition with
    | TypeDefinition type_definition ->
      if Ast.Identifier.equal identifier (Ast.Definition.Type.identifier type_definition)
      then Some type_definition
      else None
    | _ -> None
  in
  let* definitions = Monad.get Context.definitions
  in
  let type_definition = List.find_map definitions ~f:predicate
  in
  return @@ Option.bind type_definition ~f:extractor


let lookup_type =
  lookup_type_of_kind (Ast.Extract.of_anything)


(* Looks up type of register with given name *)
let lookup_register_type (identifier : Ast.Identifier.t) : Ast.Type.t option t =
  let predicate (definition : Ast.Definition.t) : Ast.Definition.register_definition option =
    match definition with
    | RegisterDefinition register_definition ->
      if Ast.Identifier.equal register_definition.identifier identifier
      then Some register_definition
      else None
    | _ -> None
  in
  let* definitions = Monad.get Context.definitions
  in
  return @@ Option.map (List.find_map definitions ~f:predicate) ~f:(fun r -> r.typ)


let is_register (identifier : Ast.Identifier.t) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


let generate_unique_int : int t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  return index


let generate_unique_identifier prefix : Ast.Identifier.t t =
  let* index = generate_unique_int
  in
  let id = Printf.sprintf "%s%d" prefix index
  in
  return (Ast.Identifier.mk id)
