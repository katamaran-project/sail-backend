open Base
open Ast
open Basics


module Context = struct
  type t =
    {
      definitions   : definition list;  (* list of definitions                          *)
      next_id_index : int;              (* counter used to generate unique identifiers  *)
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
    let get (context : t) : definition list = context.definitions
    and set (context : t) (definitions : definition list) : t = { context with definitions }
    in
    (get, set)
  
  let next_id_index =
    let get (context : t) : int = context.next_id_index
    and set (context : t) (next_id_index : int) : t = { context with next_id_index }
    in
    (get, set)
end


type error =
  | NotYetImplemented of ocaml_source_location * Libsail.Ast.l * string option
  | AssertionFailure  of ocaml_source_location * string


module Monad     = Monads.StateResult.Make (struct type t = Context.t end) (struct type t = error end)
module MonadUtil = Monads.Util.Make(Monad)

include MonadUtil
open Monads.Notations.Star(Monad)


type 'a t      = 'a Monad.t
type 'a result = 'a Monad.result = Success of 'a     (* explicitly enumerating cases here prevents result from becoming abstract *)
                                 | Failure of error

let return     = Monad.return
let bind       = Monad.bind
let recover    = Monad.recover
let run f      = Monad.run f Context.empty


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


(* let register_type (type_definition : type_definition) = *)
(*   let* types = Monad.get Context.types *)
(*   in *)
(*   let updated_types = *)
(*     let key = type_identifier type_definition *)
(*     and data = type_definition *)
(*     in *)
(*     match IdentifierMap.add types ~key ~data with *)
(*     | `Duplicate -> raise @@ TranslationError (Printf.sprintf "type %s defined multiple times" (Id.string_of key)) *)
(*     | `Ok result -> result *)
(*   in *)
(*   Monad.put Context.types updated_types *)


(* let register_register (register_definition : register_definition) = *)
(*   let* register_map = Monad.get Context.registers *)
(*   in *)
(*   let updated_register_map = *)
(*     let key = register_definition.identifier *)
(*     and data = register_definition.typ *)
(*     in *)
(*     match IdentifierMap.add register_map ~key ~data with *)
(*     | `Duplicate -> raise @@ TranslationError (Printf.sprintf "register %s defined multiple times" (Id.string_of key)) *)
(*     | `Ok result -> result *)
(*   in *)
(*   Monad.put Context.registers updated_register_map *)


let register (definition : definition) =
  let* old_definitions = Monad.get Context.definitions
  in
  let new_definitions = definition :: old_definitions
  in
  Monad.put Context.definitions new_definitions


(*
   Looks up a type definition based on the name of the type.

   The extractor (see Ast.Extract) can be used to get a specific kind of type
*)
let lookup_type
      (extractor  : type_definition -> 'a option)
      (identifier : identifier                  ) : 'a option t =
  let predicate (definition : definition) : type_definition option =
    match definition with
    | TypeDefinition type_definition ->
      if Id.equal identifier (type_identifier type_definition)
      then Some type_definition
      else None
    | _ -> None
  in
  let* definitions = Monad.get Context.definitions
  in
  let type_definition = List.find_map definitions ~f:predicate
  in
  return @@ Option.bind type_definition ~f:extractor



(* Looks up type of register with given name *)
let lookup_register_type (identifier : identifier) : nanotype option t =
  let predicate (definition : definition) : register_definition option =
    match definition with
    | RegisterDefinition register_definition ->
      if Id.equal register_definition.identifier identifier
      then Some register_definition
      else None
    | _ -> None
  in
  let* definitions = Monad.get Context.definitions
  in
  return @@ Option.map (List.find_map definitions ~f:predicate) ~f:(fun r -> r.typ)


let is_register (identifier : identifier) : bool t =
  MonadUtil.lift ~f:Option.is_some @@ lookup_register_type identifier


let generate_unique_identifier prefix : identifier t =
  let* index = Monad.get Context.next_id_index
  in
  let* () = Monad.put Context.next_id_index (index + 1)
  in
  let id = Printf.sprintf "%s%d" prefix index
  in
  return (Id.mk id)
