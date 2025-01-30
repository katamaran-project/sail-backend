open Base
open OUnit2
open Nanosail


module Context = struct
  type t = {
    substitutions : Ast.Identifier.t Ast.Identifier.Map.t
  }

  let empty : t =
    {
      substitutions = Ast.Identifier.Map.empty
    }

  let substitutions =
    let get (context : t) : Ast.Identifier.t Ast.Identifier.Map.t =
      context.substitutions
    and set
        (_context      : t                                    )
        (substitutions : Ast.Identifier.t Ast.Identifier.Map.t) : t
      =
      { substitutions }
    in
    (get, set)
end

module Monad = Monads.State.Make(Context)
open Monads.Notations.Star(Monad)


let requires_substitution (identifier : Ast.Identifier.t) : bool =
  Ast.Identifier.is_generated identifier


(* let substitute_identifier (identifier : Ast.Identifier.t) : Ast.Identifier.t Monad.t = *)
(*   if not @@ requires_substitution identifier *)
(*   then Monad.return identifier *)
(*   else begin *)
(*     let* substitutions = Monad.get Context.substitutions *)
(*     in *)
(*     match Ast.Identifier.Map.find substitutions identifier with *)
(*     | Some identifier' -> Monad.return identifier' *)
(*     | None -> begin *)
(*         let index = *)
(*           Ast.Identifier.Map.length substitutions *)
(*         in *)
(*         let identifier' = *)
(*           Ast.Identifier.mk_generated @@ Int.to_string index *)
(*         in *)
(*         let substitutions' = *)
(*           Ast.Identifier.Map.add_exn substitution identifier identifier' *)
(*         in *)
(*         let* () = Monad.put Context.substitutions substitutions' *)
(*         in *)
(*         Monad.return identifier *)
(*       end *)

(*   end     *)


(* let normalize_statement (statement : Ast.Statement.t) : Ast.Statement.t Monad.t = *)
  
