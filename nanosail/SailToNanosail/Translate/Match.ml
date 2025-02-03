open! Base

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC = TranslationContext

open Monads.Notations.Star(TC)


module Binder = struct
  type t = { identifier : Ast.Identifier.t; wildcard : bool }

  let equal
      (binder_1 : t)
      (binder_2 : t) : bool
    =
    Ast.Identifier.equal
      binder_1.identifier
      binder_2.identifier
    &&
    Bool.equal
      binder_1.wildcard
      binder_2.wildcard


  let to_fexpr (binder : t) : FExpr.t =
    let keyword =
      [
        (
          "identifier",
          Ast.Identifier.to_fexpr binder.identifier
        );
        (
          "wildcard",
          FExpr.mk_bool binder.wildcard
        );
      ]
    in
    FExpr.mk_application ~keyword "Binder"


  let unify
      (binder_1 : t)
      (binder_2 : t) : t TC.t
    =
    match binder_1.wildcard, binder_2.wildcard with
    | true, true -> begin
        if
          Ast.Identifier.is_generated binder_1.identifier
        then
          TC.return binder_2
        else
          TC.return binder_1
      end
    | true , false -> TC.return binder_2
    | false, true  -> TC.return binder_1
    | false, false -> begin
        if
          Ast.Identifier.equal binder_1.identifier binder_2.identifier
        then
          TC.return binder_1
        else
          TC.fail [%here] "cannot unify binders"
      end


  let generate_binder : t TC.t =
    let* identifier = TC.generate_unique_identifier ()
    in
    TC.return { identifier; wildcard = false }


  let generate_wildcard : t TC.t =
    let* identifier = TC.generate_unique_identifier ()
    in
    TC.return { identifier; wildcard = true }
end


module Pattern = struct
  type t =
    | ListCons    of t * t
    | ListNil
    | Tuple       of t list
    | EnumCase    of Ast.Identifier.t
    | VariantCase of Ast.Identifier.t * t
    | BoolCase    of bool
    | Binder      of Binder.t
    | Unit


  let rec to_fexpr (pattern : t) : FExpr.t =
    let head id =
      Printf.sprintf "Pattern:%s" id
    in
    match pattern with
    | ListCons (head_pattern, tail_pattern) -> begin
        let keyword =
          [
            ("head", to_fexpr head_pattern);
            ("tail", to_fexpr tail_pattern);
          ]
        in
        FExpr.mk_application ~keyword @@ head "Cons"
      end

    | ListNil -> FExpr.mk_symbol @@ head "Nil"

    | Tuple subpatterns -> begin
        let positional =
          List.map ~f:to_fexpr subpatterns
        in
        FExpr.mk_application ~positional @@ head "Tuple"
      end

    | EnumCase identifier -> begin
        let positional =
          [
            Ast.Identifier.to_fexpr identifier
          ]
        in
        FExpr.mk_application ~positional @@ head "EnumCase"
      end

    | VariantCase (identifier, subpattern) -> begin
        let positional = [
          Ast.Identifier.to_fexpr identifier;
          to_fexpr subpattern
        ]
        in
        FExpr.mk_application ~positional @@ head "VariantCase"
      end

    | BoolCase value -> begin
        let positional = [
          FExpr.mk_bool value
        ]
        in
        FExpr.mk_application ~positional @@ head "BoolCase"
      end

    | Binder { identifier; wildcard } -> begin
        let positional =
          [
            Ast.Identifier.to_fexpr identifier;
          ]
        and keyword =
          [
            ("wildcard", FExpr.mk_bool wildcard)
          ]
        in
        FExpr.mk_application ~positional ~keyword @@ head "Variable"
      end

    | Unit -> FExpr.mk_symbol @@ head "Unit"


  let is_binder (pattern : t) : bool =
    match pattern with
     | Binder _ -> true
     | _        -> false


  let identifier_of_binder (pattern : t) : Ast.Identifier.t =
    match pattern with
    | Binder { identifier; _ } -> identifier
    | _                        -> failwith "bug: should only be called on Binder patterns"
end


module PatternTree = struct
  type t =
    | Enum       of { enum_identifier    : Ast.Identifier.t; table : (Binder.t * t) Ast.Identifier.Map.t;    }
    | Variant    of { variant_identifier : Ast.Identifier.t; table : (Binder.t * variant_binders * t) Ast.Identifier.Map.t }
    | Atomic     of Ast.Type.t * Binder.t * t
    | Bool       of bool_binders
    | Terminal   of Ast.Statement.t option

  (*
     From Sail's standpoint, all constructors are unary:

      * nullary constructors take a single unit value
      * unary constructors take the one value as field
      * n-ary constructors take a single tuple containing all fields
  *)
  and variant_binders =
    | NullaryConstructor of Binder.t         (* binder necessary for unit value *)
    | UnaryConstructor   of Binder.t
    | NAryConstructor    of Binder.t list    (* one binder per field *)

  and bool_binders =
    | SingleBoolCase    of Binder.t * t
    | SeparateBoolCases of { when_true : t; when_false : t }

  let rec equal
      (node_1 : t)
      (node_2 : t) : bool
    =
    match node_1 with
    | Enum { enum_identifier = enum_identifier_1; table = table_1 } -> begin
        match node_2 with
        | Enum { enum_identifier = enum_identifier_2; table = table_2 } -> begin
            let table_entry_equality
                (binder_1, subtree_1)
                (binder_2, subtree_2)
              =
              Binder.equal
                binder_1
                binder_2
              &&
              equal
                subtree_1
                subtree_2
            in
            Ast.Identifier.equal
              enum_identifier_1
              enum_identifier_2
            &&
            Ast.Identifier.Map.equal table_entry_equality
              table_1
              table_2
          end
        | _ -> false
      end

    | Bool binders_1 -> begin
        match node_2 with
        | Bool binders_2 -> begin
            match binders_1 with
            | SingleBoolCase (binder_1, subtree_1) -> begin
                match binders_2 with
                | SingleBoolCase (binder_2, subtree_2) -> begin
                    Binder.equal
                      binder_1
                      binder_2
                    &&
                    equal
                      subtree_1
                      subtree_2
                  end
                | _ -> false
              end
            | SeparateBoolCases { when_true = when_true_1; when_false = when_false_1 } -> begin
                match binders_2 with
                | SeparateBoolCases { when_true = when_true_2; when_false = when_false_2 } -> begin
                    equal
                      when_true_1
                      when_true_2
                    &&
                    equal
                      when_false_1
                      when_false_2
                  end
                | _ -> false
              end
          end
        | _ -> false
      end

    | Variant { variant_identifier = variant_identifier_1; table = table_1 } -> begin
        match node_2 with
        | Variant { variant_identifier = variant_identifier_2; table = table_2 } -> begin
            let variant_table_data_equality
                ((binder_1, field_binders_1, subtree_1) : Binder.t * variant_binders * t)
                ((binder_2, field_binders_2, subtree_2) : Binder.t * variant_binders * t) : bool
              =
              Binder.equal
                binder_1
                binder_2
              &&
              equal
                subtree_1
                subtree_2
              &&
              begin
                match field_binders_1, field_binders_2 with
                | NullaryConstructor field_binder_1, NullaryConstructor field_binder_2 -> begin
                    Binder.equal
                      field_binder_1
                      field_binder_2
                  end
                | NullaryConstructor _ , _ -> false

                | UnaryConstructor field_binder_1, UnaryConstructor field_binder_2 -> begin
                    Binder.equal
                      field_binder_1
                      field_binder_2
                  end
                | UnaryConstructor _, _ -> false

                | NAryConstructor field_binders_1, NAryConstructor field_binders_2 -> begin
                    List.equal Binder.equal
                      field_binders_1
                      field_binders_2
                    &&
                    equal
                      subtree_1
                      subtree_2
                  end
                | NAryConstructor _, _ -> false
              end
            in
            Ast.Identifier.equal
              variant_identifier_1
              variant_identifier_2
            &&
            Ast.Identifier.Map.equal variant_table_data_equality
              table_1
              table_2
          end
        | _ -> false
      end

    | Terminal statement_1 -> begin
        match node_2 with
        | Terminal statement_2 -> begin
            Option.equal Ast.Statement.equal statement_1 statement_2
          end
        | _ -> false
      end

    | Atomic (type_1, binder_1, subtree_1) -> begin
        match node_2 with
        | Atomic (type_2, binder_2, subtree_2) -> begin
            Ast.Type.equal
              type_1
              type_2
            &&
            Binder.equal
              binder_1
              binder_2
            &&
            equal
              subtree_1
              subtree_2
          end
        | _ -> false
      end


  let rec to_fexpr (node : t) : FExpr.t =
    let mk_head (tag : string) : string =
      Printf.sprintf "PatternTree:%s" tag
    in
    match node with
    | Enum { enum_identifier; table } -> begin
        let keyword =
          [
            (
              "enum_identifier",
              Ast.Identifier.to_fexpr enum_identifier
            );
            (
              "table",
              let fexpr_of_table_entry (binder, subtree) =
                FExpr.mk_list [
                  Binder.to_fexpr binder;
                  to_fexpr subtree;
                ]
              in
              Ast.Identifier.Map.to_fexpr fexpr_of_table_entry table
            );
          ]
        in
        FExpr.mk_application ~keyword @@ mk_head "Enum"
      end

    | Bool binders -> begin
        let positional = [
          match binders with
          | SingleBoolCase (binder, subtree) -> begin
              let keyword =
                [
                  ("binder", Binder.to_fexpr binder);
                  ("subtree", to_fexpr subtree);
                ]
              in
              FExpr.mk_application ~keyword "Single"
            end
          | SeparateBoolCases { when_true; when_false } -> begin
              let keyword =
                [
                  ("true", to_fexpr when_true);
                  ("false", to_fexpr when_false);
                ]
              in
              FExpr.mk_application ~keyword "Separate"
            end
        ]
        in
        FExpr.mk_application ~positional @@ mk_head "Bool"
      end

    | Variant { variant_identifier; table } -> begin
        let keyword =
          [
            (
              "variant_identifier",
              Ast.Identifier.to_fexpr variant_identifier
            );
            (
              "table",
              let fexpr_of_table_entry ((binder, field_binders, subtree) : Binder.t * variant_binders * t) : FExpr.t =
                let fexpr_of_data =
                  match field_binders with
                  | NullaryConstructor field_binder -> begin
                      let positional =
                        [
                          Binder.to_fexpr field_binder
                        ]
                      in
                      FExpr.mk_application ~positional "NullaryConstructor"
                    end
                  | UnaryConstructor field_binder -> begin
                      let positional =
                        [
                          Binder.to_fexpr field_binder
                        ]
                      in
                      FExpr.mk_application ~positional "UnaryConstructor"
                    end
                  | NAryConstructor field_binders -> begin
                      let positional =
                        [
                          FExpr.mk_list @@ List.map ~f:Binder.to_fexpr field_binders
                        ]
                      in
                      FExpr.mk_application ~positional "NAryConstructor"
                    end
                in
                FExpr.mk_list [ Binder.to_fexpr binder; fexpr_of_data; to_fexpr subtree ]
              in
              Ast.Identifier.Map.to_fexpr fexpr_of_table_entry table
            )
          ]
        in
        FExpr.mk_application ~keyword @@ mk_head "Variant"
      end

    | Atomic (typ, binder, subtree) -> begin
        let keyword =
          [
            (
              "type",
              Ast.Type.to_fexpr typ
            );
            (
              "binder",
              Binder.to_fexpr binder
            );
            (
              "subtree",
              to_fexpr subtree
            )
          ]
        in
        FExpr.mk_application ~keyword @@ mk_head "Atomic"
      end

    | Terminal statement -> begin
        let keyword =
          [
            (
              "statement",
              FExpr.mk_option @@ Option.map statement ~f:Ast.Statement.to_fexpr
            );
          ]
        in
        FExpr.mk_application ~keyword @@ mk_head "Terminal"
      end

  
  let rec count_nodes (tree : t) : int =
    match tree with
    | Enum { enum_identifier = _; table } -> begin
        1 + List.sum
          (module Int)
          (Ast.Identifier.Map.data table)
          ~f:(fun (_, subtree) -> count_nodes subtree)
      end
      
    | Variant { variant_identifier = _; table } -> begin
        1 + List.sum
          (module Int)
          (Ast.Identifier.Map.data table)
          ~f:(fun (_, _, subtree) -> count_nodes subtree)        
      end
      
    | Atomic (_, _, subtree)                             -> 1 + count_nodes subtree
    | Bool (SingleBoolCase (_, subtree))                 -> 1 + count_nodes subtree
    | Bool (SeparateBoolCases { when_true; when_false }) -> 1 + count_nodes when_true + count_nodes when_false
    | Terminal _                                         -> 1
end


let rec build_empty_pattern_tree
    (location      : S.l            )
    (element_types : Ast.Type.t list) : PatternTree.t TC.t
  =
  let build_bool_node (subtree : PatternTree.t) : PatternTree.t TC.t =
    let* binder =
      Binder.generate_wildcard
    in
    TC.return begin
      PatternTree.Bool (PatternTree.SingleBoolCase (binder, subtree))
    end

  and build_enum_node
      (enum_identifier : Ast.Identifier.t)
      (subtree         : PatternTree.t   ) : PatternTree.t TC.t
    =
    let* enum_definition =
      TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
    in
    let* table : (Binder.t * PatternTree.t) Ast.Identifier.Map.t =
      let add_to_table
          (table                : (Binder.t * PatternTree.t) Ast.Identifier.Map.t)
          (enum_case_identifier : Ast.Identifier.t                               ) : (Binder.t * PatternTree.t) Ast.Identifier.Map.t TC.t
        =
        let* binder : Binder.t =
          Binder.generate_wildcard
        in
        TC.return begin
          Ast.Identifier.Map.add_exn
            table
            ~key:enum_case_identifier
            ~data:(binder, subtree)
        end
      in
      TC.fold_left
        enum_definition.cases
        ~init:Ast.Identifier.Map.empty
        ~f:add_to_table
    in
    TC.return begin
      PatternTree.Enum {
        enum_identifier;
        table;
      }
    end

  and build_variant_node
      (variant_identifier : Ast.Identifier.t)
      (subtree            : PatternTree.t   ) : PatternTree.t TC.t
    =
    let* variant_definition =
      TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
    in
    let* table : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t =
      let add_to_table
          (table                                 : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t)
          ((constructor_identifier, field_types) : Ast.Identifier.t * Ast.Type.t list                                           ) : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t TC.t
        =
        let* data : Binder.t * PatternTree.variant_binders * PatternTree.t =
          match List.length field_types with
          | 0 -> begin
              let* binder       = Binder.generate_wildcard
              and* field_binder = Binder.generate_wildcard
              in
              TC.return (binder, PatternTree.NullaryConstructor field_binder, subtree)
            end
          | 1 -> begin
              let* binder       = Binder.generate_wildcard
              and* field_binder = Binder.generate_wildcard
              in
              TC.return (binder, PatternTree.UnaryConstructor field_binder, subtree)
            end
          | _ -> begin
              let field_count = List.length field_types
              in
              let* binder        = Binder.generate_wildcard
              and* field_binders = TC.repeat field_count ~f:Binder.generate_wildcard
              in
              TC.return (binder, PatternTree.NAryConstructor field_binders, subtree)
            end
        in
        TC.return begin
          Ast.Identifier.Map.add_exn table ~key:constructor_identifier ~data
        end
      in
      TC.fold_left
        variant_definition.constructors
        ~init:Ast.Identifier.Map.empty
        ~f:add_to_table
    in
    TC.return begin
      PatternTree.Variant {
        variant_identifier;
        table
      }
    end

  and build_atomic_node
      (element_type : Ast.Type.t   )
      (subtree      : PatternTree.t) : PatternTree.t TC.t
    =
    let* binder =
      Binder.generate_wildcard
    in
    TC.return @@ PatternTree.Atomic (element_type, binder, subtree)

  in
  match element_types with
  | []           -> TC.return @@ PatternTree.Terminal None
  | head :: tail -> begin
      let* tail = build_empty_pattern_tree location tail
      in
      let* tree =
        match head with
        | Enum enum_identifier       -> build_enum_node enum_identifier tail
        | Int                        -> build_atomic_node Ast.Type.Int tail
        | Variant variant_identifier -> build_variant_node variant_identifier tail
        | Unit                       -> build_atomic_node Ast.Type.Unit tail
        | Bool                       -> build_bool_node tail
        | String                     -> TC.not_yet_implemented [%here] location
        | Bit                        -> TC.not_yet_implemented [%here] location
        | List _                     -> TC.not_yet_implemented [%here] location
        | Sum (_, _)                 -> TC.not_yet_implemented [%here] location
        | Bitvector _                -> TC.not_yet_implemented [%here] location
        | Tuple _                    -> TC.not_yet_implemented [%here] location
        | Record _                   -> TC.not_yet_implemented [%here] location
        | Application (_, _)         -> TC.not_yet_implemented [%here] location
        | Alias (_, _)               -> TC.not_yet_implemented [%here] location
        | Range (_, _)               -> TC.not_yet_implemented [%here] location
      in
      let* () =
        let message = lazy begin
          let node_count = PatternTree.count_nodes tree
          in
          Printf.sprintf "Constructed pattern tree with %d nodes" node_count
        end
        in        
        TC.log [%here] Logging.warning message
      in
      TC.return tree
    end


(*
   Checks if there are "leaves" in the tree that have no associated statement yet,
   i.e., answers the question "are there any unhandled cases left in the given tree?"

   todo: check if this function is still useful, it was necessary when pattern trees were represented differently,
         but the new design might make checking for gaps redundant
*)
let rec contains_gap (pattern_tree : PatternTree.t) : bool =
  match pattern_tree with
  | Enum { table; _ } -> begin
      let values : (Binder.t * PatternTree.t) list =
        Ast.Identifier.Map.data table
      in
      let subtrees : PatternTree.t list =
        List.map ~f:snd values
      in
      List.exists subtrees ~f:contains_gap
    end

  | Variant { table; _ } -> begin
      let values : (Binder.t * PatternTree.variant_binders * PatternTree.t) list =
        Ast.Identifier.Map.data table
      in
      let subtrees : PatternTree.t list =
        List.map ~f:Auxlib.Triple.third values
      in
      List.exists subtrees ~f:contains_gap
    end

  | Bool (SingleBoolCase (_, subtree)) -> begin
      contains_gap subtree
    end

  | Bool (SeparateBoolCases { when_true; when_false }) -> begin
      contains_gap when_true || contains_gap when_false
    end

  | Atomic (_, _, subtree) -> contains_gap subtree
  | Terminal statement     -> Option.is_none statement


let adorn_pattern_tree
    (location          : S.l            )
    (pattern_tree      : PatternTree.t  )
    (tuple_subpatterns : Pattern.t list )
    (body              : Ast.Statement.t) : PatternTree.t TC.t
  =
  let rec adorn
      (pattern_tree      : PatternTree.t )
      (tuple_subpatterns : Pattern.t list)
      (gap_filling       : bool          ) : PatternTree.t TC.t
    =
    let invalid_number_of_subpatterns (location : Lexing.position) =
      TC.fail location "the tree should be as deep as there are tuple subpatterns"
    and invalid_pattern (location : Lexing.position) =
      TC.fail location "pattern is incompatible with type of value being matched"
    in
    match pattern_tree with
    | Bool binders -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | Binder pattern_binder -> begin
                (*
                   Example context

                     // if pattern_binder.wildcard = false
                     match boolean_value {
                       x = > ...
                     }

                   or

                     // if pattern_binder.wildcard = true
                     match boolean_value {
                       _ => ...
                     }
                *)
                match binders with
                | SingleBoolCase (old_binder, old_subtree) -> begin
                    let* new_binder =
                      Binder.unify old_binder pattern_binder
                    and* new_subtree =
                      adorn old_subtree remaining_subpatterns true
                    in
                    TC.return begin
                      PatternTree.Bool begin
                        PatternTree.SingleBoolCase (new_binder, new_subtree)
                      end
                    end
                  end
                | SeparateBoolCases { when_true = old_when_true; when_false = old_when_false } -> begin
                    let* () =
                      if not pattern_binder.wildcard
                      then begin
                        (*
                           We have

                             match bool_value {
                               x => ...
                             }

                           The current implementation translates this to

                             match bool_value {
                               true => ...,
                               false => ...
                             }

                           whereas it should be

                             match bool_value {
                               true => let x = true in ...,
                               false => let x = true in ...,
                             }
                        *)
                        TC.log [%here] Logging.warning @@ lazy "ignoring binder while matching bool"
                      end
                      else TC.return ()
                    in
                    let* new_when_true =
                      adorn old_when_true remaining_subpatterns true
                    and* new_when_false =
                      adorn old_when_false remaining_subpatterns true
                    in
                    TC.return begin
                      PatternTree.Bool begin
                        PatternTree.SeparateBoolCases {
                          when_true  = new_when_true;
                          when_false = new_when_false
                        }
                      end
                    end
                  end
              end
            | BoolCase b -> begin
                match binders with
                | SingleBoolCase (binder, old_subtree) -> begin
                    if
                      binder.wildcard
                    then
                      (*
                         Example context

                           match boolean_value {
                             _    => A,
                             true => B
                           }

                         We interpret this as

                           match boolean_value {
                             false => A,
                             true  => B
                           }
                      *)
                      let* new_subtree =
                        adorn old_subtree remaining_subpatterns gap_filling
                      in
                      let when_true, when_false =
                        if
                          b
                        then
                          (new_subtree, old_subtree)
                        else
                          (old_subtree, new_subtree)
                      in
                      TC.return begin
                        PatternTree.Bool begin
                          PatternTree.SeparateBoolCases { when_true; when_false }
                        end
                      end
                    else begin
                      (*
                         Example context

                           match boolean_value {
                             x    => ...,
                             true => ...
                           }

                         We do not support this case.
                         This situation possibly occurs when it is combined with other patterns, such as

                           match boolean_value_1, boolean_value_2 {
                             x    , true  => ...,
                             false, false => ...,
                             true , false => ...
                           }
                      *)
                      TC.not_yet_implemented [%here] location
                    end
                  end
                | SeparateBoolCases { when_true; when_false } -> begin
                    if
                      b
                    then
                      let* when_true =
                        adorn when_true remaining_subpatterns gap_filling
                      in
                      TC.return begin
                        PatternTree.Bool begin
                          PatternTree.SeparateBoolCases { when_true; when_false }
                        end
                      end
                    else
                      let* when_false =
                        adorn when_false remaining_subpatterns gap_filling
                      in
                      TC.return begin
                        PatternTree.Bool begin
                          PatternTree.SeparateBoolCases { when_true; when_false }
                        end
                      end
                  end
              end
            | ListCons (_, _)    -> invalid_pattern [%here]
            | ListNil            -> invalid_pattern [%here]
            | Tuple _            -> invalid_pattern [%here]
            | EnumCase _         -> invalid_pattern [%here]
            | VariantCase (_, _) -> invalid_pattern [%here]
            | Unit               -> invalid_pattern [%here]
          end
        | _ -> invalid_number_of_subpatterns [%here]
      end

    | Enum { enum_identifier; table } -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | EnumCase case_identifier -> begin
                let* updated_table =
                  let binder_identifier, subtree =
                    Ast.Identifier.Map.find_exn table case_identifier
                  in
                  let* updated_subtree =
                    adorn subtree remaining_subpatterns gap_filling
                  in
                  TC.return begin
                    Ast.Identifier.Map.overwrite
                      table
                      ~key:case_identifier
                      ~data:(binder_identifier, updated_subtree)
                  end
                in
                TC.return begin
                  PatternTree.Enum { enum_identifier; table = updated_table }
                end
              end
            | Binder pattern_binder -> begin
                let* enum_definition =
                  TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
                in
                let enum_cases =
                  enum_definition.cases
                in
                let update_table
                    (table     : (Binder.t * PatternTree.t) Ast.Identifier.Map.t)
                    (enum_case : Ast.Identifier.t                               ) : (Binder.t * PatternTree.t) Ast.Identifier.Map.t TC.t
                  =
                  let binder, subtree =
                    Ast.Identifier.Map.find_exn table enum_case
                  in
                  if
                    contains_gap subtree
                  then begin
                    let* updated_subtree : PatternTree.t =
                      adorn
                        subtree
                        remaining_subpatterns
                        true
                    in
                    let* updated_binder_identifier : Binder.t =
                      Binder.unify binder pattern_binder
                    in
                    TC.return begin
                      Ast.Identifier.Map.overwrite
                        table
                        ~key:enum_case
                        ~data:(updated_binder_identifier, updated_subtree)
                    end
                  end
                  else
                    TC.return table
                in
                let* updated_table =
                  TC.fold_left ~f:update_table ~init:table enum_cases
                in
                TC.return begin
                  PatternTree.Enum { enum_identifier; table = updated_table }
                end
              end
            | Unit               -> invalid_pattern [%here]
            | ListCons (_, _)    -> invalid_pattern [%here]
            | ListNil            -> invalid_pattern [%here]
            | Tuple _            -> invalid_pattern [%here]
            | VariantCase (_, _) -> invalid_pattern [%here]
            | BoolCase _         -> invalid_pattern [%here]
          end
        | [] -> invalid_number_of_subpatterns [%here]
      end

    | Variant { variant_identifier; table } -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | VariantCase (constructor_identifier, field_pattern) -> begin
                (*
                   Example context:

                     match ??? {
                       <constructor_identifier>(<field_pattern>) => ...
                     }
                *)
                let* updated_table : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t =
                  let old_binder, old_field_binders, old_subtree = Ast.Identifier.Map.find_exn table constructor_identifier
                  in
                  let* new_binder =
                    TC.return old_binder
                  and* new_subtree =
                    adorn old_subtree remaining_subpatterns gap_filling
                  and* new_field_binders =
                    match old_field_binders with
                    | NullaryConstructor old_field_binder -> begin
                        let* new_field_binder =
                          match field_pattern with
                          | Binder pattern_binder -> Binder.unify old_field_binder pattern_binder
                          | Unit                  -> TC.return old_field_binder
                          | ListCons (_, _)       -> invalid_pattern [%here]
                          | ListNil               -> invalid_pattern [%here]
                          | Tuple _               -> invalid_pattern [%here]
                          | EnumCase _            -> invalid_pattern [%here]
                          | VariantCase (_, _)    -> invalid_pattern [%here]
                          | BoolCase _            -> invalid_pattern [%here]
                        in
                        TC.return @@ PatternTree.NullaryConstructor new_field_binder
                      end
                    | UnaryConstructor old_field_binder -> begin
                        let* new_field_binder : Binder.t =
                          match field_pattern with
                          | Binder pattern_binder -> Binder.unify old_field_binder pattern_binder
                          | Unit                  -> invalid_pattern [%here]
                          | ListCons (_, _)       -> invalid_pattern [%here]
                          | ListNil               -> invalid_pattern [%here]
                          | Tuple _               -> invalid_pattern [%here]
                          | EnumCase _            -> invalid_pattern [%here]
                          | VariantCase (_, _)    -> invalid_pattern [%here]
                          | BoolCase _            -> invalid_pattern [%here]
                        in
                        TC.return @@ PatternTree.UnaryConstructor new_field_binder
                      end
                    | NAryConstructor old_field_binders -> begin
                        let* variant_definition =
                          TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
                        in
                        let _constructor : Ast.Identifier.t * Ast.Type.t list =
                          List.find_exn variant_definition.constructors ~f:(fun (id, _) -> Ast.Identifier.equal id constructor_identifier)
                        in
                        let* pattern_field_binders : Binder.t list =
                          match field_pattern with
                          | Tuple subpatterns  -> begin
                              (* We expect all subpatterns to be binders *)
                              let extract_identifier_from_binder (pattern : Pattern.t) : Binder.t TC.t =
                                match pattern with
                                | Binder binder -> TC.return binder
                                | _             -> TC.not_yet_implemented ~message:"only binder patterns supported; no nesting of patterns allowed for now" [%here] location
                              in
                              TC.map subpatterns ~f:extract_identifier_from_binder
                            end
                          | Binder _ -> begin
                            (*
                               Example context

                                 enum A = { A1 : (int, int) }

                                 match A_value {
                                   A(ns) => ...
                                 }

                               In other words, all fields are bound to a single binder, i.e., ns should be bound to a (int, int) pair.
                               The current implementation does not support this.
                               A possible rewrite would be

                                 match A_value {
                                   A(x, y) => let ns = (x, y) in ...
                                 }
                            *)
                              TC.not_yet_implemented ~message:"unsupported binder at this location" [%here] location
                            end
                          | ListCons (_, _)    -> invalid_pattern [%here]
                          | ListNil            -> invalid_pattern [%here]
                          | EnumCase _         -> invalid_pattern [%here]
                          | VariantCase (_, _) -> invalid_pattern [%here]
                          | Unit               -> invalid_pattern [%here]
                          | BoolCase _         -> invalid_pattern [%here]
                        in
                        let* unified_field_binders =
                          TC.map ~f:(Auxlib.uncurry Binder.unify) (List.zip_exn old_field_binders pattern_field_binders)
                        in
                        TC.return @@ PatternTree.NAryConstructor unified_field_binders
                      end
                  in
                  let data =
                    (
                      new_binder,
                      new_field_binders,
                      new_subtree
                    )
                  in
                  TC.return begin
                    Ast.Identifier.Map.overwrite
                      table
                      ~key:constructor_identifier
                      ~data
                  end
                in
                TC.return begin
                  PatternTree.Variant {
                    variant_identifier;
                    table = updated_table
                  }
                end
              end
            | Binder pattern_binder -> begin
                (*
                   Example context:

                     match ??? {
                       x => ...      // if pattern_binder_wildcard == false
                     }

                   or

                     match ??? {
                       _ => ...      // if pattern_binder_wildcard == true
                     }

                   where x is not a constructor name.
                *)
                let* variant_definition =
                  TC.lookup_definition Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
                in
                let* updated_table : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t =
                  let update_table
                      (table                                  : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t)
                      ((constructor_identifier, _field_types) : Ast.Identifier.t * Ast.Type.t list                                           ) : (Binder.t * PatternTree.variant_binders * PatternTree.t) Ast.Identifier.Map.t TC.t
                    =
                    let (old_binder, old_field_binders, old_subtree) : Binder.t * PatternTree.variant_binders * PatternTree.t =
                      Ast.Identifier.Map.find_exn table constructor_identifier
                    in
                    let* new_binder =
                      Binder.unify old_binder pattern_binder
                    and* new_subtree =
                      adorn old_subtree remaining_subpatterns true
                    in
                    let new_field_binders =
                      old_field_binders
                    in
                    TC.return begin
                      Ast.Identifier.Map.overwrite
                        table
                        ~key:constructor_identifier
                        ~data:(new_binder, new_field_binders, new_subtree)
                    end
                  in
                  TC.fold_left
                    ~f:update_table
                    ~init:table
                    variant_definition.constructors
                in
                TC.return begin
                  PatternTree.Variant {
                    variant_identifier;
                    table = updated_table
                  }
                end
              end
            | EnumCase _         -> invalid_pattern [%here]
            | BoolCase _         -> invalid_pattern [%here]
            | Unit               -> invalid_pattern [%here]
            | ListCons (_, _)    -> invalid_pattern [%here]
            | ListNil            -> invalid_pattern [%here]
            | Tuple _            -> invalid_pattern [%here]
          end
        | [] -> invalid_number_of_subpatterns [%here]
      end

    | Atomic (element_type, binder, subtree) -> begin
        match tuple_subpatterns with
        | first_subpattern :: remaining_subpatterns -> begin
            match first_subpattern with
            | Binder pattern_binder -> begin
                let* updated_subtree =
                  adorn
                    subtree
                    remaining_subpatterns
                    gap_filling
                in
                let* updated_binder =
                  Binder.unify binder pattern_binder
                in
                TC.return @@ PatternTree.Atomic (element_type, updated_binder, updated_subtree)
              end
            | _ -> TC.fail [%here] "invalid pattern"
          end
        | [] -> invalid_number_of_subpatterns [%here]
      end

    | Terminal statement -> begin
        match tuple_subpatterns with
        | [] -> begin
            match statement with
            | Some _ -> begin
                if
                  gap_filling
                then
                  (* We're in gap-filling mode, but there is no gap, so keep things as they are *)
                  TC.return pattern_tree
                else
                  (*
                     We're not in gap-filling mode, and we expect a gap.
                     However, there is none, which means we're dealing with the same case twice, which should never occur.
                  *)
                  TC.fail [%here] "clashing patterns"
              end
            | None -> TC.return @@ PatternTree.Terminal (Some body)
          end
        | _::_ -> invalid_number_of_subpatterns [%here]
      end
  in
  adorn pattern_tree tuple_subpatterns false


let rec build_leveled_match_statements
    (matched_identifiers : Ast.Identifier.t list)
    (pattern_tree        : PatternTree.t        ) : Ast.Statement.t TC.t
  =
  let invalid_number_of_tuple_elements (location : Lexing.position) =
    TC.fail location "invalid number of tuple elements"
  in
  let fail_due_to_unhandled_cases =
    TC.return @@ Ast.Statement.Fail "incomplete matching"
  in
  match pattern_tree with
  | Bool bindings -> begin
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
          match bindings with
          | SingleBoolCase (binder, subtree) -> begin
              let* substatement =
                build_leveled_match_statements remaining_matched_identifiers subtree
              in
              if
                not binder.wildcard
              then
                TC.return begin
                  Ast.Statement.Let {
                    variable_identifier    = binder.identifier;
                    binding_statement_type = Ast.Type.Bool;
                    binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_matched_identifier, Ast.Type.Bool));
                    body_statement         = substatement;
                  }
                end
              else
                TC.return substatement
            end
          | SeparateBoolCases { when_true; when_false } -> begin
              let* when_true  = build_leveled_match_statements remaining_matched_identifiers when_true
              and* when_false = build_leveled_match_statements remaining_matched_identifiers when_false
              in
              TC.return begin
                Ast.Statement.Match begin
                  Ast.Statement.MatchBool {
                    condition = first_matched_identifier;
                    when_true;
                    when_false;
                  }
                end
              end
            end
        end
    end

  | Enum { enum_identifier; table } -> begin
      let enum_type = Ast.Type.Enum enum_identifier
      in
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
            (*
               The decorator adds an extra let if necessary.

                 match enum_value {
                   x => ...
                 }

               becomes

                 match enum_value {
                   Foo => let x = enum_value in ...,
                   Bar => let x = enum_value in ...,
                   ...
                 }

            *)
          let decorate_statement
              (binder    : Binder.t       )
              (statement : Ast.Statement.t) : Ast.Statement.t
            =
            if
              not binder.wildcard
            then
              Ast.Statement.Let {
                variable_identifier    = binder.identifier;
                binding_statement_type = enum_type;
                binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_matched_identifier, enum_type));
                body_statement         = statement;
              }
            else
              statement
          in
          let* cases : Ast.Statement.t Ast.Identifier.Map.t =
            let table_pairs : (Ast.Identifier.t * (Binder.t * PatternTree.t)) list =
              Ast.Identifier.Map.to_alist table
            in
            let* statement_pairs : (Ast.Identifier.t * Ast.Statement.t) list =
              let update_pair
                  (enum_case         : Ast.Identifier.t        )
                  ((binder, subtree) : Binder.t * PatternTree.t) : (Ast.Identifier.t * Ast.Statement.t) TC.t
                =
                let* statement : Ast.Statement.t =
                  let* subtree_statement =
                    build_leveled_match_statements remaining_matched_identifiers subtree
                  in
                  TC.return @@ decorate_statement binder subtree_statement
                in
                TC.return (enum_case, statement)
              in
              TC.map ~f:(Auxlib.uncurry update_pair) table_pairs
            in
            TC.return @@ Ast.Identifier.Map.of_alist_exn statement_pairs
          in
          TC.return begin
            Ast.Statement.Match begin
              Ast.Statement.MatchEnum {
                matched      = first_matched_identifier;
                matched_type = enum_identifier;
                cases;
              }
            end
          end
        end
    end

  | Variant { variant_identifier; table } -> begin
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
          let* cases : (Ast.Identifier.t list * Ast.Statement.t) Ast.Identifier.Map.t =
            let table_pairs : (Ast.Identifier.t * (Binder.t * PatternTree.variant_binders * PatternTree.t)) list =
              Ast.Identifier.Map.to_alist table
            in
            let* statement_pairs : (Ast.Identifier.t * (Ast.Identifier.t list * Ast.Statement.t)) list =
              let build_statement_pair
                  (constructor_identifier           : Ast.Identifier.t                                      )
                  ((binder, field_binders, subtree) : Binder.t * PatternTree.variant_binders * PatternTree.t) : (Ast.Identifier.t * (Ast.Identifier.t list * Ast.Statement.t)) TC.t
                =
                let* substatement =
                  let* substatement =
                    build_leveled_match_statements remaining_matched_identifiers subtree
                  in
                  if
                    binder.wildcard
                  then
                    TC.return substatement
                  else
                    let variant_type =
                      Ast.Type.Variant variant_identifier
                    in
                    TC.return begin
                      Ast.Statement.Let {
                        variable_identifier    = binder.identifier;
                        binding_statement_type = variant_type;
                        binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_matched_identifier, variant_type));
                        body_statement         = substatement
                      }
                    end
                in
                match field_binders with
                | NullaryConstructor field_binder -> begin
                    let statement =
                      if
                        field_binder.wildcard
                      then
                        substatement
                      else
                        (*
                           Sail rewrites sometimes produce

                             union A = {
                               A : unit
                             }

                             match A_value {
                               A(var) => func(var)
                             }

                           where var is bound to unit.
                           In other words, Sail wants to "recycle" the same unit value found in the variant value.
                        *)
                        Ast.Statement.Let {
                          variable_identifier    = field_binder.identifier;
                          binding_statement_type = Ast.Type.Unit;
                          binding_statement      = Ast.Statement.Expression (Ast.Expression.Val Ast.Value.Unit);
                          body_statement         = substatement;
                        }
                    in
                    let* binder_for_unit =
                      TC.generate_unique_identifier ()
                    in
                    TC.return (constructor_identifier, ([binder_for_unit], statement))
                  end
                | UnaryConstructor field_binder -> begin
                    TC.return (constructor_identifier, ([field_binder.identifier], substatement))
                  end
                | NAryConstructor field_binders -> begin
                    let field_binder_identifiers : Ast.Identifier.t list =
                      List.map ~f:(fun (binder : Binder.t) -> binder.identifier) field_binders
                    in
                    TC.return (constructor_identifier, (field_binder_identifiers, substatement))
                  end
              in
              TC.map table_pairs ~f:(Auxlib.uncurry build_statement_pair)
            in
            TC.return @@ Ast.Identifier.Map.of_alist_exn statement_pairs
          in
          TC.return begin
            Ast.Statement.Match begin
              Ast.Statement.MatchVariant {
                matched      = first_matched_identifier;
                matched_type = variant_identifier;
                cases
              }
            end
          end
        end
    end

  | Atomic (element_type, binder, subtree) -> begin
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
          let* substatement = build_leveled_match_statements remaining_matched_identifiers subtree
          in
          TC.return begin
            Ast.Statement.Let {
              variable_identifier    = binder.identifier;
              binding_statement_type = element_type;
              binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (first_matched_identifier, element_type));
              body_statement         = substatement;
            }
          end
        end
    end

  | Terminal statement -> begin
      match matched_identifiers with
      | [] -> begin
          match statement with
          | None           -> fail_due_to_unhandled_cases
          | Some statement -> TC.return @@ statement
        end
      | _::_ -> invalid_number_of_tuple_elements [%here]
    end


let create_tuple_match
    (matched       : Ast.Identifier.t                             )
    (element_types : Ast.Type.t list                              )
    (body_builder  : Ast.Identifier.t list -> Ast.Statement.t TC.t) : Ast.Statement.t TC.t
  =
  let tuple_size =
    List.length element_types
  in
  let* binder_identifiers =
    TC.generate_unique_identifiers tuple_size
  in
  let* body =
    body_builder binder_identifiers
  in
  let binders =
    List.zip_exn binder_identifiers element_types
  in
  match binders with
  | []  -> TC.fail [%here] "empty tuple should not occur"
  | [_] -> TC.fail [%here] "singleton tuple should not occur"
  | [ (id_fst, type_fst); (id_snd, type_snd) ] -> begin
      TC.return begin
        Ast.Statement.Match begin
          Ast.Statement.MatchProduct {
            matched;
            type_fst;
            type_snd;
            id_fst;
            id_snd;
            body;
          }
        end
      end
    end
  | _ -> begin
      TC.return begin
        Ast.Statement.Match begin
          Ast.Statement.MatchTuple {
            matched;
            binders;
            body;
          }
        end
      end
    end



exception InconsistentBinders of (Pattern.t * Pattern.t)


(*
   Check that both patterns are binders and have the same name, taking into account wildcards.

   todo: investigate if we can replace this with Binding.unify
*)
let consistent_binders
    (pattern_1 : Pattern.t)
    (pattern_2 : Pattern.t) : bool
  =
  match pattern_1 with
  | ListCons (_, _)    -> false
  | ListNil            -> false
  | Tuple _            -> false
  | EnumCase _         -> false
  | BoolCase _         -> false
  | VariantCase (_, _) -> false
  | Unit               -> false (* todo might need more nuanced logic *)
  | Binder { identifier = identifier_1; wildcard = wildcard_1 } -> begin
      match pattern_2 with
      | Binder { identifier = identifier_2; wildcard = wildcard_2 } -> begin
          wildcard_1 || wildcard_2 || Ast.Identifier.equal identifier_1 identifier_2
        end
      | _ -> false
    end


(*
   Translates a Sail pattern (type S.typ S.apat) into our own pattern (type Pattern.t).
*)
let rec translate_pattern
    (matched_type : Ast.Type.t  )
    (sail_pattern : S.typ S.apat) : Pattern.t TC.t
  =
  let S.AP_aux (unwrapped_sail_pattern, _type_environment, _location) = sail_pattern
  in

  let translate_variable_pattern (sail_identifier : S.id) : Pattern.t TC.t =
    let* identifier = Identifier.translate_identifier [%here] sail_identifier
    in
    TC.return @@ Pattern.Binder { identifier; wildcard = false }

  and translate_wildcard_pattern () : Pattern.t TC.t =
    let* binder = Binder.generate_wildcard
    in
    TC.return @@ Pattern.Binder binder

  and unexpected_pattern (location : Lexing.position) =
    let error_message =
      Printf.sprintf
        "expected pattern %s while matching type %s"
        (StringOf.Sail.apat sail_pattern)
        (FExpr.to_string @@ Ast.Type.to_fexpr matched_type)
    in
    TC.fail location error_message

  and translate_tuple_pattern
      (subpatterns : S.typ S.apat list)
      (subtypes    : Ast.Type.t list  ) : Pattern.t TC.t
    =
    match List.zip subtypes subpatterns with
    | Ok pairs -> begin
        let* translated_subpatterns =
          TC.map ~f:(Auxlib.uncurry translate_pattern) pairs
        in
        TC.return @@ Pattern.Tuple translated_subpatterns
      end
    | Unequal_lengths -> TC.fail [%here] "expected as many types as patterns in tuple"

  in
  let translate_pattern_for_atomic_type () =
    match unwrapped_sail_pattern with
    | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
    | AP_wild _sail_type                  -> translate_wildcard_pattern ()
    | _                                   -> unexpected_pattern [%here]

  in
  match matched_type with
  | List element_type -> begin
      match unwrapped_sail_pattern with
      | AP_cons (head_pattern, tail_pattern) -> begin
          let* head_pattern = translate_pattern element_type head_pattern
          and* tail_pattern = translate_pattern matched_type tail_pattern
          in
          TC.return @@ Pattern.ListCons (head_pattern, tail_pattern)
        end
      | AP_nil _typ                   -> TC.return @@ Pattern.ListNil
      | AP_id (identifier, _sail_typ) -> translate_variable_pattern identifier
      | AP_wild _type                 -> translate_wildcard_pattern ()
      | _                             -> unexpected_pattern [%here]
    end

  | Enum enum_identifier -> begin
      match unwrapped_sail_pattern with
      | AP_id (sail_identifier, _sail_type) -> begin
          let* identifier =
            Identifier.translate_identifier [%here] sail_identifier
          in
          let* enum_definition =
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_enum_named enum_identifier)
          in
          if
            List.mem enum_definition.cases identifier ~equal:Ast.Identifier.equal
          then
            TC.return @@ Pattern.EnumCase identifier
          else
            TC.return @@ Pattern.Binder { identifier; wildcard = false }
        end
      | AP_wild _type -> translate_wildcard_pattern ()
      | _ -> unexpected_pattern [%here]
    end

  | Unit -> begin
      match unwrapped_sail_pattern with
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _type                       -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end

  | Tuple subtypes -> begin
      match unwrapped_sail_pattern with
      | AP_tuple sail_subpatterns           -> translate_tuple_pattern sail_subpatterns subtypes
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _type                       -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end

  | Variant variant_identifier -> begin
      match unwrapped_sail_pattern with
      | AP_app (head_sail_identifier, sail_subpattern, _sail_type) -> begin
          let* head_identifier =
            Identifier.translate_identifier [%here] head_sail_identifier
          in
          let* variant_definition =
            TC.lookup_definition @@ Ast.Definition.Select.(type_definition @@ of_variant_named variant_identifier)
          in
          match List.find variant_definition.constructors ~f:(Fn.compose (Ast.Identifier.equal head_identifier) fst) with (* todo create separate function *)
          | Some (constructor_identifier, field_types) -> begin
              let field_type : Ast.Type.t =
                match field_types with
                | []  -> Ast.Type.Unit
                | [t] -> t
                | ts  -> Ast.Type.Tuple ts
              in
              let* subpattern =
                translate_pattern field_type sail_subpattern
              in
              TC.return @@ Pattern.VariantCase (constructor_identifier, subpattern)
            end
          | None -> begin
              (* This really should never occur *)
              let error_message =
                Printf.sprintf
                  "variant %s has no constructor %s"
                  (Ast.Identifier.to_string variant_identifier)
                  (Ast.Identifier.to_string head_identifier)
              in
              TC.fail [%here] error_message
            end
        end
      | AP_id (sail_identifier, _sail_type) -> translate_variable_pattern sail_identifier
      | AP_wild _sail_type                  -> translate_wildcard_pattern ()
      | _                                   -> unexpected_pattern [%here]
    end

  | Int                -> translate_pattern_for_atomic_type ()
  | Bool               -> translate_pattern_for_atomic_type ()
  | String             -> translate_pattern_for_atomic_type ()
  | Bit                -> translate_pattern_for_atomic_type ()
  | Sum (_, _)         -> translate_pattern_for_atomic_type ()
  | Bitvector _        -> translate_pattern_for_atomic_type ()
  | Record _           -> translate_pattern_for_atomic_type ()
  | Application (_, _) -> translate_pattern_for_atomic_type ()
  | Alias (_, _)       -> translate_pattern_for_atomic_type ()
  | Range (_, _)       -> translate_pattern_for_atomic_type ()


let translate_case
    (location       : S.l            )
    (matched_type   : Ast.Type.t     )
    (sail_pattern   : S.typ S.apat   )
    (sail_condition : S.typ S.aexp   )
    (body           : Ast.Statement.t) : (Pattern.t * Ast.Statement.t) TC.t
  =
  let* pattern = translate_pattern matched_type sail_pattern
  in
  let S.AE_aux (unwrapped_sail_condition, _) = sail_condition
  in
  (* Check that the condition is simply true; we expect this to be the case if the correct rewrites have been activated *)
  let* () =
    match unwrapped_sail_condition with
    | AE_val value -> begin
        match value with
        | AV_lit (literal, _) -> begin
            let S.L_aux (unwrapped_literal, _) = literal
            in
            match unwrapped_literal with
            | L_unit            -> TC.not_yet_implemented [%here] location
            | L_zero            -> TC.not_yet_implemented [%here] location
            | L_one             -> TC.not_yet_implemented [%here] location
            | L_true            -> TC.return ()
            | L_false           -> TC.not_yet_implemented [%here] location
            | L_num _           -> TC.not_yet_implemented [%here] location
            | L_hex _           -> TC.not_yet_implemented [%here] location
            | L_bin _           -> TC.not_yet_implemented [%here] location
            | L_string _        -> TC.not_yet_implemented [%here] location
            | L_undef           -> TC.not_yet_implemented [%here] location
            | L_real _          -> TC.not_yet_implemented [%here] location
          end
        | AV_id (_, _)           -> TC.not_yet_implemented [%here] location
        | AV_ref (_, _)          -> TC.not_yet_implemented [%here] location
        | AV_tuple _             -> TC.not_yet_implemented [%here] location
        | AV_list (_, _)         -> TC.not_yet_implemented [%here] location
        | AV_vector (_, _)       -> TC.not_yet_implemented [%here] location
        | AV_record (_, _)       -> TC.not_yet_implemented [%here] location
        | AV_cval (_, _)         -> TC.not_yet_implemented [%here] location
      end
    | AE_app (_, _, _)           -> TC.not_yet_implemented [%here] location
    | AE_typ (_, _)              -> TC.not_yet_implemented [%here] location
    | AE_assign (_, _)           -> TC.not_yet_implemented [%here] location
    | AE_let (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
    | AE_block (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_return (_, _)           -> TC.not_yet_implemented [%here] location
    | AE_exit (_, _)             -> TC.not_yet_implemented [%here] location
    | AE_throw (_, _)            -> TC.not_yet_implemented [%here] location
    | AE_if (_, _, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_field (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_match (_, _, _)         -> TC.not_yet_implemented [%here] location
    | AE_try (_, _, _)           -> TC.not_yet_implemented [%here] location
    | AE_struct_update (_, _, _) -> TC.not_yet_implemented [%here] location
    | AE_for (_, _, _, _, _, _)  -> TC.not_yet_implemented [%here] location
    | AE_loop (_, _, _)          -> TC.not_yet_implemented [%here] location
    | AE_short_circuit (_, _, _) -> TC.not_yet_implemented [%here] location
  in
  TC.return (pattern, body)


(*
   Matching lists currently only supports very specific patterns.

   * Empty and nonempty list:

       match list {
         [| |] => ...,
         x :: xs => ...
       }

   * Empty, singleton and 2+ list:

       match list {
         [| |] => ...,
         [| x |] => ...,
         x :: y :: rest => ...
       }

   The match cases can be in any order.
   Subpatterns are not allowed in the current implementation.
*)
let translate_list_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_type       : Ast.Type.t                        )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let translate
      (matched_identifier : Ast.Identifier.t)
      (head_identifier    : Ast.Identifier.t)
      (tail_identifier    : Ast.Identifier.t)
      (cons_body          : Ast.Statement.t )
      (nil_body           : Ast.Statement.t ) : Ast.Statement.t TC.t
    =
    TC.return begin
      Ast.Statement.Match begin
        Ast.Statement.MatchList {
          matched      = matched_identifier                           ;
          element_type                                                ;
          when_cons    = (head_identifier, tail_identifier, cons_body);
          when_nil     = nil_body                                     ;
        }
      end
    end
  in
  let cases_sorted_by_pattern_depth =
    let rec pattern_depth (pattern : Pattern.t) : int =
      let fail () =
        failwith "unexpected pattern; only patterns applicable on lists should appear"
      in
      match pattern with
      | ListCons (_, tail) -> 1 + pattern_depth tail
      | ListNil            -> 0
      | Binder _           -> 0
      | Tuple _            -> fail ()
      | EnumCase _         -> fail ()
      | BoolCase _         -> fail ()
      | VariantCase (_, _) -> fail ()
      | Unit               -> fail ()

    in
    let compare (p1, _) (p2, _) =
      pattern_depth p1 - pattern_depth p2
    in
    List.sort cases ~compare
  in
  match cases_sorted_by_pattern_depth with
  | [ (Pattern.Binder { identifier = binder_identifier; _ }, body) ] -> begin
      (*
         We're dealing with

           match lst {
             xs => body
           }

         which we translate to

           let xs = lst in body
      *)
      let matched_type = Ast.Type.List element_type
      in
      TC.return @@ Ast.Statement.Let {
        variable_identifier = binder_identifier;
        binding_statement_type = matched_type;
        binding_statement = Ast.Statement.Expression (Ast.Expression.Variable (matched_identifier, matched_type));
        body_statement = body
      }
    end
  | [ (Pattern.ListNil, nil_body);
      (Pattern.ListCons (Pattern.Binder { identifier = head_identifier; _ }, Pattern.Binder { identifier = tail_identifier; _ }), cons_body) ] -> begin
      translate matched_identifier head_identifier tail_identifier cons_body nil_body
    end
  | [ (Pattern.ListNil, if_empty_list);
      (Pattern.ListCons (Pattern.Binder { identifier = first_identifier_1; wildcard = wildcard_1 },
                         Pattern.ListNil),
       if_singleton_list);
      (Pattern.ListCons (Pattern.Binder { identifier = first_identifier_2; wildcard = wildcard_2 },
                         Pattern.ListCons (Pattern.Binder { identifier = second_identifier; _ },
                                           Pattern.Binder { identifier = rest_identifier; _ })),
       if_two_or_more_elements) ] -> begin
      (*
         We're dealing with

           match lst {
             [| |] => if_empty_list,
             [| first_identifier_1 |] => if_singleton_list,
             first_identifier_2 :: second_identifier :: rest_identifier => if_two_or_more_elements
           }

         Note that this implementation expects that, in case neither is a wildcard, first_identifier_1 equals first_identifier_2,.
      *)
      let are_first_element_binders_consistent =
        if
          wildcard_1 || wildcard_2
        then
          true
        else
          Ast.Identifier.equal first_identifier_1 first_identifier_2
      in
      if
        not are_first_element_binders_consistent
      then
        TC.not_yet_implemented ~message:"differently named first elements in list matching patterns" [%here] location
      else begin
        let first_identifier = first_identifier_1
        in
        let* tail_identifier =
          TC.generate_unique_identifier ()
        in
        let* inner_match =
          translate tail_identifier second_identifier rest_identifier if_two_or_more_elements if_singleton_list
        in
        translate matched_identifier first_identifier tail_identifier inner_match if_empty_list
      end
    end
  | _ -> TC.not_yet_implemented [%here] location


let translate_variant_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (variant_identifier : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let* empty_pattern_tree =
    build_empty_pattern_tree
      location
      [ Ast.Type.Variant variant_identifier ]
  in
  let* pattern_tree =
    TC.fold_left
      cases
      ~f:(fun tree (pattern, statement) -> adorn_pattern_tree location tree [ pattern ] statement)
      ~init:empty_pattern_tree
  in
  let* result = build_leveled_match_statements [ matched_identifier ] pattern_tree
  in
  TC.return result


(*
   We support a small number of specific matching structures.
*)
let translate_tuple_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_types      : Ast.Type.t list                   )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let translate_using_pattern_tree : Ast.Statement.t TC.t =
    (* keeps things lazy *)
    let* () = TC.return ()
    in
    let builder (binder_identifiers : Ast.Identifier.t list) : Ast.Statement.t TC.t =
      let* initial_tree =
        build_empty_pattern_tree
          location
          element_types
      in
      let categorize
          (tree      : PatternTree.t  )
          (pattern   : Pattern.t      )
          (statement : Ast.Statement.t) : PatternTree.t TC.t
        =
        match pattern with
        | Tuple subpatterns -> adorn_pattern_tree location tree subpatterns statement
        | _                 -> TC.fail [%here] "expected tuple pattern"
      in
      let* final_tree =
        TC.fold_left
          ~init:initial_tree
          ~f:(fun tree (pattern, statement) -> categorize tree pattern statement)
          cases
      in
      build_leveled_match_statements binder_identifiers final_tree
    in
    let* result =
      create_tuple_match
        matched_identifier
        element_types
        builder
    in
    TC.return result

  (*
     This function deals with the special case of having a single match pattern that contains nothing but binders, i.e.,

       match tuple_value {
         (X1, X2, ..., Xn) => ...
       }
  *)
  and translate_tuple_of_binders : Ast.Statement.t TC.t =
    (* Keeps things lazy *)
    let* () = TC.return ()
    in
    match cases with
    | [ (Pattern.Tuple subpatterns, body) ] when List.for_all subpatterns ~f:Pattern.is_binder -> begin
        let binding_variables =
          List.map subpatterns ~f:Pattern.identifier_of_binder
        in
        match List.zip binding_variables element_types with
        | List.Or_unequal_lengths.Unequal_lengths -> begin
            (* Should never occur *)
            TC.fail [%here] "different number of tuple pattern elements and tuple pattern types"
          end
        | List.Or_unequal_lengths.Ok binder_type_pairs -> begin
            match binder_type_pairs with
            | []  -> TC.fail [%here] "unexpected empty tuple"
            | [_] -> TC.fail [%here] "unexpected singleton tuple"
            | [(id_fst, type_fst); (id_snd, type_snd)] -> begin
                let match_pattern =
                  Ast.Statement.MatchProduct {
                    matched = matched_identifier;
                    type_fst;
                    type_snd;
                    id_fst;
                    id_snd;
                    body
                  }
                in
                TC.return @@ Ast.Statement.Match match_pattern
              end
            | _ -> begin
                let match_pattern =
                  Ast.Statement.MatchTuple {
                    matched = matched_identifier;
                    binders = binder_type_pairs;
                    body
                  }
                in
                TC.return @@ Ast.Statement.Match match_pattern
              end
          end
      end
    | _ -> TC.not_yet_implemented [%here] location
  in
  TC.try_multiple [
    translate_using_pattern_tree;
    translate_tuple_of_binders;
  ]


let translate_unit_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  match cases with
  | [ (pattern, statement) ] -> begin
      let* pattern_tree =
        let* tree =
          build_empty_pattern_tree
            location
            [ Ast.Type.Unit ]
        in
        adorn_pattern_tree
          location
          tree
          [ pattern ]
          statement
      in
      build_leveled_match_statements
        [ matched_identifier ]
        pattern_tree
    end
  | _ -> TC.fail [%here] "invalid number of cases"


let translate_enum_match
    (location          : S.l                                )
    (matched_identifier : Ast.Identifier.t                  )
    (enum_identifier    : Ast.Identifier.t                  )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let* empty_pattern_tree =
    build_empty_pattern_tree
      location
      [ Ast.Type.Enum enum_identifier ]
  in
  let* pattern_tree =
    TC.fold_left
      cases
      ~f:(fun tree (pattern, statement) -> adorn_pattern_tree location tree [ pattern ] statement)
      ~init:empty_pattern_tree
  in
  build_leveled_match_statements [ matched_identifier ] pattern_tree


let translate
    (location           : S.l                                                 )
    (matched_identifier : Ast.Identifier.t                                    )
    (matched_type       : Ast.Type.t                                          )
    (cases              : (S.typ S.apat * S.typ S.aexp * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let* translated_cases =
    let f (pattern, condition, clause) =
      translate_case location matched_type pattern condition clause
    in
    TC.map ~f cases
  in
  match matched_type with
  | List element_type            -> translate_list_match location matched_identifier element_type translated_cases
  | Unit                         -> translate_unit_match location matched_identifier translated_cases
  | Enum enum_identifier         -> translate_enum_match location matched_identifier enum_identifier translated_cases
  | Variant variant_identifier   -> translate_variant_match location matched_identifier variant_identifier translated_cases
  | Tuple element_types          -> translate_tuple_match location matched_identifier element_types translated_cases
  | Int                          -> TC.not_yet_implemented [%here] location
  | Bool                         -> TC.not_yet_implemented [%here] location
  | String                       -> TC.not_yet_implemented [%here] location
  | Bit                          -> TC.not_yet_implemented [%here] location
  | Sum (_, _)                   -> TC.not_yet_implemented [%here] location
  | Bitvector _                  -> TC.not_yet_implemented [%here] location
  | Record _                     -> TC.not_yet_implemented [%here] location
  | Application (_, _)           -> TC.not_yet_implemented [%here] location
  | Alias (_, _)                 -> TC.not_yet_implemented [%here] location
  | Range (_, _)                 -> TC.not_yet_implemented [%here] location
