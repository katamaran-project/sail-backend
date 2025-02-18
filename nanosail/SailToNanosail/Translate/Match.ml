(*

    This module contains all logic related to the translation of a Sail match to a nSail equivalent.


    Problem statement
    =================
    Sail code contains match statements which we need to support.
    A simple example would be

      enum MyEnum = { x, y, z }

      match e {
        x => 1,
        y => 2,
        z => 3
      }

    In nSail this is represented by (has not been checked thoroughly):

      Ast.Statement.Match begin
        Ast.Statement.MatchEnum {
          matched      = Id "e";
          matched_type = Enum (Id "MyEnum");
          cases        = Ast.Identifier.Map.of_alist [
                           (Id "x", Expression (Val (Ast.Value.Int 1)));
                           (Id "y", Expression (Val (Ast.Value.Int 2)));
                           (Id "z", Expression (Val (Ast.Value.Int 3)));
                         ]
        }
      end

    which should ultimately be translated to

      stm_match_enum Emyenum
                     (stm_exp (exp_var "e"))
                     (fun K => match K with
                               | x => stm_exp (exp_int 1%Z)
                               | y => stm_exp (exp_int 2%Z)
                               | z => stm_exp (exp_int 3%Z)
                               end)


    ISSUE 1 : Statements vs Expressions
    -----------------------------------
    A first small issue is the fact that muSail match statements operate on *expressions*, whereas
    some Sail-expressions are actually translated into muSail *statements*.
    This problem is dealt with outside this module using a helper let constructor:

      match e {
        ...
      }

    becomes

      let generated = e
      in
      match generated {
        ...
      }

    or, more formally,

      Ast.Statement.Let {
        binder                 = Id "generated";
        binding_statement_type = Enum (Id "MyEnum");
        binding_statement      = Expression e;
        body_statement         = Match begin
                                   MatchEnum {
                                     matched      = Id "generated";
                                     matched_type = Enum (Id "MyEnum");
                                     ...
                                   }
                                 end


    ISSUE 2 : Nested Patterns
    -------------------------
    Sail supports nested patterns, such as

      enum A = { A1, A2 }
      union B = { B1 : A, B2 : A }

      match B_value {
        B1(A1) => 1,
        B1(A2) => 2,
        B2(A1) => 3,
        B2(A2) => 4,
      }

    In muSail we use stm_match_prod, stm_match_tuple, stm_match_list, stm_match_enum,
    stm_match_union_alt_list, and so on, which only match "one deep", so we
    need to transform the above match into

      match B_value {
        B1(x) => match x {
                   A1 => 1,
                   A2 => 2
                 },
        B2(x) => match x {
                   A1 => 3,
                   A2 => 4
                 }
      }

    The current implementation does NOT support this yet.
    Only one specific instance of subpatterns is allowed for now,
    namely those appearing in tuples:

      enum A = { A1, A2 }
      match pair {
        (A1, A1) => 1,
        (A1, A2) => 2,
        (A2, A1) => 3,
        (A2, A2) => 4
      }

    We support this special case because of scattered functions,
    which are rewritten by Sail as matches against tuples.

      enum A = { A1, A2 }
      val foo : (MyEnum, int) -> int
      scattered function foo

      function clause foo(A1, x) = ...
      function clause foo(A2, x) = ...

    is rewritten (in Sail itself) into

      function foo merge#var =      // merge#var is a tuple typed (A, int)
        match merge#var {
          (A1, x) => ...,
          (A2, x) => ...,
        }


    ISSUE 3 : Wildcards
    -------------------
    Sail allows wildcards in patterns, making it possible to deal with
    multiple cases at once.
    When our current implementation encounters a wildcard, e.g.,
    _ => foo or x => foo, it will duplicate foo for every as of yet unmatched case.
    In other words,

       enum A = { A1, A2, A3 }
       match A_value {
         A1 => 1,
         _  => 2
       }

    is reinterpreted as

      match A_value {
        A1 => 1,
        A2 => 2,
        A3 => 2
      }

    Note that if a value (e.g., enum, variant/union, bool)


    ISSUE 4 : Clashing Binders
    --------------------------
    It is possible that the same value is bound to different identifiers in
    different match clauses.

      enum A { A1, A2 }
      match (int_value, A_value) {
        x, A1 => x,
        y, A2 => y
      }

    In the above example, int_value is bound to x and y by different clauses.
    In general, the current implementation expects the same value to be bound to the same
    identifier in all clauses.
    (It just so happens that in this example, although clear, the implementation is actually
    able to handle the clashing binders, as explained below. An example
    of an unsupported case is also given.)


    There are some specific cases where the current implementation can
    deal with clashing binders: a binder X is upgraded to a wildcard if no reference to X
    appears in the right hand side of the clause.

      match (int_value, A_value) {
        x, A1 => stm1          // x not in free(stm1)
      }

    becomes

      match (int_value, A_value) {
        _, A1 => stm1
      }

    Another case where clashing binders are allowed is when
    the same value has been matched exclusively against binders/wildcards:

      enum A = { A1, A2 }
      match pair {
        (x, A1) => x,
        (y, A2) => y
      }

    This will be transformed into

      match pair {
        (gen, A1) => gen,
        (gen, A2) => gen
      }

    An example that will be rejected by the current implementation is

      enum A = { A1, A2 }
      match pair {
        (A1, A1) => 1,
        (x,  A1) => 2,
        (y,   _) => 3,
      }

    This fails because the pattern node is expanded by the first clause,
    while the implementation can only resolve unexpanded nodes.


    Optimization
    ------------
    Given a tuple (x, y, z), the order in which the values are matched against
    impact the size of the generated code.

    The current implementation relies on brute force to find the smallest tree:
    it tries all possible orderings. This approach should be acceptable
    for most cases, i.e., where tuples don't grow larger than 8 elements.


    Pattern Trees
    =============

    TODOs
    =====
    * Generalized support for nested patterns
    * Improved optimization
    * Avoid duplication in case of wildcards
       * Introduce wildcards in muSail
       * Put duplicated code in a function
*)

open! ExtBase

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
        else begin
          let error_message =
            Printf.sprintf "cannot unify binders %s and %s"
              (Ast.Identifier.to_string binder_1.identifier)
              (Ast.Identifier.to_string binder_2.identifier)
          in
          TC.fail [%here] error_message
        end
      end


  let unify'
      (binder_1 : t)
      (binder_2 : t) : t option TC.t
    =
    match binder_1.wildcard, binder_2.wildcard with
    | true, true -> begin
        if
          Ast.Identifier.is_generated binder_1.identifier
        then
          TC.return @@ Some binder_2
        else
          TC.return @@ Some binder_1
      end
    | true , false -> TC.return @@ Some binder_2
    | false, true  -> TC.return @@ Some binder_1
    | false, false -> begin
        if
          Ast.Identifier.equal binder_1.identifier binder_2.identifier
        then
          TC.return @@ Some binder_1
        else
          TC.return None
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


  let upgrade_unused_binders_to_wildcards
      (pattern        : t                   )
      (free_variables : Ast.Identifier.Set.t) : t
    =
    let rec upgrade (pattern : t) : t =
      match pattern with
      | Binder { identifier; wildcard = _ } -> begin
          if
            Ast.Identifier.Set.mem free_variables identifier
          then
            pattern
          else
            Binder { identifier; wildcard = true }
        end
      | ListCons (head_pattern, tail_pattern)   -> ListCons (upgrade head_pattern, upgrade tail_pattern)
      | ListNil                                 -> pattern
      | Tuple subpatterns                       -> Tuple (List.map ~f:upgrade subpatterns)
      | EnumCase _                              -> pattern
      | VariantCase (field_binders, subpattern) -> VariantCase (field_binders, upgrade subpattern)
      | BoolCase _                              -> pattern
      | Unit                                    -> pattern
    in
    upgrade pattern


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
    | Enum       of { enum_identifier    : Ast.Identifier.t; table : (Binder.t * t) Ast.Identifier.Map.t }
    | Variant    of { variant_identifier : Ast.Identifier.t; table : (Binder.t * variant_binders * t) Ast.Identifier.Map.t }
    | Bool       of { when_true : t; when_false : t }
    | Binder     of { matched_type : Ast.Type.t; binder : Binder.t; subtree : t }
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

    | Bool { when_true = when_true_1; when_false = when_false_1 } -> begin
        match node_2 with
        | Bool { when_true = when_true_2; when_false = when_false_2 } -> begin
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

    | Binder { matched_type = type_1; binder = binder_1; subtree = subtree_1 } -> begin
        match node_2 with
        | Binder { matched_type = type_2; binder = binder_2; subtree = subtree_2 } -> begin
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

    | Bool { when_true; when_false } -> begin
        let keyword =
          [
            ("true", to_fexpr when_true);
            ("false", to_fexpr when_false);
          ]
        in
        FExpr.mk_application ~keyword @@ mk_head "Bool"
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

    | Binder { matched_type; binder; subtree } -> begin
        let keyword =
          [
            (
              "type",
              Ast.Type.to_fexpr matched_type
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
        FExpr.mk_application ~keyword @@ mk_head "Binder"
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

    | Binder { subtree; _ }          -> 1 + count_nodes subtree
    | Bool { when_true; when_false } -> 1 + count_nodes when_true + count_nodes when_false
    | Terminal _                     -> 1


  let rename
      (renamer : Ast.Identifier.t -> Ast.Identifier.t)
      (tree    : t                                   ) : t
    =
    let rec rename (tree : t) : t =
      match (tree : t) with
      | Enum { enum_identifier; table } -> begin
          Enum {
            enum_identifier;
            table = Ast.Identifier.Map.map_values ~f:(fun (id, subtree) -> (id, rename subtree)) table
          }
        end

      | Variant { variant_identifier; table } -> begin
          Variant {
            variant_identifier;
            table = Ast.Identifier.Map.map_values ~f:(fun (binder, binders, subtree) -> (binder, binders, rename subtree)) table
          }
        end

      | Bool { when_true; when_false } -> begin
          Bool {
            when_true  = rename when_true;
            when_false = rename when_false
          }
        end

      | Binder { matched_type; binder; subtree } -> begin
          Binder {
            matched_type;
            binder;
            subtree = rename subtree
          }
        end

      | Terminal None -> tree

      | Terminal (Some statement) -> begin
          Terminal (Some (Ast.Renaming.rename_in_statement renamer statement))
        end
    in
    rename tree
end


let build_enum_node
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


let build_bool_node (subtree : PatternTree.t) : PatternTree.t TC.t =
  TC.return begin
    PatternTree.Bool { when_true = subtree; when_false = subtree }
  end


let build_variant_node
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
            let field_count    = List.length field_types
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


let build_binder_node
    (element_type : Ast.Type.t   )
    (subtree      : PatternTree.t) : PatternTree.t TC.t
  =
  let* binder =
    Binder.generate_wildcard
  in
  TC.return @@ PatternTree.Binder { matched_type = element_type; binder; subtree }


let rec build_empty_pattern_tree
    (location      : S.l            )
    (element_types : Ast.Type.t list) : PatternTree.t TC.t
  =
  match element_types with
  | []           -> TC.return @@ PatternTree.Terminal None
  | head :: tail -> begin
      let* subtree =
        build_empty_pattern_tree location tail
      in
      let* tree =
        (*
           Using atomic_node is the easy way out as it only provides support for binders.
           When the need arises, it can be useful to implement specialized functions for specific types.
        *)
        match head with
        | Enum enum_identifier         -> build_binder_node (Ast.Type.Enum enum_identifier) subtree
        | Int                          -> build_binder_node Ast.Type.Int subtree
        | Variant variant_identifier   -> build_binder_node (Ast.Type.Variant variant_identifier) subtree
        | Unit                         -> build_binder_node Ast.Type.Unit subtree
        | Bool                         -> build_binder_node Ast.Type.Bool subtree
        | String                       -> build_binder_node Ast.Type.String subtree
        | Bit                          -> TC.not_yet_implemented [%here] location
        | List _                       -> TC.not_yet_implemented [%here] location
        | Sum (_, _)                   -> TC.not_yet_implemented [%here] location
        | Bitvector numeric_expression -> build_binder_node (Ast.Type.Bitvector numeric_expression) subtree
        | Tuple _                      -> TC.not_yet_implemented [%here] location
        | Record _                     -> TC.not_yet_implemented [%here] location
        | Application (_, _)           -> TC.not_yet_implemented [%here] location
        | Alias (_, _)                 -> TC.not_yet_implemented [%here] location
        | Range (_, _)                 -> TC.not_yet_implemented [%here] location
      in
      let* () =
        let node_count = PatternTree.count_nodes tree
        in
        if
          node_count > 100
        then
          let message : PP.document Lazy.t = lazy begin
            PP.format "Constructed pattern tree with %d nodes" node_count
          end
          in
          TC.log [%here] Logging.warning message
        else
          TC.return ()
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

  | Bool { when_true; when_false } -> begin
      contains_gap when_true || contains_gap when_false
    end

  | Binder { subtree; _ }  -> contains_gap subtree
  | Terminal statement     -> Option.is_none statement


let rec adorn_pattern_tree
    (location          : S.l                    )
    (pattern_tree      : PatternTree.t          )
    (tuple_subpatterns : Pattern.t list         )
    ?(gap_filling      : bool           = false )
    (body              : Ast.Statement.t        ) : PatternTree.t TC.t
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
    match pattern_tree, tuple_subpatterns with
    | Terminal statement, [] -> begin
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

    | Terminal _, _ -> invalid_number_of_subpatterns [%here]

    | _, [] -> invalid_number_of_subpatterns [%here]

    | Bool { when_true = old_when_true; when_false = old_when_false }, first_subpattern :: remaining_subpatterns -> begin
        match first_subpattern with
        | Binder pattern_binder -> begin
            (*
               Example context

                 // if pattern_binder.wildcard = false
                 match boolean_value {
                   false => ...,
                   x = > ...
                 }

               or

                 // if pattern_binder.wildcard = true
                 match boolean_value {
                   false => ...,
                   _ => ...
                 }

               In other words, the Bool pattern tree node has been expanded by a first clause,
               and a second clause uses a binder/wildcard to match the boolean value.
            *)
            let* () =
              if
                not pattern_binder.wildcard
              then begin
                (*
                   We have

                     match bool_value {
                       false => ...,
                       x => ...
                     }

                   The current implementation translates this to

                     match bool_value {
                       false => ...,
                       true => ...
                     }

                   whereas it should be

                     match bool_value {
                       false => ...,
                       true => let x = true in ...,
                     }

                   todo fix this
                *)
                TC.log [%here] Logging.warning @@ lazy (PP.string "ignoring binder while matching bool")
              end
              else TC.return ()
            in
            let* new_when_true =
              adorn old_when_true remaining_subpatterns true
            and* new_when_false =
              adorn old_when_false remaining_subpatterns true
            in
            TC.return begin
              PatternTree.Bool {
                when_true  = new_when_true;
                when_false = new_when_false
              }
            end
          end
        | BoolCase b -> begin
            if
              b
            then
              let* new_when_true =
                adorn old_when_true remaining_subpatterns gap_filling
              in
              TC.return begin
                PatternTree.Bool { when_true = new_when_true; when_false = old_when_false }
              end
            else
              let* new_when_false =
                adorn old_when_false remaining_subpatterns gap_filling
              in
              TC.return begin
                PatternTree.Bool { when_true = old_when_true; when_false = new_when_false }
              end
          end
        | ListCons (_, _)    -> invalid_pattern [%here]
        | ListNil            -> invalid_pattern [%here]
        | Tuple _            -> invalid_pattern [%here]
        | EnumCase _         -> invalid_pattern [%here]
        | VariantCase (_, _) -> invalid_pattern [%here]
        | Unit               -> invalid_pattern [%here]
      end

    | Enum { enum_identifier; table }, first_subpattern :: remaining_subpatterns -> begin
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

    | Variant { variant_identifier; table }, first_subpattern :: remaining_subpatterns -> begin
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
                      TC.map ~f:(Fn.uncurry Binder.unify) (List.zip_exn old_field_binders pattern_field_binders)
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

    | Binder { matched_type; binder; subtree }, first_subpattern :: remaining_subpatterns -> begin
        match first_subpattern with
        | EnumCase _ -> begin
            if
              not binder.wildcard
            then
              TC.not_yet_implemented [%here] location
            else begin
              match matched_type with
              | Enum enum_identifier -> begin
                  let* expanded_node =
                    build_enum_node
                      enum_identifier
                      subtree
                  in
                  adorn
                    expanded_node
                    tuple_subpatterns
                    gap_filling
                end
              | _ -> TC.fail [%here] "expected enum type"
            end
          end
        | VariantCase (_, _) -> begin
            if
              not binder.wildcard
            then
              TC.not_yet_implemented [%here] location
            else begin
              match matched_type with
              | Variant variant_identifier -> begin
                  let* expanded_node =
                    build_variant_node
                      variant_identifier
                      subtree
                  in
                  adorn
                    expanded_node
                    tuple_subpatterns
                    gap_filling
                end
              | _ -> TC.fail [%here] "expected enum type"
            end
          end
        | BoolCase _ -> begin
            if
              not binder.wildcard
            then
              TC.not_yet_implemented [%here] location
            else begin
              let* expanded_node =
                build_bool_node subtree
              in
              adorn
                expanded_node
                tuple_subpatterns
                gap_filling
            end
          end
        | ListCons (_, _) -> TC.not_yet_implemented [%here] location
        | ListNil -> TC.not_yet_implemented [%here] location
        | Tuple _ -> TC.not_yet_implemented [%here] location
        | Binder pattern_binder -> begin
            let* updated_binder =
              Binder.unify' binder pattern_binder
            in
            let* () =
              let message = lazy begin
                PP.format "Unifying %s %s" (FExpr.to_string @@ Binder.to_fexpr binder) (FExpr.to_string @@ Binder.to_fexpr pattern_binder)
              end
              in
              TC.log [%here] Logging.debug message
            in
            match updated_binder with
            | Some updated_binder -> begin
                let* updated_subtree =
                  adorn
                    subtree
                    remaining_subpatterns
                    true
                in
                TC.return begin
                  PatternTree.Binder {
                    matched_type;
                    binder  = updated_binder;
                    subtree = updated_subtree;
                  }
                end
              end
            | None -> begin
                let* generated_identifier =
                  let suffix =
                    Printf.sprintf
                      "-%s-%s"
                      (Ast.Identifier.to_string binder.identifier)
                      (Ast.Identifier.to_string pattern_binder.identifier)
                  in
                  TC.generate_unique_identifier ~suffix ()
                in
                let* () =
                  let message = lazy begin
                    PP.format
                      "Clashing identifiers at Sail location %s! Renaming %s and %s to %s"
                      (StringOf.Sail.location location)
                      (Ast.Identifier.to_string binder.identifier)
                      (Ast.Identifier.to_string pattern_binder.identifier)
                      (Ast.Identifier.to_string generated_identifier)
                  end
                  in
                  TC.log [%here] Logging.warning message
                in
                let renamed_subtree =
                  let renamer =
                    Ast.Renaming.create_renamer binder.identifier generated_identifier
                  in
                  PatternTree.rename renamer subtree
                in
                let renamed_body =
                  let renamer =
                    Ast.Renaming.create_renamer pattern_binder.identifier generated_identifier
                  in
                  Ast.Renaming.rename_in_statement renamer body
                in
                let* updated_renamed_subtree =
                  adorn_pattern_tree
                    location
                    renamed_subtree
                    remaining_subpatterns
                    ~gap_filling:true
                    renamed_body
                in
                TC.return begin
                  PatternTree.Binder {
                    matched_type;
                    binder  = { identifier = generated_identifier; wildcard = false };
                    subtree = updated_renamed_subtree
                  }
                end
              end
          end
        | Unit -> TC.not_yet_implemented [%here] location
      end
  in
  adorn pattern_tree tuple_subpatterns gap_filling


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
  | Bool { when_true; when_false } -> begin
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
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
                binder                 = binder.identifier;
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
              TC.map ~f:(Fn.uncurry update_pair) table_pairs
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
                        binder                 = binder.identifier;
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
                          binder                 = field_binder.identifier;
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
              TC.map table_pairs ~f:(Fn.uncurry build_statement_pair)
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

  | Binder { matched_type = element_type; binder; subtree } -> begin
      match matched_identifiers with
      | [] -> invalid_number_of_tuple_elements [%here]
      | first_matched_identifier :: remaining_matched_identifiers -> begin
          let* substatement = build_leveled_match_statements remaining_matched_identifiers subtree
          in
          if
            binder.wildcard
          then
            TC.return substatement
          else
            TC.return begin
              Ast.Statement.Let {
                binder                 = binder.identifier;
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
          TC.map ~f:(Fn.uncurry translate_pattern) pairs
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
  let* pattern =
    let* pattern =
      translate_pattern matched_type sail_pattern
    in
    let free_variables =
      Ast.Statement.free_variables body
    in
    TC.return @@ Pattern.upgrade_unused_binders_to_wildcards pattern free_variables
  in
  let S.AE_aux (unwrapped_sail_condition, _) = sail_condition
  in
  (*
     Check that the condition is simply true; we expect this to be the case if the correct rewrites have been activated
     We match each case explicitly so that in case of failure, we know immediately which exact case caused it.
  *)
  let* () =
    match unwrapped_sail_condition with
    | AE_val value -> begin
        match value with
        | AV_lit (literal, _) -> begin
            let S.L_aux (unwrapped_literal, _) = literal
            in
            match unwrapped_literal with
            | L_unit             -> TC.not_yet_implemented [%here] location
            | L_zero             -> TC.not_yet_implemented [%here] location
            | L_one              -> TC.not_yet_implemented [%here] location
            | L_true             -> TC.return ()
            | L_false            -> TC.not_yet_implemented [%here] location
            | L_num _            -> TC.not_yet_implemented [%here] location
            | L_hex _            -> TC.not_yet_implemented [%here] location
            | L_bin _            -> TC.not_yet_implemented [%here] location
            | L_string _         -> TC.not_yet_implemented [%here] location
            | L_undef            -> TC.not_yet_implemented [%here] location
            | L_real _           -> TC.not_yet_implemented [%here] location
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
        binder                 = binder_identifier;
        binding_statement_type = matched_type;
        binding_statement      = Ast.Statement.Expression (Ast.Expression.Variable (matched_identifier, matched_type));
        body_statement         = body
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
   Creates a match-statement of the form

     match matched {
       (x1, x2, ..., xn) => body
     }

   where
   - matched is a variable containing a tuple with elements of types element_types
   - x1, x2, ..., xn are generated binders
   - body is produced by calling body_builder with x1, x2, ..., xn
*)
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


let translate_tuple_match
    (location           : S.l                               )
    (matched_identifier : Ast.Identifier.t                  )
    (element_types      : Ast.Type.t list                   )
    (cases              : (Pattern.t * Ast.Statement.t) list) : Ast.Statement.t TC.t
  =
  let* cases : (Pattern.t list * Ast.Statement.t) list =
    let retrieve_tuple_subpatterns (pattern : Pattern.t) : Pattern.t list TC.t =
      match pattern with
      | Tuple subpatterns -> TC.return subpatterns
      | _                 -> TC.fail [%here] "expected a tuple pattern"
    in
    let process_case ((pattern, statement) : Pattern.t * Ast.Statement.t) : (Pattern.t list * Ast.Statement.t) TC.t =
      let* subpatterns = retrieve_tuple_subpatterns pattern
      in
      TC.return (subpatterns, statement)
    in
    TC.map ~f:process_case cases
  in
  let build_pattern_tree_using_permuter (permuter : <permute : 'a. 'a list -> 'a list>) : PatternTree.t TC.t
    =
    let* initial_tree =
      build_empty_pattern_tree
        location
        (permuter#permute element_types)
    in
    let* adorned_tree =
      TC.fold_left
        ~init:initial_tree
        ~f:(fun tree (subpatterns, statement) -> adorn_pattern_tree location tree (permuter#permute subpatterns) statement)
        cases
    in
    let* () = TC.log [%here] Logging.debug @@ lazy (PP.format "Built pattern tree with %d nodes" (PatternTree.count_nodes adorned_tree))
    in
    TC.return adorned_tree
  in
  let* optimal_tree, optimal_permuter =
    let permuters =
      List.permuters (List.length element_types)
    in
    let* triples_of_tree_size_permuter =
      let build permuter =
        let format_error (error : TC.Error.t) =
          PP.vertical [
            PP.string "Error occurred while optimizing tree; ignoring it";
            PP.string @@ TC.Error.to_string error
          ]
        in
        TC.recover begin
          let* tree =
            build_pattern_tree_using_permuter permuter
          in
          TC.return @@ Some (tree, PatternTree.count_nodes tree, permuter)
        end (fun error -> let* () = TC.log [%here] Logging.warning (lazy (format_error error))  in TC.return None)
      in
      TC.filter_map ~f:build permuters
    in
    let optimal_result =
      List.min_elt
        ~compare:(fun (_, size1, _) (_, size2, _) -> Int.compare size1 size2)
        triples_of_tree_size_permuter
    in
    match optimal_result with
    | Some (smallest_tree, smallest_size, optimal_permuter) -> begin
        let* () =
          let message = lazy begin
            let tree_sizes =
              String.concat ~sep:", " @@ List.map ~f:(Fn.compose Int.to_string Auxlib.Triple.second) triples_of_tree_size_permuter
            in
            PP.format "Pattern tree sizes: [%s], smallest is %d" tree_sizes smallest_size
          end
          in
          TC.log [%here] Logging.info message
        in
        TC.return (smallest_tree, optimal_permuter)
      end
    | None -> TC.fail [%here] "Failed to produce single pattern tree"
  in
  let builder (binder_identifiers : Ast.Identifier.t list) : Ast.Statement.t TC.t =
    build_leveled_match_statements (optimal_permuter#permute binder_identifiers) optimal_tree
  in
  let* result =
    create_tuple_match
      matched_identifier
      element_types
      builder
  in
  TC.return result


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
