open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let genblock loc label doc =
  let* doc = doc
  in
  GC.generation_block loc (PP.string label) doc


(*

    From Coq Require Import Classes.EquivDec
                            Strings.String.
    
    From stdpp Require finite.
    
    From Equations Require Import Equations.
    
    From Katamaran Require Import Base
                                  Bitvector.
  
*)
let generate_base_prelude () : PP.document GC.t =
  GC.return @@ PP.paragraphs [
    PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Coq"      ) ~import:true  [ "Classes.EquivDec"; "Strings.String" ];
    PP.annotate [%here] @@ Coq.pp_require ~from:(Some "stdpp"    ) ~import:false [ "finite" ];
    PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Equations") ~import:true  [ "Equations" ];
    PP.annotate [%here] @@ Coq.pp_require ~from:(Some "Katamaran") ~import:true  [ "Base"; "Bitvector" ];
  ]


(*

  Import ctx.notations.
  Import ctx.resolution.
  Import env.notations.
  Import stdpp.finite.

*)
let pp_imports () : PP.document GC.t =
  genblock [%here] "Imports" begin
    GC.return @@ PP.vertical [
      Coq.pp_imports ["ctx.notations"];
      Coq.pp_imports ["ctx.resolution"];
      Coq.pp_imports ["env.notations"];
      Coq.pp_imports ["stdpp.finite"];
    ]
  end


(*

   Local Open Scope string_scope.

 *)
let pp_open_string_scope () : PP.document GC.t =
  GC.return @@ PP.annotate [%here] @@ PP.vertical @@ List.map ~f:PP.string [
    "Local Open Scope string_scope."
  ]


let pp_alias_notations (alias_definitions : (Ast.Identifier.t * Ast.Definition.type_quantifier * Ast.Type.t) list) : PP.document GC.t =
  genblock [%here] "Notations for Aliases" begin
    let pp_alias_notation (id, _type_quantifier, typ) =
      let notation =
        PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.add_prefix "ty." id
      in
      let* expression =
        GC.pp_annotate [%here] @@ Nanotype.pp_nanotype typ
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_notation notation expression
    in
    GC.block begin
      let* notations = GC.map ~f:pp_alias_notation alias_definitions
      in
      GC.return @@ PP.annotate [%here] @@ PP.vertical notations
    end
  end


(*

      #[export] Instance typedeclkit : TypeDeclKit :=
        {|
           enumi := Enums;
           unioni := Unions;
           recordi := Records;
        |}.

 *)
let pp_typedeclkit () : PP.document GC.t =
  genblock [%here] "typedeclkit" begin
    GC.return @@ PP.vertical @@ List.map ~f:PP.string [
      "#[export] Instance typedeclkit : TypeDeclKit :=";
      "  {|";
      "     enumi   := Enums;";
      "     unioni  := Unions;";
      "     recordi := Records;";
      "  |}.";
    ]
  end


(*

   Helper function to define denote functions, such as enum_denote.

 *)
let pp_denote_function
    ?(scope                : PP.document option               = None)
    ~(denotations          : (PP.document * PP.document) list       )
    ~(parameter_identifier : PP.document                            )
    ~(tag_type_identifier  : PP.document                            )
    ~(function_identifier  : PP.document                            ) () : PP.document GC.t
  =
  let identifier  = PP.annotate [%here] @@ function_identifier
  and parameters  = [
      (
        PP.annotate [%here] @@ parameter_identifier,
        Some (PP.annotate [%here] @@ tag_type_identifier)
      )
    ]
  and result_type =
    Some (PP.annotate [%here] @@ PP.string "Set")
  and body =
    let matched_expression = PP.annotate [%here] @@ parameter_identifier
    and cases              = denotations
    in
    PP.annotate [%here] @@ Coq.pp_match ~scope matched_expression cases
  in
  GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type body


(*

   Generates code for enum_denote, which maps enum names to their types.

      Definition enum_denote (e : Enums) : Set :=
        match e with
        | permission => Permission
        | regname    => RegName
        end.

 *)
let pp_enum_denote (enum_definitions : Ast.Definition.Type.Enum.t list) : PP.document GC.t =
  genblock [%here] "Enum Denote" begin
    let denotations =
      let regname_denotation =
        (
          PP.annotate [%here] @@ Identifier.pp Registers.regname_tag,
          PP.annotate [%here] @@ Identifier.pp Registers.regname_inductive_type_identifier
        )
      in
      let enum_identifiers =
        List.map ~f:(fun enum_definition -> enum_definition.identifier) enum_definitions
      in
      let denotation_pair_for enum_identifier =
        (
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_enum_name enum_identifier,
          PP.annotate [%here] @@ Identifier.pp enum_identifier
        )
      in
      regname_denotation :: List.map ~f:denotation_pair_for enum_identifiers
    in
    let parameter_identifier = PP.annotate [%here] @@ PP.string "e"
    and tag_type_identifier  = PP.annotate [%here] @@ PP.string "Enums"
    and function_identifier  = PP.annotate [%here] @@ PP.string "enum_denote"
    in
    GC.block begin
        GC.pp_annotate [%here] @@ pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier ()
      end
  end


(*

   Generates code for union_denote, which maps union names to their types.

     Definition union_denote (U : Unions) : Set :=
       match U with
       | instruction => Instruction
       end.

 *)
let pp_union_denote (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document GC.t =
  genblock [%here] "Union Denote" begin
    let denotations =
      let variant_identifiers =
        List.map ~f:(fun variant_definition -> variant_definition.identifier) variant_definitions
      in
      let denotation_pair_for variant_identifier =
        (
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_name variant_identifier,
          PP.annotate [%here] @@ Identifier.pp variant_identifier
        )
      in
      List.map ~f:denotation_pair_for variant_identifiers

    and parameter_identifier = PP.annotate [%here] @@ PP.string "u"
    and tag_type_identifier  = PP.annotate [%here] @@ PP.string "Unions"
    and function_identifier  = PP.annotate [%here] @@ PP.string "union_denote"
    in
      GC.pp_annotate [%here] @@ pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier ()
  end


(*

   Generates code for record_denote, which maps record names to their types.

     Definition record_denote (R : Records) : Set :=
       match R with
       | capability => Capability
       end.

 *)
let pp_record_denote (record_definitions : Ast.Definition.Type.Record.t list) : PP.document GC.t =
  genblock [%here] "Record Denote" begin
    let denotations =
      let record_identifiers =
        List.map ~f:(fun record_definition -> record_definition.identifier) record_definitions
      in
      let denotation_pair_for record_identifier =
        (
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_record_name record_identifier,
          PP.annotate [%here] @@ Identifier.pp record_identifier
        )
      in
      List.map ~f:denotation_pair_for record_identifiers

    and parameter_identifier = PP.annotate [%here] @@ PP.string "r"
    and tag_type_identifier  = PP.annotate [%here] @@ PP.string "Records"
    and function_identifier  = PP.annotate [%here] @@ PP.string "record_denote"
    in
    GC.block begin
      GC.pp_annotate [%here] @@ pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier ()
    end
  end

(*

  #[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=
    {|
       enumt := enum_denote;
       uniont := union_denote;
       recordt := record_denote;
    |}.

 *)
let pp_typedenotekit () : PP.document GC.t =
  genblock [%here] "typedenotekit" begin
    let coq_lines = [
      "#[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=";
      "  {|";
      "     enumt := enum_denote;";
      "     uniont := union_denote;";
      "     recordt := record_denote;";
      "  |}.";
    ]
    in
    GC.return @@ PP.annotate [%here] @@ PP.(vertical @@ List.map ~f:string coq_lines)
  end


(*

   Generates code for union_constructor.

     Definition union_constructor (U : Unions) : Set :=
       match U with
       | instruction => InstructionConstructor
       end.

 *)
let pp_union_constructor (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document GC.t =
  genblock [%here] "Union Constructor" begin
    let denotations =
      let variant_identifiers =
        List.map ~f:(fun variant_definition -> variant_definition.identifier) variant_definitions
      in
      let denotation_pair_for variant_identifier =
        (
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_name variant_identifier,
          PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_identifier
        )
      in
      List.map ~f:denotation_pair_for variant_identifiers

    and parameter_identifier = PP.annotate [%here] @@ PP.string "u"
    and tag_type_identifier  = PP.annotate [%here] @@ PP.string "Unions"
    and function_identifier  = PP.annotate [%here] @@ PP.string "union_constructor"
    in
    GC.block begin
      GC.pp_annotate [%here] @@ pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier ()
    end
  end

(*

   Generates code for union_constructor_type.

     Definition union_constructor_type (U : Unions) : union_constructor U -> Ty :=
       match U with
       | instruction => fun K =>
         match K with
         | kjalr_cap      => ty.prod ty.dst ty.src
         | kcjalr         => ty.tuple [ty.dst; ty.src; ty.int]
         | kcjal          => ty.prod ty.dst ty.int
         | kbne           => ty.tuple [ty.src; ty.src; ty.int]
         | kld            => ty.tuple [ty.dst; ty.src; ty.int]
         | ksd            => ty.tuple [ty.src; ty.src; ty.int]
         | kaddi          => ty.tuple [ty.dst; ty.src; ty.int]
         | kadd           => ty.tuple [ty.dst; ty.src; ty.src]
         | ksub           => ty.tuple [ty.dst; ty.src; ty.src]
         | kslt           => ty.tuple [ty.dst; ty.src; ty.src]
         | kslti          => ty.tuple [ty.dst; ty.src; ty.int]
         | ksltu          => ty.tuple [ty.dst; ty.src; ty.src]
         | ksltiu         => ty.tuple [ty.dst; ty.src; ty.int]
         | kcmove         => ty.prod ty.dst ty.src
         | kcincoffset    => ty.tuple [ty.dst; ty.src; ty.src]
         | kcandperm      => ty.tuple [ty.dst; ty.src; ty.src]
         | kcsetbounds    => ty.tuple [ty.dst; ty.src; ty.src]
         | kcsetboundsimm => ty.tuple [ty.dst; ty.src; ty.int]
         | kcgettag       => ty.prod ty.dst ty.src
         | kcgetperm      => ty.prod ty.dst ty.src
         | kcgetbase      => ty.prod ty.dst ty.src
         | kcgetlen       => ty.prod ty.dst ty.src
         | kcgetaddr      => ty.prod ty.dst ty.src
         | kfail          => ty.unit
         | kret           => ty.unit
         end
       end.

 *)
let pp_union_constructor_type (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document GC.t =
  genblock [%here] "Union Constructor Type" begin
    let identifier  = PP.annotate [%here] @@ PP.string "union_constructor_type"
    and parameters  = [
        (
          PP.annotate [%here] @@ PP.string "u",
          Some (PP.annotate [%here] @@ PP.string "Unions")
        )
      ]
    and result_type =
      Some (PP.annotate [%here] @@ PP.string "union_constructor u -> Ty")
    in
    let* body =
      let matched_expression = PP.annotate [%here] @@ PP.string "u"
      in
      let* cases =
        let pp_variant_case (variant_definition : Ast.Definition.Type.Variant.t) =
          let* match_constructor_cases =
            let* constructor_cases =
              let pp_constructor_case (constructor : Ast.Definition.Type.Variant.constructor) =
                let (constructor_identifier, constructor_field_types) = constructor
                in
                let pp_constructor_tag =
                  PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_constructor_name constructor_identifier
                in
                let* pp_constructor_field_types =
                  let packed_type =
                    match constructor_field_types with
                    | []     -> Ast.Type.Unit
                    | [x]    -> x
                    | [x; y] -> Ast.Type.Product (x, y)
                    | xs     -> Ast.Type.Tuple xs
                  in
                  GC.pp_annotate [%here] @@ Nanotype.pp_nanotype packed_type
                in
                GC.return @@ (pp_constructor_tag, pp_constructor_field_types)
              in
              GC.map ~f:pp_constructor_case variant_definition.constructors
            in
            GC.return @@ PP.annotate [%here] @@ Coq.pp_match (PP.string "k") constructor_cases
          in
          GC.return begin
              (
                PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_name variant_definition.identifier,
                PP.annotate [%here] @@ PP.(horizontal [ string "fun k => "; match_constructor_cases ])
              )
            end
        in
        GC.map ~f:pp_variant_case variant_definitions
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_match matched_expression cases
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type body
  end


(*

  #[export] Instance eqdec_enum_denote E : EqDec (enum_denote E) :=
    ltac:(destruct E; auto with typeclass_instances).
  #[export] Instance finite_enum_denote E : finite.Finite (enum_denote E) :=
    ltac:(destruct E; auto with typeclass_instances).
  #[export] Instance eqdec_union_denote U : EqDec (union_denote U) :=
    ltac:(destruct U; cbn; auto with typeclass_instances).
  #[export] Instance eqdec_union_constructor U : EqDec (union_constructor U) :=
    ltac:(destruct U; cbn; auto with typeclass_instances).
  #[export] Instance finite_union_constructor U : finite.Finite (union_constructor U) :=
    ltac:(destruct U; cbn; auto with typeclass_instances).
  #[export] Instance eqdec_record_denote R : EqDec (record_denote R) :=
    ltac:(destruct R; auto with typeclass_instances).

 *)
let pp_eqdec_and_finite_instances () : PP.document GC.t =
  let coq_lines = [
      "#[export] Instance eqdec_enum_denote E : EqDec (enum_denote E) :=";
      "  ltac:(destruct E; auto with typeclass_instances).";
      "#[export] Instance finite_enum_denote E : finite.Finite (enum_denote E) :=";
      "  ltac:(destruct E; auto with typeclass_instances).";
      "#[export] Instance eqdec_union_denote U : EqDec (union_denote U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance eqdec_union_constructor U : EqDec (union_constructor U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance finite_union_constructor U : finite.Finite (union_constructor U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance eqdec_record_denote R : EqDec (record_denote R) :=";
      "  ltac:(destruct R; auto with typeclass_instances).";
    ]
  in
  genblock [%here] "EqDec/Finite Instances" begin
    GC.return @@ PP.(vertical @@ List.map ~f:PP.string coq_lines)
  end


(* Helper function for pp_union_fold and pp_union_unfold *)
let pp_match_variant_constructors
    ~(matched_identifier  : Ast.Identifier.t                  )
    ~(variant_definitions : Ast.Definition.Type.Variant.t list)
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> (PP.document * PP.document) GC.t) : PP.document GC.t
  =
  let variant_case_handler (variant_definition : Ast.Definition.Type.Variant.t) : (PP.document * PP.document) GC.t =
    let parameter_identifier = Ast.Identifier.mk "Kv"
    in
    let pattern =
      PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_name variant_definition.identifier
    in
    let* expression =
      let* lambda_body =
        Types.Variants.generate_constructor_match
          ~matched_identifier:parameter_identifier
          ~variant_definition
          ~constructor_case_handler
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_lambda (Identifier.pp parameter_identifier) lambda_body
    in
    GC.return (pattern, expression)
  in
  GC.pp_annotate [%here] @@ Types.Variants.generate_tag_match ~matched_identifier ~variant_definitions ~variant_case_handler


(*

   Generates code for union_fold.

     Definition union_fold (U : unioni) : { K & Val (union_constructor_type U K) } -> uniont U :=
       match U with
       | instruction => fun Kv =>
         match Kv with
         | existT kjalr_cap      (cd , cs)              => jalr_cap      cd  cs
         | existT kcjalr         (tt , cd , cs , imm)   => cjalr         cd  cs  imm
         | existT kcjal          (cd , imm)             => cjal          cd  imm
         | existT kbne           (tt , rs1 , rs2 , imm) => bne           rs1 rs2 imm
         | existT kld            (tt , cd , cs , imm)   => ld            cd  cs  imm
         | existT ksd            (tt , rs1 , rs2, imm)  => sd            rs1 rs2 imm
         | existT kaddi          (tt , rd , rs , imm)   => addi          rd  rs  imm
         | existT kadd           (tt , rd , rs1 , rs2)  => add           rd  rs1 rs2
         | existT ksub           (tt , rd , rs1 , rs2)  => sub           rd  rs1 rs2
         | existT kslt           (tt , rd , rs1 , rs2)  => slt           rd  rs1 rs2
         | existT kslti          (tt , rd , rs , imm)   => slti          rd  rs  imm
         | existT ksltu          (tt , rd , rs1 , rs2)  => sltu          rd  rs1 rs2
         | existT ksltiu         (tt , rd , rs , imm)   => sltiu         rd  rs  imm
         | existT kcmove         (cd , cs)              => cmove         cd  cs
         | existT kcincoffset    (tt , cd , cs , rs)    => cincoffset    cd  cs  rs
         | existT kcandperm      (tt , cd , cs , rs)    => candperm      cd  cs  rs
         | existT kcsetbounds    (tt , cd , cs , rs)    => csetbounds    cd  cs  rs
         | existT kcsetboundsimm (tt , cd , cs , imm)   => csetboundsimm cd  cs  imm
         | existT kcgettag       (rd , cs)              => cgettag       rd  cs
         | existT kcgetperm      (rd , cs)              => cgetperm      rd  cs
         | existT kcgetbase      (rd , cs)              => cgetbase      rd  cs
         | existT kcgetlen       (rd , cs)              => cgetlen       rd  cs
         | existT kcgetaddr      (rd , cs)              => cgetaddr      rd  cs
         | existT kfail          tt                     => fail
         | existT kret           tt                     => ret
         end
       end.

 *)
let pp_union_fold (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document GC.t =
  genblock [%here] "Union Fold" begin
    let result =
      let identifier = PP.annotate [%here] @@ PP.string "union_fold"
      and parameters = [
          (
            PP.annotate [%here] @@ PP.string "U",
            Some (PP.annotate [%here] @@ PP.string "unioni"))
        ]
      and result_type =
        Some (PP.annotate [%here] @@ PP.string "{ K & Val (union_constructor_type U K) } -> uniont U")
      in
      let* contents =
        let matched_identifier = Ast.Identifier.mk "U"
        in
        let constructor_case_handler (variant_constructor : Ast.Definition.Type.Variant.constructor) : (PP.document * PP.document) GC.t =
          let (constructor_identifier, constructor_field_types) = variant_constructor
          in
          let field_variables =
            let generate_identifier index =
              PP.annotate [%here] @@ Identifier.pp @@ Configuration.tag_as_generated @@ Ast.Identifier.mk @@ Int.to_string index
            and indices =
              let n_fields = List.length constructor_field_types
              in
              List.range ~start:`inclusive ~stop:`inclusive 1 n_fields
            in
            List.map ~f:generate_identifier indices
          in
          let pattern =
            let fields =
              let tt = PP.annotate [%here] @@ PP.string "tt"
              in
              match field_variables with
              | []     -> tt
              | [t]    -> t
              | [_; _] -> PP.(annotate [%here] @@ surround parens @@ separate_horizontally ~separator:(horizontal [comma; space]) field_variables)
              | _      -> PP.(annotate [%here] @@ surround parens @@ separate_horizontally ~separator:(horizontal [comma; space]) (tt :: field_variables))
            in
            let parts = [
              PP.annotate [%here] @@ PP.string "existT";
              PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_variant_constructor_name constructor_identifier;
              fields
            ]
            in
            PP.(annotate [%here] @@ separate_horizontally ~separator:space parts)
          and expression =
            PP.(annotate [%here] @@ separate_horizontally ~separator:space @@ Identifier.pp constructor_identifier :: field_variables)
          in
          GC.return (pattern, expression)
        in
        GC.pp_annotate [%here] @@ pp_match_variant_constructors ~variant_definitions ~matched_identifier ~constructor_case_handler
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type contents
    in
    GC.block result
  end


(*

   Generates code for union_unfold.

     Definition union_unfold (U : unioni) : uniont U -> { K & Val (union_constructor_type U K) } :=
       match U with
       | instruction => fun Kv =>
         match Kv with
         | jalr_cap      cd  cs      => existT kjalr_cap      (cd , cs)
         | cjalr         cd  cs  imm => existT kcjalr         (tt , cd , cs , imm)
         | cjal          cd  imm     => existT kcjal          (cd , imm)
         | bne           rs1 rs2 imm => existT kbne           (tt , rs1 , rs2 , imm)
         | ld            cd  cs  imm => existT kld            (tt , cd , cs , imm)
         | sd            rs1 rs2 imm => existT ksd            (tt , rs1 , rs2 , imm)
         | addi          rd  rs  imm => existT kaddi          (tt , rd , rs , imm)
         | add           rd  rs1 rs2 => existT kadd           (tt , rd , rs1 , rs2)
         | sub           rd  rs1 rs2 => existT ksub           (tt , rd , rs1 , rs2)
         | slt           rd  rs1 rs2 => existT kslt           (tt , rd , rs1 , rs2)
         | slti          rd  rs  imm => existT kslti          (tt , rd , rs , imm)
         | sltu          rd  rs1 rs2 => existT ksltu          (tt , rd , rs1 , rs2)
         | sltiu         rd  rs  imm => existT ksltiu         (tt , rd , rs , imm)
         | cmove         cd  cs      => existT kcmove         (cd , cs)
         | cincoffset    cd  cs  rs  => existT kcincoffset    (tt , cd , cs , rs)
         | candperm      cd  cs  rs  => existT kcandperm      (tt , cd , cs , rs)
         | csetbounds    cd  cs  rs  => existT kcsetbounds    (tt, cd , cs , rs)
         | csetboundsimm cd  cs  imm => existT kcsetboundsimm (tt, cd , cs , imm)
         | cgettag       rd  cs      => existT kcgettag       (rd , cs)
         | cgetperm      rd  cs      => existT kcgetperm      (rd , cs)
         | cgetbase      rd  cs      => existT kcgetbase      (rd , cs)
         | cgetlen       rd  cs      => existT kcgetlen       (rd , cs)
         | cgetaddr      rd  cs      => existT kcgetaddr      (rd , cs)
         | fail                      => existT kfail          tt
         | ret                       => existT kret           tt
         end
       end.

*)
let pp_union_unfold (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document GC.t =
  genblock [%here] "Union Unfold" begin
    let result =
      let identifier = PP.annotate [%here] @@ PP.string "union_unfold"
      and parameters =
        [
          (
            PP.annotate [%here] @@ PP.string "U",
            Some (PP.annotate [%here] @@ PP.string "unioni")
          )
        ]
      and result_type =
        Some (PP.annotate [%here] @@ PP.string "uniont U -> { K & Val (union_constructor_type U K) }")
      in
      let* contents =
        let matched_identifier = Ast.Identifier.mk "U"
        in
        let constructor_case_handler (constructor_identifier, field_types) : (PP.document * PP.document) GC.t =
          let field_names =
            let generate_identifier index =
              PP.annotate [%here] @@ Identifier.pp @@ Configuration.tag_as_generated @@ Ast.Identifier.mk @@ Int.to_string index
            and indices =
              let n_fields = List.length field_types
              in
              List.range ~start:`inclusive ~stop:`inclusive 1 n_fields
            in
            List.map ~f:generate_identifier indices
          in
          let pattern = PP.annotate [%here] @@ PP.separate_horizontally ~separator:PP.space @@ Auxlib.build_list @@ fun { add; addall; _ } -> begin
              add    @@ Identifier.pp constructor_identifier;
              addall @@ field_names
            end
          and expression =
            let tuple =
              let tt = PP.annotate [%here] @@ PP.string "tt"
              in
              match field_names with
              | []     -> tt
              | [t]    -> t
              | [_; _] -> PP.annotate [%here] @@ PP.(surround parens) @@ PP.separate_horizontally ~separator:(PP.string ", ") field_names
              | _      -> PP.annotate [%here] @@ PP.(surround parens) @@ PP.separate_horizontally ~separator:(PP.string ", ") (tt :: field_names)
            in
            PP.(separate_horizontally ~separator:space [
                string "existT";
                Identifier.pp @@ Identifier.reified_variant_constructor_name constructor_identifier;
                tuple
              ])
          in
          GC.return (pattern, expression)
        in
        GC.pp_annotate [%here] @@ pp_match_variant_constructors ~variant_definitions ~matched_identifier ~constructor_case_handler
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type contents
    in
    GC.block result
  end


(*

   Generates code for record_field_type.

     Definition record_field_type (R : recordi) : NCtx string Ty :=
       match R with
       | capability => [ "cap_permission" ∷ ty.perm;
                         "cap_begin"      ∷ ty.addr;
                         "cap_end"        ∷ ty.addr;
                         "cap_cursor"     ∷ ty.addr
                       ]
       end.

 *)
let pp_record_field_type (record_definitions : Ast.Definition.Type.Record.t list) : PP.document GC.t =
  genblock [%here] "Record Field Type" begin
    let result =
      let matched_identifier = Ast.Identifier.mk "R"
      in
      let identifier = PP.annotate [%here] @@ PP.string "record_field_type"
      and parameters =
        [
          (
            PP.annotate [%here] @@ Identifier.pp matched_identifier,
            Some (PP.annotate [%here] @@ PP.string "recordi")
          )
        ]
      and result_type = Some (PP.annotate [%here] @@ PP.string "NCtx string Ty")
      in
      let* contents =
        let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : (PP.document * PP.document) GC.t =
          let pattern =
            PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_record_name record_definition.identifier
          in
          let* expression =
            let pp_field (field_identifier, field_type) =
              let id = PP.annotate [%here] @@ Identifier.pp field_identifier
              in
              let* t = GC.pp_annotate [%here] @@ Nanotype.pp_nanotype field_type
              in
              GC.return @@ PP.(annotate [%here] @@ separate_horizontally ~separator:space [ surround dquotes id; string "::"; t ])
            in
            let* fields = GC.map ~f:pp_field record_definition.fields
            in
            GC.return @@ PP.annotate [%here] @@ Coq.pp_list fields
          in
          GC.return (pattern, expression)
        in
        GC.pp_annotate [%here] @@ Types.Records.generate_tag_match ~matched_identifier ~record_definitions ~record_case_handler ()
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type contents
    in
    GC.block result
  end


(*

   Generates code for record_fold.

     Definition record_fold (R : recordi) : NamedEnv Val (record_field_type R) -> recordt R :=
       match R with
       | capability =>

           fun fields =>
           MkCap
             fields.[??"cap_permission"]
             fields.[??"cap_begin"]
             fields.[??"cap_end"]
             fields.[??"cap_cursor"]
       end%exp.

 *)
let pp_record_fold (record_definitions : Ast.Definition.Type.Record.t list) : PP.document GC.t =
  genblock [%here] "Record Fold" begin
    let result =
      let scope =
        Some "exp"
      and matched_identifier =
        Ast.Identifier.mk "R"
      in
      let identifier =
        PP.annotate [%here] @@ PP.string "record_fold"
      and parameters =
        [
          (
            PP.annotate [%here] @@ Identifier.pp matched_identifier,
            Some (PP.annotate [%here] @@ PP.string "recordi"))
        ]
      and result_type =
        let parameter_type =
          PP.annotate [%here] @@ Coq.pp_application (PP.string "NamedEnv") [
              PP.annotate [%here] @@ PP.string "Val";
              PP.annotate [%here] @@ PP.(surround parens) @@ Coq.pp_application (PP.string "record_field_type") [ Identifier.pp matched_identifier ];
            ]
        and result_type =
          PP.annotate [%here] @@ Coq.pp_application (PP.string "recordt") [ Identifier.pp matched_identifier ]
        in
        Some (PP.annotate [%here] @@ Coq.pp_function_type [ parameter_type ] result_type)
      in
      let* contents =
        let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : (PP.document * PP.document) GC.t =
          let pattern =
            PP.annotate [%here] @@ Identifier.pp @@ Identifier.reified_record_name record_definition.identifier
          and expression =
            let lambda_parameter = Ast.Identifier.mk "fields"
            in
            let lambda_body =
              let constructor_identifier =
                Types.Records.derive_constructor_from_identifier record_definition.identifier
              in
              let arguments =
                let f (field_identifier, _field_type) =
                  PP.annotate [%here] @@ PP.string @@ Printf.sprintf "%s.[??\"%s\"]"
                    (Ast.Identifier.string_of lambda_parameter)
                    (Ast.Identifier.string_of field_identifier)
                in
                List.map ~f record_definition.fields
              in
              PP.annotate [%here] @@ Coq.pp_application (Identifier.pp constructor_identifier) arguments
            in
            PP.annotate [%here] @@ Coq.pp_lambda (Identifier.pp lambda_parameter) lambda_body
          in
          GC.return (pattern, expression)
        in
        GC.pp_annotate [%here] @@ Types.Records.generate_tag_match ~scope ~matched_identifier ~record_definitions ~record_case_handler ()
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type contents
    in
    GC.block result
  end

(*

   Generates code for record_unfold.

     Definition record_unfold (R : recordi) : recordt R -> NamedEnv Val (record_field_type R) :=
       match R  with
       | capability =>
         fun c=>
           env.nil
             ► ("cap_permission" ∷ ty.perm ↦ cap_permission c)
             ► ("cap_begin"      ∷ ty.addr ↦ cap_begin c)
             ► ("cap_end"        ∷ ty.addr ↦ cap_end c)
             ► ("cap_cursor"     ∷ ty.addr ↦ cap_cursor c)
       end%env.

*)
let pp_record_unfold (record_definitions : Ast.Definition.Type.Record.t list) : PP.document GC.t =
  genblock [%here] "Record Unfold" begin
    let result =
      let scope = Some "env"
      and matched_identifier = Ast.Identifier.mk "R"
      in
      let identifier = PP.annotate [%here] @@ PP.string "record_unfold"
      and parameters =
        [
          (
            PP.annotate [%here] @@ Identifier.pp matched_identifier,
            Some (PP.annotate [%here] @@ PP.string "recordi")
          )
        ]
      and result_type =
        (* recordt R -> NamedEnv Val (record_field_type R) *)
        let parameter_type =
          PP.annotate [%here] @@ Coq.pp_application (PP.string "recordt") [ Identifier.pp matched_identifier ]
        and return_type =
          PP.annotate [%here] @@ Coq.pp_application (PP.string "NamedEnv") [
              PP.string "Val";
              PP.(surround parens) @@ Coq.pp_application (PP.string "record_field_type") [ Identifier.pp matched_identifier ]
            ]
        in
        Some (PP.annotate [%here] @@ Coq.pp_function_type [ parameter_type ] return_type)
      in
      let* contents =
        let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : (PP.document * PP.document) GC.t =
          let pattern =
            Identifier.pp @@ Identifier.reified_record_name record_definition.identifier
          in
          let* expression =
            let lambda_parameter = Ast.Identifier.mk "r"
            in
            let* lambda_body =
              let* bindings =
                let make_binding (field_identifier, field_type) =
                  let* pp_field_type =
                    GC.pp_annotate [%here] @@ Nanotype.pp_nanotype field_type
                  in
                  GC.return @@ PP.separate_horizontally ~separator:PP.space [
                    PP.annotate [%here] @@ PP.string "►";
                    PP.annotate [%here] @@ PP.(surround parens) @@ PP.separate_horizontally ~separator:PP.space [
                      PP.annotate [%here] @@ PP.(surround dquotes) @@ Identifier.pp field_identifier;
                      PP.annotate [%here] @@ PP.string "∷";
                      PP.annotate [%here] @@ pp_field_type;
                      PP.annotate [%here] @@ PP.string "↦";
                      PP.annotate [%here] @@ Identifier.pp field_identifier;
                      PP.annotate [%here] @@ Identifier.pp lambda_parameter
                    ]
                  ]
                in
                GC.map ~f:make_binding record_definition.fields
              in
              GC.return PP.(vertical [
                                string "env.nil";
                                indent @@ vertical bindings ])
            in
            GC.return @@ PP.annotate [%here] @@ Coq.pp_lambda (Identifier.pp lambda_parameter) lambda_body
          in
          GC.return (pattern, expression)
        in
        GC.pp_annotate [%here] @@ Types.Records.generate_tag_match ~scope ~matched_identifier ~record_definitions ~record_case_handler ()
      in
      GC.return @@ PP.annotate [%here] @@ Coq.pp_definition ~identifier ~parameters ~result_type contents
    in
    GC.block result
  end


(*

  #[export,refine] Instance typedefkit : TypeDefKit typedenotekit :=
    {| unionk           := union_constructor;
       unionk_ty        := union_constructor_type;
       recordf          := string;
       recordf_ty       := record_field_type;
       unionv_fold      := union_fold;
       unionv_unfold    := union_unfold;
       recordv_fold     := record_fold;
       recordv_unfold   := record_unfold;
    |}.
  Proof.
    - abstract (now intros [] []).
    - abstract (intros [] [[] x]; cbn in x;
                repeat
                  match goal with
                  | x: unit     |- _ => destruct x
                  | x: prod _ _ |- _ => destruct x
                  end; auto).
    - abstract (now intros [] []).
    - abstract (intros []; now apply env.Forall_forall).
  Defined.

 *)
let pp_typedefkit_instance () : PP.document GC.t =
  genblock [%here] "Typedefkit" begin
    GC.return @@ PP.vertical @@ List.map ~f:PP.string [
      "#[export,refine] Instance typedefkit : TypeDefKit typedenotekit :=";
      "  {| unionk           := union_constructor;";
      "     unionk_ty        := union_constructor_type;";
      "     recordf          := string;";
      "     recordf_ty       := record_field_type;";
      "     unionv_fold      := union_fold;";
      "     unionv_unfold    := union_unfold;";
      "     recordv_fold     := record_fold;";
      "     recordv_unfold   := record_unfold;";
      "  |}.";
      "Proof.";
      "  - abstract (now intros [] []).";
      "  - abstract (intros [] [[] x]; cbn in x;";
      "              repeat";
      "                match goal with";
      "                | x: unit     |- _ => destruct x";
      "                | x: prod _ _ |- _ => destruct x";
      "                end; auto).";
      "  - abstract (now intros [] []).";
      "  - abstract (intros []; now apply env.Forall_forall).";
      "Defined.";
    ]
  end


(*

  Canonical typedeclkit.
  Canonical typedenotekit.
  Canonical typedefkit.

*)
let pp_canonicals () : PP.document GC.t =
  let identifiers =
    List.map ~f:Ast.Identifier.mk [ "typedeclkit"; "typedenotekit"; "typedefkit" ]
  in
  genblock [%here] "Canonicals" begin
    GC.return @@ PP.vertical @@ List.map ~f:Coq.pp_canonical identifiers
  end


(*

  #[export] Instance varkit : VarKit := DefaultVarKit.

*)
let pp_varkit_instance () : PP.document GC.t =
  genblock [%here] "Varkit" begin
    GC.return @@ PP.string "#[export] Instance varkit : VarKit := DefaultVarKit."
  end


(*

  Section RegDeclKit.

    Inductive Reg : Ty -> Set :=
    | pc   : Reg ty.cap
    | reg1 : Reg ty.word
    | reg2 : Reg ty.word
    | reg3 : Reg ty.word.

    Section TransparentObligations.
      Local Set Transparent Obligations.
      Derive Signature NoConfusion NoConfusionHom EqDec for Reg.
    End TransparentObligations.

    Definition 𝑹𝑬𝑮 : Ty -> Set := Reg.
    #[export] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=
      sigma_eqdec _ _.

    Local Obligation Tactic :=
      finite_from_eqdec.

    #[export,program] Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=
      {| enum := [ existT _ pc; existT _ reg1; existT _ reg2; existT _ reg3 ] |}.

  End RegDeclKit.

*)
let pp_regdeclkit register_definitions : PP.document GC.t =
  genblock [%here] "RegDeclKit" begin
    Registers.pp_regdeclkit register_definitions
  end


(*

  Section MemoryModel.
   Definition Memory := Z -> Z. (* TODO *)
  End MemoryModel.


*)
let pp_memory_model () : PP.document GC.t =
  genblock [%here] "Memory Model" begin
    let identifier =
      Ast.Identifier.mk "MemoryModel"
    in
    let* content =
      GC.block begin
          let* () = GC.add_comment @@ PP.string "TODO"
          in
          GC.return begin
              PP.annotate [%here] begin
                  Coq.pp_definition
                    ~identifier:(PP.string "Memory")
                    (PP.string "Z -> Z")
                end
            end
        end
    in
    GC.return @@ Coq.pp_section identifier content
    end


(*

   Include BaseMixin.

*)
let pp_include_mixin () : PP.document GC.t =
  genblock [%here] "Base Mixin" begin
    GC.return @@ Coq.pp_include_module (PP.string "BaseMixin")
  end


let pp_base_module (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.document GC.t =
  let enum_definitions =
    List.map ~f:snd Ast.Definition.Select.(select (type_definition of_enum) definitions)
  and variant_definitions =
    List.map ~f:snd Ast.Definition.Select.(select (type_definition of_variant) definitions)
  and record_definitions =
    List.map ~f:snd Ast.Definition.Select.(select (type_definition of_record) definitions)
  and alias_definitions =
    List.map ~f:snd Ast.Definition.Select.(select (type_definition of_alias) definitions)
  and register_definitions =
    Ast.Definition.Select.(select register_definition definitions)
  in
  begin
    let base_module_name = Configuration.(get base_name)
    and flag = Coq.Export
    and includes = [ "Base" ]
    in
    let* contents =
      let* sections = GC.sequence [
        GC.pp_annotate [%here] @@ pp_imports ();
        GC.pp_annotate [%here] @@ pp_open_string_scope ();
        GC.pp_annotate [%here] @@ pp_alias_notations alias_definitions;
        GC.pp_annotate [%here] @@ pp_typedeclkit ();
        GC.pp_annotate [%here] @@ pp_enum_denote enum_definitions;
        GC.pp_annotate [%here] @@ pp_union_denote variant_definitions;
        GC.pp_annotate [%here] @@ pp_record_denote record_definitions;
        GC.pp_annotate [%here] @@ pp_typedenotekit ();
        GC.pp_annotate [%here] @@ pp_union_constructor variant_definitions;
        GC.pp_annotate [%here] @@ pp_union_constructor_type variant_definitions;
        GC.pp_annotate [%here] @@ pp_eqdec_and_finite_instances ();
        GC.pp_annotate [%here] @@ pp_union_fold variant_definitions;
        GC.pp_annotate [%here] @@ pp_union_unfold variant_definitions;
        GC.pp_annotate [%here] @@ pp_record_field_type record_definitions;
        GC.pp_annotate [%here] @@ pp_record_fold record_definitions;
        GC.pp_annotate [%here] @@ pp_record_unfold record_definitions;
        GC.pp_annotate [%here] @@ pp_typedefkit_instance ();
        GC.pp_annotate [%here] @@ pp_canonicals ();
        GC.pp_annotate [%here] @@ pp_varkit_instance ();
        GC.pp_annotate [%here] @@ pp_regdeclkit register_definitions;
        GC.pp_annotate [%here] @@ pp_memory_model ();
        GC.pp_annotate [%here] @@ pp_include_mixin ();
      ]
      in
      GC.return @@ PP.annotate [%here] @@ PP.paragraphs sections
    in
    GC.return @@ PP.annotate [%here] @@ Coq.pp_module ~flag ~includes base_module_name contents
  end
