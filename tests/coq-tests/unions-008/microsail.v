(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:47 Prelude *)
  From Coq Require Import Classes.EquivDec
                          Strings.String.
  From stdpp Require finite.
  From Equations Require Import Equations.
  Require Import Katamaran.Base.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:47 Prelude *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:63 Register Definitions *)
  (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:78 Regname Inductive Type *)
    Inductive RegName : Set :=
    .
  (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:78 Regname Inductive Type *)
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:63 Register Definitions *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:57 Translated Type Definitions *)
  (* struct MyStruct = {x : int} *)
  Record MyStruct : Set :=
    MkMyStruct
      {
        x : Z;
      }.

  (* union Foo = {FooX : MyStruct} *)
  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for Foo *)
    Inductive Foo : Set :=
      | FooX : MyStruct -> Foo
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:44 Union Inductive Type for Foo *)

  (* <<<<< nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forFoo *)
    Inductive FooConstructor : Set :=
      | Kfoox
    .
  (* >>>>> nanosail/NanosailToMicrosail/Types/Variants.ml:64 Constructors Inductive Type forFoo *)
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:57 Translated Type Definitions *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:69 Enum Tags *)
  Inductive Enums : Set :=
    | regname
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:69 Enum Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:81 Variant Tags *)
  Inductive Unions : Set :=
    | Ufoo
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:81 Variant Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:75 Record Tags *)
  Inductive Records : Set :=
    | Rmystruct
  .
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:75 Record Tags *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:150 No Confusion *)
  Section TransparentObligations.
    Local Set Transparent Obligations.

    Derive NoConfusion for Enums.
    Derive NoConfusion for Unions.
    Derive NoConfusion for Foo.
    Derive NoConfusion for FooConstructor.
    Derive NoConfusion for Records.
    Derive NoConfusion for MyStruct.
    Derive NoConfusion for RegName.
  End TransparentObligations.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:150 No Confusion *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:165 EqDec *)
  Derive EqDec for Enums.
  Derive EqDec for Unions.
  Derive EqDec for Foo.
  Derive EqDec for FooConstructor.
  Derive EqDec for Records.
  Derive EqDec for MyStruct.
  Derive EqDec for RegName.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:165 EqDec *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:127 Finite *)
  Section Finite.
    Import stdpp.finite.

    Local Obligation Tactic :=
      finite_from_eqdec.

    #[export,program] Instance RegName_finite : Finite RegName :=
      {| enum := [  ] |}.

    #[export,program] Instance FooConstructor_finite : Finite FooConstructor :=
      {| enum := [ Kfoox ] |}.
  End Finite.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:127 Finite *)

(* <<<<< nanosail/NanosailToMicrosail/Katamaran.ml:87 Base Module *)
  Module Export UntitledBase <: Base.
    Import ctx.notations.
    Import ctx.resolution.
    Import env.notations.
    Import stdpp.finite.

    Local Open Scope string_scope.

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:49 Notations for Aliases *)

    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:49 Notations for Aliases *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:79 typedeclkit *)
      #[export] Instance typedeclkit : TypeDeclKit :=
        {|
           enumi   := Enums;
           unioni  := Unions;
           recordi := Records;
        |}.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:79 typedeclkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:137 Enum Denote *)
      Definition enum_denote (e : Enums) : Set :=
        match e with
        | regname => RegName
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:137 Enum Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:168 Union Denote *)
      Definition union_denote (u : Unions) : Set :=
        match u with
        | Ufoo => Foo
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:168 Union Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:199 Record Denote *)
      Definition record_denote (r : Records) : Set :=
        match r with
        | Rmystruct => MyStruct
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:199 Record Denote *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:224 typedenotekit *)
      #[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=
        {|
           enumt := enum_denote;
           uniont := union_denote;
           recordt := record_denote;
        |}.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:224 typedenotekit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:255 Union Constructor *)
      Definition union_constructor (u : Unions) : Set :=
        match u with
        | Ufoo => FooConstructor
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:255 Union Constructor *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:343 Union Constructor Type *)
      Definition union_constructor_type (u : Unions) : union_constructor u -> Ty :=
        match u with
        | Ufoo => fun k => match k with
                           | Kfoox => ty.record Rmystruct
                           end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:343 Union Constructor Type *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:381 EqDec/Finite Instances *)
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
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:381 EqDec/Finite Instances *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:497 Union Fold *)
      Definition union_fold (U : unioni) : { K & Val (union_constructor_type U K) } -> uniont U :=
        match U with
        | Ufoo => fun Kv => match Kv with
                            | existT Kfoox ж1 => FooX ж1
                            end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:497 Union Fold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:585 Union Unfold *)
      Definition union_unfold (U : unioni) : uniont U -> { K & Val (union_constructor_type U K) } :=
        match U with
        | Ufoo => fun Kv => match Kv with
                            | FooX ж1 => existT Kfoox ж1
                            end
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:585 Union Unfold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:635 Record Field Type *)
      Definition record_field_type (R : recordi) : NCtx string Ty :=
        match R with
        | Rmystruct => [ "x" :: ty.int ]
        end.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:635 Record Field Type *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:709 Record Fold *)
      Definition record_fold (R : recordi) : NamedEnv Val (record_field_type R) -> recordt R :=
        match R with
        | Rmystruct => fun fields => MkMyStruct fields.[??"x"]
        end%exp.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:709 Record Fold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:785 Record Unfold *)
      Definition record_unfold (R : recordi) : recordt R -> NamedEnv Val (record_field_type R) :=
        match R with
        | Rmystruct => fun r => env.nil
                                  ► ("x" ∷ ty.int ↦ x r)
        end%env.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:785 Record Unfold *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:816 Typedefkit *)
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
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:816 Typedefkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:854 Canonicals *)
      Canonical typedeclkit.
      Canonical typedenotekit.
      Canonical typedefkit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:854 Canonicals *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:865 Varkit *)
      #[export] Instance varkit : VarKit := DefaultVarKit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:865 Varkit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:899 RegDeclKit *)
      Section RegDeclKit.
        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:30 Reg Inductive Type *)
          Inductive Reg : Ty -> Set :=
          .
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:30 Reg Inductive Type *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:36 No Confusion for Reg *)
          Section TransparentObligations.
            Local Set Transparent Obligations.
            Derive Signature NoConfusion NoConfusionHom EqDec for Reg.
          End TransparentObligations.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:36 No Confusion for Reg *)

        Definition 𝑹𝑬𝑮 : Ty -> Set := Reg.

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:101 REG_eq_dec Instance *)
          #[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=
            fun '(existT σ жx) '(existT τ жy) =>
            match жx, жy with
            end.
          Proof. all: transparent_abstract (intros H; depelim H). Defined.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:101 REG_eq_dec Instance *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:142 Obligation Tactic *)
          Local Obligation Tactic :=
            finite_from_eqdec.
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:142 Obligation Tactic *)

        (* <<<<< nanosail/NanosailToMicrosail/Registers.ml:126 REG_finite Instance *)
          Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=
            {| enum := [] |}
        (* >>>>> nanosail/NanosailToMicrosail/Registers.ml:126 REG_finite Instance *).
      End RegDeclKit.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:899 RegDeclKit *)

    (* <<<<< nanosail/NanosailToMicrosail/BaseModule.ml:917 Memory Model *)
      Section MemoryModel.
        (* TODO *)
      End MemoryModel.
    (* >>>>> nanosail/NanosailToMicrosail/BaseModule.ml:917 Memory Model *)

    Include BaseMixin.
  End UntitledBase.
(* >>>>> nanosail/NanosailToMicrosail/Katamaran.ml:87 Base Module *)

(*



*)
