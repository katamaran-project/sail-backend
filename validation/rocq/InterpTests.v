(* Tests of Interp.v on a tiny hand-written µSail program. *)

From Stdlib Require Import
     Lists.List
     Strings.String
     ZArith.ZArith.
From Equations Require Import
     Equations.
From Katamaran Require Import
     Bitvector
     Semantics.Registers
     Program
     Semantics.
From stdpp Require Import finite.
From Validation Require Import Interp.

Set Implicit Arguments.
Import ctx.notations.
Import env.notations.
Open Scope string_scope.
Open Scope Z_scope.

(*** Types ***)

Inductive Enums : Set := ordering.
Inductive Ordering : Set := LT | EQ | GT.
Inductive Unions : Set :=.
Inductive Records : Set :=.

Section TransparentObligations.
  Local Set Transparent Obligations.
  Derive NoConfusion for Enums.
  Derive NoConfusion for Ordering.
  Derive NoConfusion for Unions.
  Derive NoConfusion for Records.
End TransparentObligations.

Derive EqDec for Enums.
Derive EqDec for Ordering.
Derive EqDec for Unions.
Derive EqDec for Records.

Section Finite.
  Import stdpp.finite.
  Local Obligation Tactic := finite_from_eqdec.
  #[export] Program Instance Ordering_finite : Finite Ordering :=
    {| enum := cons LT (cons EQ (cons GT nil)) |}.
End Finite.

(*** Base ***)

Module Import TestBase <: Base.
  Import stdpp.finite.

  #[export] Instance typedeclkit : TypeDeclKit :=
    {| enumi := Enums; unioni := Unions; recordi := Records |}.

  Definition enum_denote (E : Enums) : Set :=
    match E with ordering => Ordering end.
  Definition union_denote (U : Unions) : Set := match U with end.
  Definition record_denote (R : Records) : Set := match R with end.

  #[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=
    {| enumt := enum_denote; uniont := union_denote; recordt := record_denote |}.

  Definition union_constructors (U : Unions) : Set := match U with end.
  Definition union_constructor_type (U : Unions) : union_constructors U -> Ty :=
    match U with end.
  Definition union_unfold (U : unioni) :
    uniont U -> { K & Val (union_constructor_type U K) } := match U with end.
  Definition union_fold (U : unioni) :
    { K & Val (union_constructor_type U K) } -> uniont U := match U with end.
  Definition record_field_type (R : recordi) : NCtx Empty_set Ty := match R with end.
  Definition record_fold (R : recordi) :
    NamedEnv Val (record_field_type R) -> recordt R := match R with end.
  Definition record_unfold (R : recordi) :
    recordt R -> NamedEnv Val (record_field_type R) := match R with end.

  #[export] Instance eqdec_enum_denote E : EqDec (enum_denote E) :=
    ltac:(destruct E; auto with typeclass_instances).
  #[export] Instance finite_enum_denote E : finite.Finite (enum_denote E) :=
    ltac:(destruct E; auto with typeclass_instances).
  #[export] Instance eqdec_union_denote U : EqDec (union_denote U) :=
    ltac:(destruct U).
  #[export] Instance eqdec_union_constructors U : EqDec (union_constructors U) :=
    ltac:(destruct U).
  #[export] Instance finite_union_constructors U : finite.Finite (union_constructors U) :=
    ltac:(destruct U).
  #[export] Instance eqdec_record_denote R : EqDec (record_denote R) :=
    ltac:(destruct R).

  #[export,refine] Instance typedefkit : TypeDefKit typedenotekit :=
    {| unionk         := union_constructors;
       unionk_ty      := union_constructor_type;
       unionv_fold    := union_fold;
       unionv_unfold  := union_unfold;
       recordf        := Empty_set;
       recordf_ty     := record_field_type;
       recordv_fold   := record_fold;
       recordv_unfold := record_unfold |}.
  Proof. all: abstract (intros []). Defined.

  #[export] Instance varkit : VarKit := DefaultVarKit.

  Section RegDeclKit.
    Inductive Reg : Ty -> Set :=
    | acc  : Reg ty.int
    | flag : Reg ty.bool
    | pc   : Reg (ty.bvec 32).

    Section TransparentObligations.
      Local Set Transparent Obligations.
      Derive Signature NoConfusion for Reg.
    End TransparentObligations.

    Definition 𝑹𝑬𝑮 : Ty -> Set := Reg.

    #[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=
      fun '(existT σ x) '(existT τ y) =>
        match x, y with
        | acc , acc  => left eq_refl
        | flag, flag => left eq_refl
        | pc  , pc   => left eq_refl
        | _   , _    => right _
        end.
    Proof. all: transparent_abstract (intros H; depelim H). Defined.

    Local Obligation Tactic := finite_from_eqdec.
    #[export] Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=
      {| enum := cons (existT _ acc) (cons (existT _ flag) (cons (existT _ pc) nil)) |}.
  End RegDeclKit.

  (* Memory: association list, newest write first. *)
  Definition Memory : Type := list (Z * Z).

  Include BaseMixin.
End TestBase.

Fixpoint mem_read (μ : Memory) (a : Z) : Z :=
  match μ with
  | nil => 0
  | cons (b, v) μ => if Z.eqb a b then v else mem_read μ a
  end.
Definition mem_write (μ : Memory) (a v : Z) : Memory := cons (a, v) μ.

(*** Program ***)

Module Import TestProgram <: Program TestBase.

  Section FunDeclKit.
    Inductive Fun : PCtx -> Ty -> Set :=
    | cmp        : Fun [ "x" ∷ ty.int; "y" ∷ ty.int ] (ty.enum ordering)
    | sum_to     : Fun [ "n" ∷ ty.int ] ty.int
    | bump       : Fun [ "d" ∷ ty.int ] ty.int
    | advance_pc : Fun ctx.nil ty.unit
    | store_load : Fun [ "a" ∷ ty.int; "v" ∷ ty.int ] ty.int
    | safe_sub   : Fun [ "x" ∷ ty.int; "y" ∷ ty.int ] ty.int
    | sum_list   : Fun [ "xs" ∷ ty.list ty.int ] ty.int
    | main       : Fun [ "n" ∷ ty.int ] ty.int.

    Inductive FunX : PCtx -> Ty -> Set :=
    | rd  : FunX [ "a" ∷ ty.int ] ty.int
    | wr  : FunX [ "a" ∷ ty.int; "v" ∷ ty.int ] ty.unit
    | rnd : FunX ctx.nil ty.int.

    Inductive Lem : PCtx -> Set :=
    | nonneg : Lem [ "n" ∷ ty.int ].

    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := FunX.
    Definition 𝑳  : PCtx -> Set := Lem.
  End FunDeclKit.

  Include FunDeclMixin TestBase.

  Section FunDefKit.
    Import ctx.resolution.
    Local Coercion stm_exp : Exp >-> Stm.
    Local Notation "'x'"  := (@exp_var _ "x" _ _) : exp_scope.
    Local Notation "'y'"  := (@exp_var _ "y" _ _) : exp_scope.
    Local Notation "'n'"  := (@exp_var _ "n" _ _) : exp_scope.
    Local Notation "'d'"  := (@exp_var _ "d" _ _) : exp_scope.
    Local Notation "'a'"  := (@exp_var _ "a" _ _) : exp_scope.
    Local Notation "'v'"  := (@exp_var _ "v" _ _) : exp_scope.
    Local Notation "'r'"  := (@exp_var _ "r" _ _) : exp_scope.
    Local Notation "'s'"  := (@exp_var _ "s" _ _) : exp_scope.
    Local Notation "'o'"  := (@exp_var _ "o" _ _) : exp_scope.
    Local Notation "'`LT'" := (@exp_val _ (ty.enum ordering) LT).
    Local Notation "'`EQ'" := (@exp_val _ (ty.enum ordering) EQ).
    Local Notation "'`GT'" := (@exp_val _ (ty.enum ordering) GT).

    Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
      | cmp => if: x < y then `LT else if: x = y then `EQ else `GT
      (* recursion as a loop *)
      | sum_to =>
          if: n <= exp_int 0 then stm_val ty.int 0
          else let: "r" := call sum_to (n - exp_int 1) in n + r
      (* registers *)
      | bump =>
          let: "a" := stm_read_register acc in
          let: "r" := stm_exp (a + d) in
          stm_write_register acc r ;;
          stm_write_register flag (exp_int 10 < r) ;;
          r
      | advance_pc =>
          let: "a" := stm_read_register pc in
          stm_write_register pc (exp_binop bop.bvadd a (exp_val (ty.bvec 32) (bv.of_Z 4))) ;;
          stm_val ty.unit tt
      (* foreign calls *)
      | store_load => foreign wr a v ;; foreign rd a
      (* failure; the lemma is a no-op *)
      | safe_sub =>
          stm_lemmak nonneg (env.snoc env.nil ("n"∷ty.int) x%exp)
            (stm_assertk (y <= x) (exp_string "underflow") (x - y))
      (* list match *)
      | sum_list =>
          stm_match_list (exp_var "xs") (stm_val ty.int 0) "h" "t"
            (let: "r" := call sum_list (exp_var "t") in exp_var "h" + r)
      | main =>
          let: "s" := call sum_to n in
          let: "o" := call cmp s (exp_int 10) in
          match: o in ordering with
          | LT => call bump s
          | EQ => stm_fail _ "ten"
          | GT => call store_load (exp_int 4) s
          end
      end.
  End FunDefKit.

  Include DefaultRegStoreKit TestBase.

  Section ForeignKit.
    Definition ForeignCall {Δ σ} (f : 𝑭𝑿 Δ σ) :
      CStore Δ -> string + Val σ -> RegStore -> RegStore -> Memory -> Memory -> Prop :=
      match f in FunX Δ σ return CStore Δ -> string + Val σ -> _ with
      | rd  => fun args res γ γ' μ μ' =>
                 γ' = γ /\ μ' = μ /\ res = inr (mem_read μ (env.head args))
      | wr  => fun args res γ γ' μ μ' =>
                 γ' = γ /\ res = inr tt /\
                 μ' = mem_write μ (env.head (env.tail args)) (env.head args)
      | rnd => fun args res γ γ' μ μ' =>
                 γ' = γ /\ μ' = μ /\ exists v, res = inr v
      end.
    Lemma ForeignProgress {Δ σ} (f : 𝑭𝑿 Δ σ) (args : CStore Δ) γ μ :
      exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.
    Proof. destruct f; cbn; repeat eexists. Unshelve. exact 0. Qed.
  End ForeignKit.

  Include ProgramMixin TestBase.
  Import callgraph.

  Lemma fundef_bindfree (Δ : PCtx) (τ : Ty) (f : Fun Δ τ) :
    stm_bindfree (FunDef f).
  Proof. destruct f; now vm_compute. Qed.

  Definition 𝑭_call_graph := generic_call_graph.
  Lemma 𝑭_call_graph_wellformed : CallGraphWellFormed 𝑭_call_graph.
  Proof. apply generic_call_graph_wellformed, fundef_bindfree. Qed.

  Definition 𝑭_accessible {Δ τ} (f : 𝑭 Δ τ) : option (Accessible 𝑭_call_graph f) :=
    None.
End TestProgram.

(*** Interpreter instance ***)

Module Import TestInterp := MakeInterp TestBase TestProgram.

(* Answers rd/wr from the memory; no answer for rnd. *)
Definition oracle : Oracle :=
  fun Δ σ f =>
    match f in FunX Δ σ return CStore Δ -> State -> option (Result σ * State) with
    | rd  => fun args st => Some (inr (mem_read (st_mem st) (env.head args)), st)
    | wr  => fun args st =>
               Some (inr tt, mkState (st_regs st)
                       (mem_write (st_mem st) (env.head (env.tail args)) (env.head args)))
    | rnd => fun _ _ => None
    end.

(* Same, but rnd returns 7. *)
Definition oracle7 : Oracle :=
  fun Δ σ f =>
    match f in FunX Δ σ return CStore Δ -> State -> option (Result σ * State) with
    | rnd => fun _ st => Some (inr 7, st)
    | f   => oracle f
    end.

Definition regs0 : RegStore :=
  fun σ r => match r in Reg σ return Val σ with
             | acc  => 0
             | flag => false
             | pc   => bv.of_Z 0x80000000
             end.
Definition st0 : State := mkState regs0 nil.

(* Observable part of a state: registers acc, flag, pc (as Z) and memory. *)
Definition observe (st : State) : Z * bool * Z * Memory :=
  (read_register (st_regs st) acc, read_register (st_regs st) flag,
   bv.unsigned (read_register (st_regs st) pc), st_mem st).

Definition run {Γ τ} (o : Oracle) (fuel : nat) (s : Stm Γ τ) (δ : CStore Γ) (st : State) :=
  omap (fun '(r, st') => (r, observe st')) (exec o fuel s st δ).

Definition fuel := 100%nat.

(*** Tests ***)

(* value and pure match on an enum via cmp *)
Example t_cmp :
  run oracle fuel (FunDef cmp) [env].["x"∷ty.int ↦ 3].["y"∷ty.int ↦ 5] st0
  = Done (inr LT, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* recursion: 1 + 2 + ... + 10 *)
Example t_sum_to :
  run oracle fuel (FunDef sum_to) [env].["n"∷ty.int ↦ 10] st0
  = Done (inr 55, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* out of fuel: recursion depth exceeds the fuel *)
Example t_fuel :
  run oracle 20 (FunDef sum_to) [env].["n"∷ty.int ↦ 10] st0 = OutOfFuel.
Proof. vm_compute. reflexivity. Qed.

(* registers: two bumps, flag set when acc > 10 *)
Example t_bump :
  run oracle fuel
    (stm_seq (stm_call bump [env].["d"∷ty.int ↦ exp_int 5])
             (stm_call bump [env].["d"∷ty.int ↦ exp_int 7]))
    [env] st0
  = Done (inr 12, (12, true, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* bitvector register *)
Example t_pc :
  run oracle fuel (FunDef advance_pc) [env] st0
  = Done (inr tt, (0, false, 0x80000004, nil)).
Proof. vm_compute. reflexivity. Qed.

(* foreign calls answered by the oracle *)
Example t_store_load :
  run oracle fuel (FunDef store_load) [env].["a"∷ty.int ↦ 4].["v"∷ty.int ↦ 42] st0
  = Done (inr 42, (0, false, 0x80000000, cons (4, 42) nil)).
Proof. vm_compute. reflexivity. Qed.

(* oracle without an answer: NoAnswer rnd (OutOfFuel without fuel); another oracle answers *)
Example t_rnd_none :
  run oracle fuel (stm_foreign rnd [env]) [env] st0 = NoAnswer rnd.
Proof. vm_compute. reflexivity. Qed.
Example t_rnd_none_deep :
  run oracle fuel
    (stm_seq (stm_call bump [env].["d"∷ty.int ↦ exp_int 5])
             (stm_let "x" ty.int (stm_foreign rnd [env]) (stm_val ty.int 0)))
    [env] st0
  = NoAnswer rnd.
Proof. vm_compute. reflexivity. Qed.
Example t_rnd_fuel :
  run oracle 0 (stm_foreign rnd [env]) [env] st0 = OutOfFuel.
Proof. vm_compute. reflexivity. Qed.
Example t_rnd_7 :
  run oracle7 fuel (stm_foreign rnd [env]) [env] st0
  = Done (inr 7, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* assertion passes (lemma skipped) / fails *)
Example t_safe_sub_ok :
  run oracle fuel (FunDef safe_sub) [env].["x"∷ty.int ↦ 5].["y"∷ty.int ↦ 3] st0
  = Done (inr 2, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.
Example t_safe_sub_fail :
  run oracle fuel (FunDef safe_sub) [env].["x"∷ty.int ↦ 3].["y"∷ty.int ↦ 5] st0
  = Done (inl "underflow", (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* failure after a register write: the write persists, the rest is skipped *)
Example t_fail_after_write :
  run oracle fuel
    (stm_seq (stm_call bump [env].["d"∷ty.int ↦ exp_int 5])
       (stm_seq (stm_fail ty.int "stop")
                (stm_call bump [env].["d"∷ty.int ↦ exp_int 7])))
    [env] st0
  = Done (inl "stop", (5, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* list match *)
Example t_sum_list :
  run oracle fuel (FunDef sum_list) [env].["xs"∷ty.list ty.int ↦ (cons 1 (cons 2 (cons 3 (cons 4 nil))))] st0
  = Done (inr 10, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* main: the three arms of the enum match *)
Example t_main_lt :
  run oracle fuel (FunDef main) [env].["n"∷ty.int ↦ 3] st0
  = Done (inr 6, (6, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.
Example t_main_eq :
  run oracle fuel (FunDef main) [env].["n"∷ty.int ↦ 4] st0
  = Done (inl "ten", (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.
Example t_main_gt :
  run oracle fuel (FunDef main) [env].["n"∷ty.int ↦ 5] st0
  = Done (inr 15, (0, false, 0x80000000, cons (4, 15) nil)).
Proof. vm_compute. reflexivity. Qed.

(* exec_call *)
Example t_exec_call :
  omap fst (exec_call oracle fuel sum_to [env].["n"∷ty.int ↦ 4] st0) = Done (inr 10).
Proof. vm_compute. reflexivity. Qed.

(* stm_bind, stm_debugk *)
Example t_bind :
  run oracle fuel
    (stm_debugk (stm_bind (stm_val ty.int 3) (fun k => stm_val ty.int (k + 1))))
    [env] st0
  = Done (inr 4, (0, false, 0x80000000, nil)).
Proof. vm_compute. reflexivity. Qed.

(* local store: assignment, let and block scoping *)
Section Locals.
  Import ctx.resolution.
  Definition s_locals : Stm [ "x" ∷ ty.int ] ty.int :=
    stm_seq (stm_assign "x" (stm_val ty.int 5))
      (stm_let "y" ty.int (stm_val ty.int 1)
         (stm_block [env].["z"∷ty.int ↦ 2]
            (stm_assign "x" (stm_exp (exp_var "x" + exp_var "y" + exp_var "z"))))).
End Locals.

Example t_locals :
  omap (fun '(r, _, δ) => (r, env.head δ))
    (exec_aux oracle fuel s_locals st0 [env].["x"∷ty.int ↦ 0])
  = Done (inr 8, 8).
Proof. vm_compute. reflexivity. Qed.
