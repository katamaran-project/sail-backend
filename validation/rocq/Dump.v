(* Canonical text dump of a CHERIoT harness state, deltas, and a memory write
   log.  Format: validation/docs/state-format.md. *)

From Stdlib Require Import
     Strings.Ascii
     Strings.String
     Lists.List
     ZArith.ZArith.
From Katamaran Require Import
     Base
     Bitvector
     Program.
From CheriotModel Require Import
     base
     machine.
From Validation Require Import
     Interp
     Harness
     Names.



Import ListNotations.
Import UntitledBase.
Import ModelProgram.
Import Harness.I.

Set Default Proof Mode "Classic".
Open Scope string_scope.
Open Scope Z_scope.

(*** Names, from the inductive types (Names.v) ***)

Definition reg_name_dep : forall σ (r : 𝑹𝑬𝑮 σ), named r.
Proof. intros σ r; destruct r; ltac2:(name_goal ()). Defined.

Definition reg_name (r : sigT 𝑹𝑬𝑮) : string := let (σ, x) := r in reg_name_dep σ x.

Definition enum_name_dep : forall E (v : enumt E), named v.
Proof. intros E v; destruct E; destruct v; ltac2:(name_goal ()). Defined.

Definition enum_name (E : enumi) (v : enumt E) : string := enum_name_dep E v.

(* Union values: the name of the value's constructor (Sail's), not of its
   [unionk] tag. *)
Definition union_name_dep : forall U (v : uniont U), named v.
Proof. intros U v; destruct U; destruct v; ltac2:(name_goal ()). Defined.

Definition union_name (U : unioni) (v : uniont U) : string := union_name_dep U v.

(* All registers, in declaration order; the generated Finite instance is
   local to its section, hence explicit. *)
Definition all_regs : list (sigT 𝑹𝑬𝑮) := @stdpp.finite.enum _ _ 𝑹𝑬𝑮_finite.

(*** Numbers ***)

Definition digit (d : Z) : ascii :=
  ascii_of_nat (Z.to_nat (if d <? 10 then 48 + d else 87 + d)).

Fixpoint hex_aux (k : nat) (z : Z) (acc : string) : string :=
  match k with
  | O => acc
  | S k' => hex_aux k' (Z.shiftr z 4) (String (digit (Z.land z 15)) acc)
  end.

(* "0x" and exactly [k] lowercase digits (z >= 0, z < 16^k). *)
Definition hex (k : nat) (z : Z) : string := "0x" ++ hex_aux k z "".

(* Digits of a bv n: ceil(n / 4), at least 1. *)
Definition hex_width (n : nat) : nat := Nat.max 1 (Nat.div (n + 3) 4).

(* Addresses: at least 8 digits. *)
Definition hex_addr (a : Z) : string :=
  hex (Nat.max 8 (Nat.div (Z.to_nat (Z.log2 a) + 4) 4)) a.

Fixpoint dec_aux (k : nat) (z : Z) (acc : string) : string :=
  match k with
  | O => acc
  | S k' =>
      let acc' := String (digit (z mod 10)) acc in
      if z <? 10 then acc' else dec_aux k' (z / 10) acc'
  end.

Definition dec (z : Z) : string :=
  let s := dec_aux (S (Z.to_nat (Z.log2 (Z.abs z)))) (Z.abs z) "" in
  if z <? 0 then "-" ++ s else s.

Definition dec_nat (n : nat) : string := dec (Z.of_nat n).

(*** Values ***)

(* One line of a dump: key and value. *)
Definition Item : Type := string * string.

Definition line (i : Item) : string := fst i ++ " " ++ snd i.

(* Leaves: [k v].  Records: [k.field ...].  Unions and sums: [k Ctor], then the
   payload under [k.Ctor] (none for unit).  Products, tuples, lists: [k.0], [k.1], ...
   (lists also [k.len]).  [fuel] bounds the nesting depth. *)
Fixpoint render (fuel : nat) (σ : Ty) (k : string) {struct fuel} : Val σ -> list Item :=
  match fuel with
  | O => fun _ => [(k, "<depth>")]
  | S fuel' =>
      let payload (τ : Ty) (k' : string) : Val τ -> list Item :=
        match τ return Val τ -> list Item with
        | ty.unit => fun _ => []
        | τ => render fuel' τ k'
        end in
      match σ return Val σ -> list Item with
      | ty.int => fun v => [(k, dec v)]
      | ty.bool => fun v => [(k, if v then "true" else "false")]
      | ty.string => fun v => [(k, """" ++ v ++ """")]
      | ty.unit => fun _ => [(k, "()")]
      | ty.enum E => fun v => [(k, enum_name E v)]
      | ty.bvec n => fun v => [(k, hex (hex_width n) (bv.unsigned v))]
      | ty.list τ => fun v =>
          (k ++ ".len", dec_nat (length v))
            :: concat (map (fun '(i, x) => render fuel' τ (k ++ "." ++ dec_nat i) x)
                         (combine (seq 0 (length v)) v))
      | ty.prod τ1 τ2 => fun '(a, b) => app (render fuel' τ1 (k ++ ".0") a) (render fuel' τ2 (k ++ ".1") b)
      | ty.sum τ1 τ2 => fun v =>
          match v with
          | inl a => (k, "inl") :: payload τ1 (k ++ ".inl") a
          | inr b => (k, "inr") :: payload τ2 (k ++ ".inr") b
          end
      | ty.tuple σs => fun v =>
          (fix tup (σs : Ctx Ty) : EnvRec Val σs -> list Item :=
             match σs return EnvRec Val σs -> list Item with
             | ctx.nil => fun _ => []
             | ctx.snoc σs' τ => fun '(vs, x) =>
                 app (tup σs' vs) (render fuel' τ (k ++ "." ++ dec_nat (ctx.length σs')) x)
             end) σs v
      | ty.union U => fun v =>
          let c := union_name U v in
          match unionv_unfold U v with
          | existT K p => (k, c) :: payload (unionk_ty U K) (k ++ "." ++ c) p
          end
      | ty.record R => fun v =>
          (fix fields {Δ} (e : NamedEnv Val Δ) : list Item :=
             match e with
             | env.nil => []
             | @env.snoc _ _ _ e' b x => app (fields e') (render fuel' (type b) (k ++ "." ++ name b) x)
             end) _ (recordv_unfold R v)
      end
  end.

Definition render_depth : nat := 16.

(*** Registers ***)

(* Registers left out of the dump, with the reason (state-format.md, "Not
   compared").  Everything else in [all_regs] is dumped. *)
Definition excluded : list (string * string) :=
  map (fun i => ("vr" ++ dec_nat i, "vector register: vector extension out of scope; 65536 bits"))
      (seq 0 32).

Definition excluded_names : list string := map fst excluded.

Definition reg_items (γ : RegStore) : list Item :=
  let ex := excluded_names in
  concat (map (fun '(existT σ x) =>
                 let n := reg_name (existT σ x) in
                 if existsb (String.eqb n) ex then []
                 else render render_depth σ n (read_register γ x))
              all_regs).

(*** Memory write log ***)

(* The harness memory is a function (Harness.v), so it cannot be listed.
   [oracle_log] wraps Harness.oracle and records each written key in the
   memory itself, above every physical address (key log_base: count; key
   log_base + 1 + i: i-th written key).  Keys as in Harness.v: byte address
   (>= 0) or tag key (< 0). *)
Definition log_base : Z := 2 ^ 40.

Definition log_push (m : Memory) (key : Z) : Memory :=
  let n := m log_base in
  fun j => if j =? log_base then n + 1 else if j =? log_base + 1 + n then key else m j.

Definition log_keys (m : Memory) : list Z :=
  map (fun i => m (log_base + 1 + Z.of_nat i)) (seq 0 (Z.to_nat (m log_base))).

(* Keys a foreign call writes.  Only __write_mem and MEMw_tag change memory
   (harness.md, "Foreign functions"). *)
Definition written {Δ σ} (f : 𝑭𝑿 Δ σ) : CStore Δ -> list Z :=
  match f in FunX Δ σ return CStore Δ -> list Z with
  (* __write_mem(kind, xlen, addr, width, data) *)
  | @__write_mem _ _ => fun a =>
      let addr := bv.unsigned (env.head (env.tail (env.tail a))) in
      map (fun i => addr + Z.of_nat i) (seq 0 (Z.to_nat (env.head (env.tail a))))
  (* MEMw_tag(tag address, tag) *)
  | MEMw_tag => fun a => [tag_key (bv.unsigned (env.head (env.tail a)))]
  | _ => fun _ => []
  end.

Definition oracle_log : Oracle :=
  fun Δ σ f args st =>
  match oracle Δ σ f args st with
  | Some (r, st') => Some (r, mkState (st_regs st') (fold_left log_push (written f args) (st_mem st')))
  | None => None
  end.

(* As Harness.reset and Harness.run_step, with [oracle_log] and any start state. *)
Definition reset_from (fuel : nat) (st0 : State) : Outcome (Result ty.unit * State) :=
  omap (fun '(r, st) =>
          match r with
          | inr _ => (r, set_pc st c_entry)
          | inl _ => (r, st)
          end)
    (exec_call oracle_log fuel init_model (env.snoc env.nil (MkB _ _) tt) st0).

Definition reset_log (fuel : nat) (m : Memory) : Outcome (Result ty.unit * State) :=
  reset_from fuel (state0 m).

Definition step_log (fuel : nat) (st : State) : Outcome (Result ty.bool * State) :=
  exec_call oracle_log fuel step (env.snoc env.nil (MkB _ _) 0) st.

(* Up to [n] steps; stops after a step that does not return [inr _].
   The states after each step, with the step's result. *)
Fixpoint steps_log (fuel n : nat) (st : State) : Outcome (list (Result ty.bool * State)) :=
  match n with
  | O => Done []
  | S n' =>
      obind (step_log fuel st) (fun '(r, st') =>
        match r with
        | inr _ => omap (cons (r, st')) (steps_log fuel n' st')
        | inl _ => Done [(r, st')]
        end)
  end.

(*** Memory items ***)

Fixpoint insert_Z (x : Z) (l : list Z) : list Z :=
  match l with
  | [] => [x]
  | y :: l' => if x <? y then x :: l else if x =? y then l else y :: insert_Z x l'
  end.

(* Sorted, without duplicates. *)
Definition sort_Z (l : list Z) : list Z := fold_right insert_Z [] l.

(* mem <address> <byte>, by address; tag <granule address> <bool>, by address.
   Tag keys are [-1 - tag address]; the granule address is 8 * tag address. *)
Definition mem_items (m : Memory) : list Item * list Item :=
  let ks := sort_Z (log_keys m) in
  let bytes := filter (fun k => 0 <=? k) ks in
  let tags := sort_Z (map (fun k => -1 - k) (filter (fun k => k <? 0) ks)) in
  (map (fun a => ("mem " ++ hex_addr a, hex 2 (m a))) bytes,
   map (fun t => ("tag " ++ hex_addr (8 * t), if read_tag m t then "true" else "false")) tags).

(*** States ***)

(* Merge sort of items by key (bytewise). *)
Fixpoint merge_items (l1 : list Item) : list Item -> list Item :=
  fix merge_r (l2 : list Item) : list Item :=
    match l1, l2 with
    | [], _ => l2
    | _, [] => l1
    | x :: l1', y :: l2' =>
        if String.leb (fst x) (fst y) then x :: merge_items l1' l2 else y :: merge_r l2'
    end.

Fixpoint sort_pass (l : list (list Item)) : list (list Item) :=
  match l with
  | a :: b :: rest => merge_items a b :: sort_pass rest
  | _ => l
  end.

Fixpoint sort_loop (fuel : nat) (l : list (list Item)) : list Item :=
  match l with
  | [] => []
  | [a] => a
  | _ => match fuel with O => concat l | S f => sort_loop f (sort_pass l) end
  end.

Definition sort_items (l : list Item) : list Item :=
  sort_loop (length l) (map (fun i => [i]) l).

(* A dumped state: three sections, each sorted by key. *)
Record Dumped : Type := mkDumped { d_regs : list Item; d_mem : list Item; d_tags : list Item }.

Definition dumped (st : State) : Dumped :=
  let '(ms, ts) := mem_items (st_mem st) in
  mkDumped (sort_items (reg_items (st_regs st))) ms ts.

Definition items (d : Dumped) : list Item := app (d_regs d) (app (d_mem d) (d_tags d)).

(* Registers, then memory, then tags. *)
Definition dump (st : State) : list string := map line (items (dumped st)).

(* Items of [after] whose key is new or whose value changed; both sorted by key. *)
Fixpoint delta_sorted (before : list Item) : list Item -> list Item :=
  fix delta_r (after : list Item) : list Item :=
    match before, after with
    | _, [] => []
    | [], _ => after
    | (kb, vb) :: before', (ka, va) :: after' =>
        match String.compare kb ka with
        | Lt => delta_sorted before' after
        | Gt => (ka, va) :: delta_r after'
        | Eq => if String.eqb vb va then delta_sorted before' after'
                else (ka, va) :: delta_sorted before' after'
        end
    end.

Definition delta_dumped (b a : Dumped) : list Item :=
  app (delta_sorted (d_regs b) (d_regs a))
      (app (delta_sorted (d_mem b) (d_mem a)) (delta_sorted (d_tags b) (d_tags a))).

Definition delta (before after : State) : list string :=
  map line (delta_dumped (dumped before) (dumped after)).

(* A run: state i (0 = after reset) prefixed with "i ". *)
Definition dump_run (sts : list State) : list string :=
  concat (map (fun '(i, st) => map (fun l => dec_nat i ++ " " ++ l) (dump st))
              (combine (seq 0 (length sts)) sts)).

(* The excluded registers, one per line: name and reason. *)
Definition excluded_lines : list string := map (fun '(n, why) => n ++ " " ++ why) excluded.

(*** Start values not fixed by reset ***)

(* Another start store: every bitvector inverted, every bool negated, every int
   + 1, every enum the next constructor.  A register item that differs after
   reset from [reg_default] and from [reg_alt] depends on the start value. *)
Definition next_enum (E : enumi) (v : enumt E) : enumt E :=
  match stdpp.finite.enum (enumt E) with
  | [] => v
  | x0 :: _ =>
      (fix go (l : list (enumt E)) : enumt E :=
         match l with
         | [] => x0
         | x :: rest => if Equations.Prop.Classes.eq_dec x v then hd x0 rest else go rest
         end) (stdpp.finite.enum (enumt E))
  end.

Fixpoint perturb (fuel : nat) (σ : Ty) {struct fuel} : Val σ -> Val σ :=
  match fuel with
  | O => fun v => v
  | S fuel' =>
      match σ return Val σ -> Val σ with
      | ty.bvec n => fun v => bv.not v
      | ty.bool => negb
      | ty.int => fun v => v + 1
      | ty.enum E => next_enum E
      | ty.record R => fun v =>
          recordv_fold R (env.map (fun b x => perturb fuel' (type b) x) (recordv_unfold R v))
      | _ => fun v => v
      end
  end.

Definition reg_alt : RegStore := fun σ r => perturb render_depth σ (reg_default σ r).

(* Keys whose value after reset differs between the two start stores. *)
Definition start_dependent (fuel : nat) (m : Memory) : Outcome (list string) :=
  obind (reset_from fuel (mkState reg_default m)) (fun '(_, st1) =>
  omap (fun '(_, st2) => map fst (delta_dumped (dumped st1) (dumped st2)))
       (reset_from fuel (mkState reg_alt m))).
