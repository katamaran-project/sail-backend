open ExtBase


(*
   What a branch of an if or a match can change about the size variables, compared to the code around it.

   Function.specialise_branch asks Sail's solver, for every branch, whether the branch is dead and
   which size variables it fixes (if 'n == 32 then ...). Each query starts the SMT solver.
   Sail's prove and solve_unique depend on the environment only through its constraints
   (Type_check.Env.get_constraints), so:

   - a branch that adds no constraints is dead only if the code around it is;
   - a size variable that shares no constraint with an added one, not even through other size variables,
     has the same values inside the branch as outside, so the branch cannot fix it.

   The queries are limited accordingly.
*)


module S = struct
  include Libsail.Ast
  include Libsail.Ast_util
end

module KidSet = Libsail.Ast_compare.KidSet


(* The constraints of the branch's environment that the outer environment does not have *)
let added_constraints
    ~(outer : S.n_constraint list)
    ~(inner : S.n_constraint list) : S.n_constraint list
  =
  let is_outer (numeric_constraint : S.n_constraint) : bool =
    List.exists outer ~f:(fun outer_constraint -> Libsail.Ast_compare.NC.compare numeric_constraint outer_constraint = 0)
  in
  List.filter inner ~f:(fun numeric_constraint -> not (is_outer numeric_constraint))


(* The size variables that share a constraint with one of the given variables, directly or through other variables *)
let linked_size_variables
    ~(constraints : S.n_constraint list)
    (kids         : KidSet.t          ) : KidSet.t
  =
  let rec close (kids : KidSet.t) : KidSet.t =
    let kids' =
      List.fold constraints ~init:kids ~f:(fun accumulated numeric_constraint ->
          let constraint_kids = S.tyvars_of_constraint numeric_constraint
          in
          if
            KidSet.is_empty (KidSet.inter constraint_kids accumulated)
          then
            accumulated
          else
            KidSet.union constraint_kids accumulated
        )
    in
    if
      KidSet.equal kids kids'
    then
      kids
    else
      close kids'
  in
  close kids


(* The size variables whose values the branch can change: those linked, by the branch's constraints, to an added constraint *)
let affected_size_variables
    ~(outer : S.n_constraint list)
    ~(inner : S.n_constraint list) : KidSet.t
  =
  let added_kids =
    List.fold (added_constraints ~outer ~inner) ~init:KidSet.empty ~f:(fun accumulated numeric_constraint ->
        KidSet.union accumulated (S.tyvars_of_constraint numeric_constraint))
  in
  linked_size_variables ~constraints:inner added_kids
