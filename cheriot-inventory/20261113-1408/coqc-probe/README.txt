Probe copies of full-workaround/base.v and machine.v, used only to see which coqc errors
lie beyond the NYI holes. Each file gets, after its imports:

    Import ListNotations.
    Axiom NYI : forall {A : Type}, list nat -> A.

so that every NYI[k] becomes an admitted term. Nothing else is changed.
