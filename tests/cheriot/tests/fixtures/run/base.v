From Katamaran Require Import Base.

(*
  Untranslated definitions
*)

----
  $[complete]
  function bool_to_bits x = [bool_to_bit(x)]

OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 394
Sail location: File "model.sail" line 10 chars 1-40
Message: Elements of bit vector should all be bits; they were Type:Bitvector[1]
----

----
  function count_down n = {
    foreach (i from n to 0 by 1 in dec) { () }
  }

OCaml location: nanosail/SailToNanosail/Translate/Function.ml line 3
Sail location: File "model.sail" line 20 chars 1-60
Message: No message
----

----
  $file_start model.sail

OCaml location: nanosail/SailToNanosail/Translation.ml line 4
Sail location: UnknownLocation
Message: pragma file_start
----
