default Order dec

$include <prelude.sail>

/*
   (exclude-functions-as-value '(("pmp_check" "None()") ("pmp_count" "0")))
   removes pmp_check and the extern pmp_count, and replaces each call with the
   given Sail expression, type-checked at the call's type: pmp_check always
   says "no fault", and pmp_count says that there are no PMP entries.
   pmp_check's body has a loop, which the translator does not support.
   The argument of the second call to pmp_check has an effect (it reads a
   register), so it is still computed.
*/

enum ExceptionType = { E_Load_Access_Fault, E_SAMO_Access_Fault }

val pmp_count = pure "pmp_count" : unit -> range(0, 64)

register mstatus : bits(32)

val pmp_check : forall 'n, 'n > 0. (bits(32), int('n)) -> option(ExceptionType)
function pmp_check(addr, width) = {
  var r : option(ExceptionType) = None();
  foreach (i from 0 to 3) {
    r = Some(E_Load_Access_Fault)
  };
  r
}

val is_fault : option(ExceptionType) -> bool
function is_fault(r) = match r { Some(_) => true, None() => false }

val checked : bits(32) -> bool
function checked(addr) =
  if pmp_count() == 0 then false else is_fault(pmp_check(addr, 4))

val checked_mstatus : unit -> bool
function checked_mstatus() = is_fault(pmp_check(mstatus, 4))
