$ifndef _MSP430EXCEPTION
$define _MSP430EXCEPTION

scattered union exception
union clause exception = notImplemented : string
union clause exception = notAllowed : string
union clause exception = undefindedBehavior :string
union clause exception = undefindedInstruction : bits(16)

union clause exception = ipe_violation : bits(16)
union clause exception = power_up_clear : unit

union clause exception = test_fail : string

$endif
