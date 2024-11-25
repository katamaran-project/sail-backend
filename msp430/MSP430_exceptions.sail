$ifndef _MSP430EXCEPTION
$define _MSP430EXCEPTION

scattered union exception
union clause exception = notImplemented : string
union clause exception = notAllowed : string
union clause exception = undefindedBehavior : string
union clause exception = undefindedInstruction : bits(16)

$endif
