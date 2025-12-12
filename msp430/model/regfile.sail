/* [not used]
//zero value for our registers so we can zero them out
let zero_reg : wordBits = 0b0000000000000000
*/

register    PC_reg  : wordBits /* program counter    */
register    SP_reg  : wordBits /* stack pointer      */
register SRCG1_reg  : wordBits /* status             */
register   CG2_reg  : wordBits /* constant generator */
register    R4_reg  : wordBits /* general purpose    */
register    R5_reg  : wordBits /* general purpose    */
register    R6_reg  : wordBits /* general purpose    */
register    R7_reg  : wordBits /* general purpose    */
register    R8_reg  : wordBits /* general purpose    */
register    R9_reg  : wordBits /* general purpose    */
register   R10_reg  : wordBits /* general purpose    */
register   R11_reg  : wordBits /* general purpose    */
register   R12_reg  : wordBits /* general purpose    */
register   R13_reg  : wordBits /* general purpose    */
register   R14_reg  : wordBits /* general purpose    */
register   R15_reg  : wordBits /* general purpose    */

mapping RegisterMapping: Register <-> registerAddressBits  = {
        PC    <-> 0b0000,
        SP    <-> 0b0001,
        SRCG1 <-> 0b0010,
        CG2   <-> 0b0011,
        R4    <-> 0b0100,
        R5    <-> 0b0101,
        R6    <-> 0b0110,
        R7    <-> 0b0111,
        R8    <-> 0b1000,
        R9    <-> 0b1001,
        R10   <-> 0b1010,
        R11   <-> 0b1011,
        R12   <-> 0b1100,
        R13   <-> 0b1101,
        R14   <-> 0b1110,
        R15   <-> 0b1111,
}

val init_base_regs: unit -> unit
function init_base_regs() = {
    // [previously undefined]
    PC_reg     = undefined; /* needs to be the address located at 0FFFEh */
    SP_reg     = undefined; /* needs to be initialized by the user */
    SRCG1_reg  = undefined;
    CG2_reg    = undefined;
    R4_reg     = undefined;
    R5_reg     = undefined;
    R6_reg     = undefined;
    R7_reg     = undefined;
    R8_reg     = undefined;
    R9_reg     = undefined;
    R10_reg    = undefined;
    R11_reg    = undefined;
    R12_reg    = undefined;
    R13_reg    = undefined;
    R14_reg    = undefined;
    R15_reg    = undefined;
}