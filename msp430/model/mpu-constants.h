// let mpu_reg_base : Address = 0x05A0 // MSP430FR5969
#define mpu_reg_base 0x05A0

// MPU control register 0 (MPUCTL0)
// let MPUPW_high   = 15 // allows writing MPU registers when correct
// let MPUPW_low    = 8
// let MPUSEGIE_bit = 4
// let MPULOCK_bit  = 1  // if 1 prevents writes on MPU regs, except MPUCTL1 and IPE regs
// let MPUENA_bit   = 0  // enable MPU

#define MPUPW_high  15
#define MPUPW_low   8
#define MPULOCK_bit 1

// MPU control register 1 (MPUCTL1)
// let MPUSEGIPIFG_bit = 4
// let MPUSEGIIFG_bit  = 3
// let MPUSEG3IFG_bit  = 2
// let MPUSEG2IFG_bit  = 1
// let MPUSEG1IFG_bit  = 0

// MPU segmentation access register (MPUSAM)
// let MPUSEGIE_bit = 4
// let MPULOCK_bit = 1

// IPE control register (MPUIPC0)
/* let MPUIPLOCK_bit = 7 // if 1, prevents writes on IPE registers */
/* let MPUIPENA_bit  = 6 // enable IPE */
/* let MPUIPVS_bit   = 5 // violation select: wether a PUC occurs on IPE violations */
#define MPUIPLOCK_bit 7
#define MPUIPENA_bit  6
#define MPUIPVS_bit   5
