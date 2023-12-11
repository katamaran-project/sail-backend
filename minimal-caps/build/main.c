#include "sail.h"
#include "rts.h"
#include "elf.h"
#ifdef __cplusplus
extern "C" {
#endif

// enum write_kind
enum zwrite_kind { zWrite_plain, zWrite_conditional, zWrite_release, zWrite_exclusive, zWrite_exclusive_release, zWrite_RISCV_release, zWrite_RISCV_strong_release, zWrite_RISCV_conditional, zWrite_RISCV_conditional_release, zWrite_RISCV_conditional_strong_release, zWrite_X86_locked };

static bool eq_zwrite_kind(enum zwrite_kind op1, enum zwrite_kind op2) { return op1 == op2; }

static enum zwrite_kind UNDEFINED(zwrite_kind)(unit u) { return zWrite_plain; }

// enum trans_kind
enum ztrans_kind { zTransaction_start, zTransaction_commit, zTransaction_abort };

static bool eq_ztrans_kind(enum ztrans_kind op1, enum ztrans_kind op2) { return op1 == op2; }

static enum ztrans_kind UNDEFINED(ztrans_kind)(unit u) { return zTransaction_start; }

// enum read_kind
enum zread_kind { zRead_plain, zRead_reserve, zRead_acquire, zRead_exclusive, zRead_exclusive_acquire, zRead_stream, zRead_ifetch, zRead_RISCV_acquire, zRead_RISCV_strong_acquire, zRead_RISCV_reserved, zRead_RISCV_reserved_acquire, zRead_RISCV_reserved_strong_acquire, zRead_X86_locked };

static bool eq_zread_kind(enum zread_kind op1, enum zread_kind op2) { return op1 == op2; }

static enum zread_kind UNDEFINED(zread_kind)(unit u) { return zRead_plain; }

// union exception
enum kind_zexception { Kind_zCINCOFFSETOnEnterCapability, Kind_zCSETBOUNDSOnEnterCapability, Kind_zCapabilityCursorCannotBeModified, Kind_zCapabilityDoesNotHaveWritePermission, Kind_zExpectedCapabilityRegisterContents, Kind_zExpectedNumberRegisterContents, Kind_zFail };

struct zexception {
  enum kind_zexception kind;
  union {
    struct { unit zCINCOFFSETOnEnterCapability; };
    struct { unit zCSETBOUNDSOnEnterCapability; };
    struct { unit zCapabilityCursorCannotBeModified; };
    struct { unit zCapabilityDoesNotHaveWritePermission; };
    struct { unit zExpectedCapabilityRegisterContents; };
    struct { unit zExpectedNumberRegisterContents; };
    struct { unit zFail; };
  };
};

static void CREATE(zexception)(struct zexception *op)
{
  op->kind = Kind_zCINCOFFSETOnEnterCapability;

}

static void RECREATE(zexception)(struct zexception *op) {}

static void KILL(zexception)(struct zexception *op)
{{};}

static void COPY(zexception)(struct zexception *rop, struct zexception op)
{
  {};
  rop->kind = op.kind;
  if (op.kind == Kind_zCINCOFFSETOnEnterCapability) {
    rop->zCINCOFFSETOnEnterCapability = op.zCINCOFFSETOnEnterCapability;
  } else if (op.kind == Kind_zCSETBOUNDSOnEnterCapability) {
    rop->zCSETBOUNDSOnEnterCapability = op.zCSETBOUNDSOnEnterCapability;
  } else if (op.kind == Kind_zCapabilityCursorCannotBeModified) {
    rop->zCapabilityCursorCannotBeModified = op.zCapabilityCursorCannotBeModified;
  } else if (op.kind == Kind_zCapabilityDoesNotHaveWritePermission) {
    rop->zCapabilityDoesNotHaveWritePermission = op.zCapabilityDoesNotHaveWritePermission;
  } else if (op.kind == Kind_zExpectedCapabilityRegisterContents) {
    rop->zExpectedCapabilityRegisterContents = op.zExpectedCapabilityRegisterContents;
  } else if (op.kind == Kind_zExpectedNumberRegisterContents) {
    rop->zExpectedNumberRegisterContents = op.zExpectedNumberRegisterContents;
  } else if (op.kind == Kind_zFail){
    rop->zFail = op.zFail;
  }
}

static bool EQUAL(zexception)(struct zexception op1, struct zexception op2) {
  if (op1.kind == Kind_zCINCOFFSETOnEnterCapability && op2.kind == Kind_zCINCOFFSETOnEnterCapability) {
    return EQUAL(unit)(op1.zCINCOFFSETOnEnterCapability, op2.zCINCOFFSETOnEnterCapability);
  } else if (op1.kind == Kind_zCSETBOUNDSOnEnterCapability && op2.kind == Kind_zCSETBOUNDSOnEnterCapability) {
    return EQUAL(unit)(op1.zCSETBOUNDSOnEnterCapability, op2.zCSETBOUNDSOnEnterCapability);
  } else if (op1.kind == Kind_zCapabilityCursorCannotBeModified && op2.kind == Kind_zCapabilityCursorCannotBeModified) {
    return EQUAL(unit)(op1.zCapabilityCursorCannotBeModified, op2.zCapabilityCursorCannotBeModified);
  } else if (op1.kind == Kind_zCapabilityDoesNotHaveWritePermission && op2.kind == Kind_zCapabilityDoesNotHaveWritePermission) {
    return EQUAL(unit)(op1.zCapabilityDoesNotHaveWritePermission, op2.zCapabilityDoesNotHaveWritePermission);
  } else if (op1.kind == Kind_zExpectedCapabilityRegisterContents && op2.kind == Kind_zExpectedCapabilityRegisterContents) {
    return EQUAL(unit)(op1.zExpectedCapabilityRegisterContents, op2.zExpectedCapabilityRegisterContents);
  } else if (op1.kind == Kind_zExpectedNumberRegisterContents && op2.kind == Kind_zExpectedNumberRegisterContents) {
    return EQUAL(unit)(op1.zExpectedNumberRegisterContents, op2.zExpectedNumberRegisterContents);
  } else if (op1.kind == Kind_zFail && op2.kind == Kind_zFail) {
    return EQUAL(unit)(op1.zFail, op2.zFail);
  } else return false;
}

static void zCINCOFFSETOnEnterCapability(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zCINCOFFSETOnEnterCapability;
  rop->zCINCOFFSETOnEnterCapability = op;
}

static void zCSETBOUNDSOnEnterCapability(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zCSETBOUNDSOnEnterCapability;
  rop->zCSETBOUNDSOnEnterCapability = op;
}

static void zCapabilityCursorCannotBeModified(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zCapabilityCursorCannotBeModified;
  rop->zCapabilityCursorCannotBeModified = op;
}

static void zCapabilityDoesNotHaveWritePermission(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zCapabilityDoesNotHaveWritePermission;
  rop->zCapabilityDoesNotHaveWritePermission = op;
}

static void zExpectedCapabilityRegisterContents(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zExpectedCapabilityRegisterContents;
  rop->zExpectedCapabilityRegisterContents = op;
}

static void zExpectedNumberRegisterContents(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zExpectedNumberRegisterContents;
  rop->zExpectedNumberRegisterContents = op;
}

static void zFail(struct zexception *rop, unit op)
{
  {}
  rop->kind = Kind_zFail;
  rop->zFail = op;
}

struct zexception *current_exception = NULL;
bool have_exception = false;
sail_string *throw_location = NULL;

// enum cache_op_kind
enum zcache_op_kind { zCache_op_D_IVAC, zCache_op_D_ISW, zCache_op_D_CSW, zCache_op_D_CISW, zCache_op_D_ZVA, zCache_op_D_CVAC, zCache_op_D_CVAU, zCache_op_D_CIVAC, zCache_op_I_IALLUIS, zCache_op_I_IALLU, zCache_op_I_IVAU };

static bool eq_zcache_op_kind(enum zcache_op_kind op1, enum zcache_op_kind op2) { return op1 == op2; }

static enum zcache_op_kind UNDEFINED(zcache_op_kind)(unit u) { return zCache_op_D_IVAC; }

// struct tuple_(%bv2, %bv2)
struct ztuple_z8z5bv2zCz0z5bv2z9 {
  uint64_t ztup0;
  uint64_t ztup1;
};

static void COPY(ztuple_z8z5bv2zCz0z5bv2z9)(struct ztuple_z8z5bv2zCz0z5bv2z9 *rop, const struct ztuple_z8z5bv2zCz0z5bv2z9 op) {
  rop->ztup0 = op.ztup0;
  rop->ztup1 = op.ztup1;
}

static bool EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(struct ztuple_z8z5bv2zCz0z5bv2z9 op1, struct ztuple_z8z5bv2zCz0z5bv2z9 op2) {
  return EQUAL(fbits)(op1.ztup0, op2.ztup0) && EQUAL(fbits)(op1.ztup1, op2.ztup1);
}


// struct tuple_(%bv2, %bv20)
struct ztuple_z8z5bv2zCz0z5bv20z9 {
  uint64_t ztup0;
  uint64_t ztup1;
};

static void COPY(ztuple_z8z5bv2zCz0z5bv20z9)(struct ztuple_z8z5bv2zCz0z5bv20z9 *rop, const struct ztuple_z8z5bv2zCz0z5bv20z9 op) {
  rop->ztup0 = op.ztup0;
  rop->ztup1 = op.ztup1;
}

static bool EQUAL(ztuple_z8z5bv2zCz0z5bv20z9)(struct ztuple_z8z5bv2zCz0z5bv20z9 op1, struct ztuple_z8z5bv2zCz0z5bv20z9 op2) {
  return EQUAL(fbits)(op1.ztup0, op2.ztup0) && EQUAL(fbits)(op1.ztup1, op2.ztup1);
}


// struct tuple_(%bv2, %bv2, %bv2)
struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 {
  uint64_t ztup0;
  uint64_t ztup1;
  uint64_t ztup2;
};

static void COPY(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 *rop, const struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op) {
  rop->ztup0 = op.ztup0;
  rop->ztup1 = op.ztup1;
  rop->ztup2 = op.ztup2;
}

static bool EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op1, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op2) {
  return EQUAL(fbits)(op1.ztup0, op2.ztup0) && EQUAL(fbits)(op1.ztup1, op2.ztup1) && EQUAL(fbits)(op1.ztup2, op2.ztup2);
}


// struct tuple_(%bv2, %bv2, %bv12)
struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 {
  uint64_t ztup0;
  uint64_t ztup1;
  uint64_t ztup2;
};

static void COPY(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 *rop, const struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op) {
  rop->ztup0 = op.ztup0;
  rop->ztup1 = op.ztup1;
  rop->ztup2 = op.ztup2;
}

static bool EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op1, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op2) {
  return EQUAL(fbits)(op1.ztup0, op2.ztup0) && EQUAL(fbits)(op1.ztup1, op2.ztup1) && EQUAL(fbits)(op1.ztup2, op2.ztup2);
}

// union ast
enum kind_zast { Kind_zADD, Kind_zADDI, Kind_zBNE, Kind_zCANDPERM, Kind_zCGETADDR, Kind_zCGETBASE, Kind_zCGETLEN, Kind_zCGETPERM, Kind_zCGETTAG, Kind_zCINCOFFSET, Kind_zCJAL, Kind_zCJALR, Kind_zCMOVE, Kind_zCSETBOUNDS, Kind_zCSETBOUNDSIMM, Kind_zFAIL, Kind_zJALR_CAP, Kind_zLD, Kind_zRET, Kind_zSD, Kind_zSLT, Kind_zSLTI, Kind_zSLTIU, Kind_zSLTU, Kind_zSUB };

struct zast {
  enum kind_zast kind;
  union {
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zADD; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zADDI; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zBNE; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zCANDPERM; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCGETADDR; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCGETBASE; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCGETLEN; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCGETPERM; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCGETTAG; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zCINCOFFSET; };
    struct { struct ztuple_z8z5bv2zCz0z5bv20z9 zCJAL; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zCJALR; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zCMOVE; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zCSETBOUNDS; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zCSETBOUNDSIMM; };
    struct { unit zFAIL; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2z9 zJALR_CAP; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zLD; };
    struct { unit zRET; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zSD; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zSLT; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zSLTI; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zSLTIU; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zSLTU; };
    struct { struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zSUB; };
  };
};

static void CREATE(zast)(struct zast *op)
{
  op->kind = Kind_zADD;

}

static void RECREATE(zast)(struct zast *op) {}

static void KILL(zast)(struct zast *op)
{{};}

static void COPY(zast)(struct zast *rop, struct zast op)
{
  {};
  rop->kind = op.kind;
  if (op.kind == Kind_zADD) {
    rop->zADD = op.zADD;
  } else if (op.kind == Kind_zADDI) {
    rop->zADDI = op.zADDI;
  } else if (op.kind == Kind_zBNE) {
    rop->zBNE = op.zBNE;
  } else if (op.kind == Kind_zCANDPERM) {
    rop->zCANDPERM = op.zCANDPERM;
  } else if (op.kind == Kind_zCGETADDR) {
    rop->zCGETADDR = op.zCGETADDR;
  } else if (op.kind == Kind_zCGETBASE) {
    rop->zCGETBASE = op.zCGETBASE;
  } else if (op.kind == Kind_zCGETLEN) {
    rop->zCGETLEN = op.zCGETLEN;
  } else if (op.kind == Kind_zCGETPERM) {
    rop->zCGETPERM = op.zCGETPERM;
  } else if (op.kind == Kind_zCGETTAG) {
    rop->zCGETTAG = op.zCGETTAG;
  } else if (op.kind == Kind_zCINCOFFSET) {
    rop->zCINCOFFSET = op.zCINCOFFSET;
  } else if (op.kind == Kind_zCJAL) {
    rop->zCJAL = op.zCJAL;
  } else if (op.kind == Kind_zCJALR) {
    rop->zCJALR = op.zCJALR;
  } else if (op.kind == Kind_zCMOVE) {
    rop->zCMOVE = op.zCMOVE;
  } else if (op.kind == Kind_zCSETBOUNDS) {
    rop->zCSETBOUNDS = op.zCSETBOUNDS;
  } else if (op.kind == Kind_zCSETBOUNDSIMM) {
    rop->zCSETBOUNDSIMM = op.zCSETBOUNDSIMM;
  } else if (op.kind == Kind_zFAIL) {
    rop->zFAIL = op.zFAIL;
  } else if (op.kind == Kind_zJALR_CAP) {
    rop->zJALR_CAP = op.zJALR_CAP;
  } else if (op.kind == Kind_zLD) {
    rop->zLD = op.zLD;
  } else if (op.kind == Kind_zRET) {
    rop->zRET = op.zRET;
  } else if (op.kind == Kind_zSD) {
    rop->zSD = op.zSD;
  } else if (op.kind == Kind_zSLT) {
    rop->zSLT = op.zSLT;
  } else if (op.kind == Kind_zSLTI) {
    rop->zSLTI = op.zSLTI;
  } else if (op.kind == Kind_zSLTIU) {
    rop->zSLTIU = op.zSLTIU;
  } else if (op.kind == Kind_zSLTU) {
    rop->zSLTU = op.zSLTU;
  } else if (op.kind == Kind_zSUB){
    rop->zSUB = op.zSUB;
  }
}

static bool EQUAL(zast)(struct zast op1, struct zast op2) {
  if (op1.kind == Kind_zADD && op2.kind == Kind_zADD) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zADD, op2.zADD);
  } else if (op1.kind == Kind_zADDI && op2.kind == Kind_zADDI) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zADDI, op2.zADDI);
  } else if (op1.kind == Kind_zBNE && op2.kind == Kind_zBNE) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zBNE, op2.zBNE);
  } else if (op1.kind == Kind_zCANDPERM && op2.kind == Kind_zCANDPERM) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zCANDPERM, op2.zCANDPERM);
  } else if (op1.kind == Kind_zCGETADDR && op2.kind == Kind_zCGETADDR) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCGETADDR, op2.zCGETADDR);
  } else if (op1.kind == Kind_zCGETBASE && op2.kind == Kind_zCGETBASE) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCGETBASE, op2.zCGETBASE);
  } else if (op1.kind == Kind_zCGETLEN && op2.kind == Kind_zCGETLEN) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCGETLEN, op2.zCGETLEN);
  } else if (op1.kind == Kind_zCGETPERM && op2.kind == Kind_zCGETPERM) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCGETPERM, op2.zCGETPERM);
  } else if (op1.kind == Kind_zCGETTAG && op2.kind == Kind_zCGETTAG) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCGETTAG, op2.zCGETTAG);
  } else if (op1.kind == Kind_zCINCOFFSET && op2.kind == Kind_zCINCOFFSET) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zCINCOFFSET, op2.zCINCOFFSET);
  } else if (op1.kind == Kind_zCJAL && op2.kind == Kind_zCJAL) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv20z9)(op1.zCJAL, op2.zCJAL);
  } else if (op1.kind == Kind_zCJALR && op2.kind == Kind_zCJALR) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zCJALR, op2.zCJALR);
  } else if (op1.kind == Kind_zCMOVE && op2.kind == Kind_zCMOVE) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zCMOVE, op2.zCMOVE);
  } else if (op1.kind == Kind_zCSETBOUNDS && op2.kind == Kind_zCSETBOUNDS) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zCSETBOUNDS, op2.zCSETBOUNDS);
  } else if (op1.kind == Kind_zCSETBOUNDSIMM && op2.kind == Kind_zCSETBOUNDSIMM) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zCSETBOUNDSIMM, op2.zCSETBOUNDSIMM);
  } else if (op1.kind == Kind_zFAIL && op2.kind == Kind_zFAIL) {
    return EQUAL(unit)(op1.zFAIL, op2.zFAIL);
  } else if (op1.kind == Kind_zJALR_CAP && op2.kind == Kind_zJALR_CAP) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2z9)(op1.zJALR_CAP, op2.zJALR_CAP);
  } else if (op1.kind == Kind_zLD && op2.kind == Kind_zLD) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zLD, op2.zLD);
  } else if (op1.kind == Kind_zRET && op2.kind == Kind_zRET) {
    return EQUAL(unit)(op1.zRET, op2.zRET);
  } else if (op1.kind == Kind_zSD && op2.kind == Kind_zSD) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zSD, op2.zSD);
  } else if (op1.kind == Kind_zSLT && op2.kind == Kind_zSLT) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zSLT, op2.zSLT);
  } else if (op1.kind == Kind_zSLTI && op2.kind == Kind_zSLTI) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zSLTI, op2.zSLTI);
  } else if (op1.kind == Kind_zSLTIU && op2.kind == Kind_zSLTIU) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9)(op1.zSLTIU, op2.zSLTIU);
  } else if (op1.kind == Kind_zSLTU && op2.kind == Kind_zSLTU) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zSLTU, op2.zSLTU);
  } else if (op1.kind == Kind_zSUB && op2.kind == Kind_zSUB) {
    return EQUAL(ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9)(op1.zSUB, op2.zSUB);
  } else return false;
}

static void zADD(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zADD;
  rop->zADD = op;
}

static void zADDI(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zADDI;
  rop->zADDI = op;
}

static void zBNE(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zBNE;
  rop->zBNE = op;
}

static void zCANDPERM(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCANDPERM;
  rop->zCANDPERM = op;
}

static void zCGETADDR(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCGETADDR;
  rop->zCGETADDR = op;
}

static void zCGETBASE(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCGETBASE;
  rop->zCGETBASE = op;
}

static void zCGETLEN(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCGETLEN;
  rop->zCGETLEN = op;
}

static void zCGETPERM(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCGETPERM;
  rop->zCGETPERM = op;
}

static void zCGETTAG(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCGETTAG;
  rop->zCGETTAG = op;
}

static void zCINCOFFSET(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCINCOFFSET;
  rop->zCINCOFFSET = op;
}

static void zCJAL(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv20z9 op)
{
  {}
  rop->kind = Kind_zCJAL;
  rop->zCJAL = op;
}

static void zCJALR(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zCJALR;
  rop->zCJALR = op;
}

static void zCMOVE(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCMOVE;
  rop->zCMOVE = op;
}

static void zCSETBOUNDS(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zCSETBOUNDS;
  rop->zCSETBOUNDS = op;
}

static void zCSETBOUNDSIMM(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zCSETBOUNDSIMM;
  rop->zCSETBOUNDSIMM = op;
}

static void zFAIL(struct zast *rop, unit op)
{
  {}
  rop->kind = Kind_zFAIL;
  rop->zFAIL = op;
}

static void zJALR_CAP(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zJALR_CAP;
  rop->zJALR_CAP = op;
}

static void zLD(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zLD;
  rop->zLD = op;
}

static void zRET(struct zast *rop, unit op)
{
  {}
  rop->kind = Kind_zRET;
  rop->zRET = op;
}

static void zSD(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zSD;
  rop->zSD = op;
}

static void zSLT(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zSLT;
  rop->zSLT = op;
}

static void zSLTI(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zSLTI;
  rop->zSLTI = op;
}

static void zSLTIU(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 op)
{
  {}
  rop->kind = Kind_zSLTIU;
  rop->zSLTIU = op;
}

static void zSLTU(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zSLTU;
  rop->zSLTU = op;
}

static void zSUB(struct zast *rop, struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 op)
{
  {}
  rop->kind = Kind_zSUB;
  rop->zSUB = op;
}























// union option<Uast<>>
enum kind_zoptionzIUastzIzKzK { Kind_zNonezIUastzIzKzK, Kind_zSomezIUastzIzKzK };

struct zoptionzIUastzIzKzK {
  enum kind_zoptionzIUastzIzKzK kind;
  union {
    struct { unit zNonezIUastzIzKzK; };
    struct { struct zast zSomezIUastzIzKzK; };
  };
};

static void CREATE(zoptionzIUastzIzKzK)(struct zoptionzIUastzIzKzK *op)
{
  op->kind = Kind_zNonezIUastzIzKzK;

}

static void RECREATE(zoptionzIUastzIzKzK)(struct zoptionzIUastzIzKzK *op) {}

static void KILL(zoptionzIUastzIzKzK)(struct zoptionzIUastzIzKzK *op)
{
  if (op->kind == Kind_zSomezIUastzIzKzK){
    KILL(zast)(&op->zSomezIUastzIzKzK);
  };
}

static void COPY(zoptionzIUastzIzKzK)(struct zoptionzIUastzIzKzK *rop, struct zoptionzIUastzIzKzK op)
{
  if (rop->kind == Kind_zSomezIUastzIzKzK){
    KILL(zast)(&rop->zSomezIUastzIzKzK);
  };
  rop->kind = op.kind;
  if (op.kind == Kind_zNonezIUastzIzKzK) {
    rop->zNonezIUastzIzKzK = op.zNonezIUastzIzKzK;
  } else if (op.kind == Kind_zSomezIUastzIzKzK){
    CREATE(zast)(&rop->zSomezIUastzIzKzK); COPY(zast)(&rop->zSomezIUastzIzKzK, op.zSomezIUastzIzKzK);
  }
}

static bool EQUAL(zoptionzIUastzIzKzK)(struct zoptionzIUastzIzKzK op1, struct zoptionzIUastzIzKzK op2) {
  if (op1.kind == Kind_zNonezIUastzIzKzK && op2.kind == Kind_zNonezIUastzIzKzK) {
    return EQUAL(unit)(op1.zNonezIUastzIzKzK, op2.zNonezIUastzIzKzK);
  } else if (op1.kind == Kind_zSomezIUastzIzKzK && op2.kind == Kind_zSomezIUastzIzKzK) {
    return EQUAL(zast)(op1.zSomezIUastzIzKzK, op2.zSomezIUastzIzKzK);
  } else return false;
}

static void zNonezIUastzIzKzK(struct zoptionzIUastzIzKzK *rop, unit op)
{
  if (rop->kind == Kind_zSomezIUastzIzKzK){
    KILL(zast)(&rop->zSomezIUastzIzKzK);
  }
  rop->kind = Kind_zNonezIUastzIzKzK;
  rop->zNonezIUastzIzKzK = op;
}

static void zSomezIUastzIzKzK(struct zoptionzIUastzIzKzK *rop, struct zast op)
{
  if (rop->kind == Kind_zSomezIUastzIzKzK){
    KILL(zast)(&rop->zSomezIUastzIzKzK);
  }
  rop->kind = Kind_zSomezIUastzIzKzK;
  CREATE(zast)(&rop->zSomezIUastzIzKzK);
  COPY(zast)(&rop->zSomezIUastzIzKzK, op);

}

// enum a64_barrier_type
enum za64_barrier_type { zA64_barrier_all, zA64_barrier_LD, zA64_barrier_ST };

static bool eq_za64_barrier_type(enum za64_barrier_type op1, enum za64_barrier_type op2) { return op1 == op2; }

static enum za64_barrier_type UNDEFINED(za64_barrier_type)(unit u) { return zA64_barrier_all; }

// enum a64_barrier_domain
enum za64_barrier_domain { zA64_FullShare, zA64_InnerShare, zA64_OuterShare, zA64_NonShare };

static bool eq_za64_barrier_domain(enum za64_barrier_domain op1, enum za64_barrier_domain op2) { return op1 == op2; }

static enum za64_barrier_domain UNDEFINED(za64_barrier_domain)(unit u) { return zA64_FullShare; }

// enum Permission
enum zPermission { zO, zR, zRW, zE };

static bool eq_zPermission(enum zPermission op1, enum zPermission op2) { return op1 == op2; }

static enum zPermission UNDEFINED(zPermission)(unit u) { return zO; }

// struct Capability
struct zCapability {
  uint64_t zcap_begin;
  uint64_t zcap_cursor;
  uint64_t zcap_end;
  enum zPermission zcap_permission;
};

static void COPY(zCapability)(struct zCapability *rop, const struct zCapability op) {
  rop->zcap_begin = op.zcap_begin;
  rop->zcap_cursor = op.zcap_cursor;
  rop->zcap_end = op.zcap_end;
  rop->zcap_permission = op.zcap_permission;
}

static bool EQUAL(zCapability)(struct zCapability op1, struct zCapability op2) {
  return EQUAL(fbits)(op1.zcap_begin, op2.zcap_begin) && EQUAL(fbits)(op1.zcap_cursor, op2.zcap_cursor) && EQUAL(fbits)(op1.zcap_end, op2.zcap_end) && EQUAL(zPermission)(op1.zcap_permission, op2.zcap_permission);
}

// union word
enum kind_zword { Kind_zCap, Kind_zNum };

struct zword {
  enum kind_zword kind;
  union {
    struct { struct zCapability zCap; };
    struct { uint64_t zNum; };
  };
};

static void CREATE(zword)(struct zword *op)
{
  op->kind = Kind_zCap;

}

static void RECREATE(zword)(struct zword *op) {}

static void KILL(zword)(struct zword *op)
{{};}

static void COPY(zword)(struct zword *rop, struct zword op)
{
  {};
  rop->kind = op.kind;
  if (op.kind == Kind_zCap) {
    rop->zCap = op.zCap;
  } else if (op.kind == Kind_zNum){
    rop->zNum = op.zNum;
  }
}

static bool EQUAL(zword)(struct zword op1, struct zword op2) {
  if (op1.kind == Kind_zCap && op2.kind == Kind_zCap) {
    return EQUAL(zCapability)(op1.zCap, op2.zCap);
  } else if (op1.kind == Kind_zNum && op2.kind == Kind_zNum) {
    return EQUAL(fbits)(op1.zNum, op2.zNum);
  } else return false;
}

static void zCap(struct zword *rop, struct zCapability op)
{
  {}
  rop->kind = Kind_zCap;
  rop->zCap = op;
}

static void zNum(struct zword *rop, uint64_t op)
{
  {}
  rop->kind = Kind_zNum;
  rop->zNum = op;
}







struct node_zz5listz8z5unionz0zzwordz9 {
  unsigned int rc;
  struct zword hd;
  struct node_zz5listz8z5unionz0zzwordz9 *tl;
};
typedef struct node_zz5listz8z5unionz0zzwordz9 *zz5listz8z5unionz0zzwordz9;

static void CREATE(zz5listz8z5unionz0zzwordz9)(zz5listz8z5unionz0zzwordz9 *rop) { *rop = NULL; }

static void internal_inc_zz5listz8z5unionz0zzwordz9(zz5listz8z5unionz0zzwordz9 l) {
  if (l == NULL) return;
  l->rc += 1;
}

static void internal_dec_zz5listz8z5unionz0zzwordz9(zz5listz8z5unionz0zzwordz9 l) {
  if (l == NULL) return;
  l->rc -= 1;
}

static void KILL(zz5listz8z5unionz0zzwordz9)(zz5listz8z5unionz0zzwordz9 *rop) {
  if (*rop == NULL) return;
  if ((*rop)->rc >= 1) {
    (*rop)->rc -= 1;
  }
  zz5listz8z5unionz0zzwordz9 node = *rop;
  while (node != NULL && node->rc == 0) {
    KILL(zword)(&node->hd);
    zz5listz8z5unionz0zzwordz9 next = node->tl;
    sail_free(node);
    node = next;
    internal_dec_zz5listz8z5unionz0zzwordz9(node);
  }
}

static void RECREATE(zz5listz8z5unionz0zzwordz9)(zz5listz8z5unionz0zzwordz9 *rop) { KILL(zz5listz8z5unionz0zzwordz9)(rop); *rop = NULL; }

static void COPY(zz5listz8z5unionz0zzwordz9)(zz5listz8z5unionz0zzwordz9 *rop, zz5listz8z5unionz0zzwordz9 op) {
  internal_inc_zz5listz8z5unionz0zzwordz9(op);
  KILL(zz5listz8z5unionz0zzwordz9)(rop);
  *rop = op;
}

static void zconsz3z5unionz0zzword(zz5listz8z5unionz0zzwordz9 *rop, struct zword x, zz5listz8z5unionz0zzwordz9 xs) {
  bool same = *rop == xs;
  *rop = sail_new(struct node_zz5listz8z5unionz0zzwordz9);
  (*rop)->rc = 1;
  CREATE(zword)(&(*rop)->hd);
  COPY(zword)(&(*rop)->hd, x);
  if (!same) internal_inc_zz5listz8z5unionz0zzwordz9(xs);
  (*rop)->tl = xs;
}

static void pick_zword(struct zword *x, const zz5listz8z5unionz0zzwordz9 xs) { COPY(zword)(x, xs->hd); }

static bool EQUAL(zz5listz8z5unionz0zzwordz9)(const zz5listz8z5unionz0zzwordz9 op1, const zz5listz8z5unionz0zzwordz9 op2) {
  if (op1 == NULL && op2 == NULL) { return true; };
  if (op1 == NULL || op2 == NULL) { return false; };
  return EQUAL(zword)(op1->hd, op2->hd) && EQUAL(zz5listz8z5unionz0zzwordz9)(op1->tl, op2->tl);
}

static void UNDEFINED(zz5listz8z5unionz0zzwordz9)(zz5listz8z5unionz0zzwordz9 *rop, struct zword u) {
  *rop = NULL;
}



struct node_zz5listz8z5enumz0zzPermissionz9 {
  unsigned int rc;
  enum zPermission hd;
  struct node_zz5listz8z5enumz0zzPermissionz9 *tl;
};
typedef struct node_zz5listz8z5enumz0zzPermissionz9 *zz5listz8z5enumz0zzPermissionz9;

static void CREATE(zz5listz8z5enumz0zzPermissionz9)(zz5listz8z5enumz0zzPermissionz9 *rop) { *rop = NULL; }

static void internal_inc_zz5listz8z5enumz0zzPermissionz9(zz5listz8z5enumz0zzPermissionz9 l) {
  if (l == NULL) return;
  l->rc += 1;
}

static void internal_dec_zz5listz8z5enumz0zzPermissionz9(zz5listz8z5enumz0zzPermissionz9 l) {
  if (l == NULL) return;
  l->rc -= 1;
}

static void KILL(zz5listz8z5enumz0zzPermissionz9)(zz5listz8z5enumz0zzPermissionz9 *rop) {
  if (*rop == NULL) return;
  if ((*rop)->rc >= 1) {
    (*rop)->rc -= 1;
  }
  zz5listz8z5enumz0zzPermissionz9 node = *rop;
  while (node != NULL && node->rc == 0) {
    zz5listz8z5enumz0zzPermissionz9 next = node->tl;
    sail_free(node);
    node = next;
    internal_dec_zz5listz8z5enumz0zzPermissionz9(node);
  }
}

static void RECREATE(zz5listz8z5enumz0zzPermissionz9)(zz5listz8z5enumz0zzPermissionz9 *rop) { KILL(zz5listz8z5enumz0zzPermissionz9)(rop); *rop = NULL; }

static void COPY(zz5listz8z5enumz0zzPermissionz9)(zz5listz8z5enumz0zzPermissionz9 *rop, zz5listz8z5enumz0zzPermissionz9 op) {
  internal_inc_zz5listz8z5enumz0zzPermissionz9(op);
  KILL(zz5listz8z5enumz0zzPermissionz9)(rop);
  *rop = op;
}

static void zconsz3z5enumz0zzPermission(zz5listz8z5enumz0zzPermissionz9 *rop, enum zPermission x, zz5listz8z5enumz0zzPermissionz9 xs) {
  bool same = *rop == xs;
  *rop = sail_new(struct node_zz5listz8z5enumz0zzPermissionz9);
  (*rop)->rc = 1;
  (*rop)->hd = x;
  if (!same) internal_inc_zz5listz8z5enumz0zzPermissionz9(xs);
  (*rop)->tl = xs;
}

static enum zPermission pick_zPermission(const zz5listz8z5enumz0zzPermissionz9 xs) { return xs->hd; }

static bool EQUAL(zz5listz8z5enumz0zzPermissionz9)(const zz5listz8z5enumz0zzPermissionz9 op1, const zz5listz8z5enumz0zzPermissionz9 op2) {
  if (op1 == NULL && op2 == NULL) { return true; };
  if (op1 == NULL || op2 == NULL) { return false; };
  return EQUAL(zPermission)(op1->hd, op2->hd) && EQUAL(zz5listz8z5enumz0zzPermissionz9)(op1->tl, op2->tl);
}

static void UNDEFINED(zz5listz8z5enumz0zzPermissionz9)(zz5listz8z5enumz0zzPermissionz9 *rop, enum zPermission u) {
  *rop = NULL;
}





















bool zneq_bits(lbits zx, lbits zy)
{
  __label__ end_function_1, end_block_exception_2;

  bool zcbz30;
  bool zgaz30;
  zgaz30 = eq_bits(zx, zy);
  zcbz30 = not(zgaz30);

end_function_1: ;
  return zcbz30;
end_block_exception_2: ;

  return false;
}







































void zconcat_str_dec(sail_string *rop, const_sail_string, sail_int);

void zconcat_str_dec(sail_string *zcbz31, const_sail_string zstr, sail_int zx)
{
  __label__ end_function_4, end_block_exception_5, end_function_259;

  sail_string zgaz31;
  CREATE(sail_string)(&zgaz31);
  dec_str(&zgaz31, zx);
  concat_str((*(&zcbz31)), zstr, zgaz31);
  KILL(sail_string)(&zgaz31);
end_function_4: ;
  goto end_function_259;
end_block_exception_5: ;
  goto end_function_259;
end_function_259: ;
}



int64_t zaddress_sizze;
static void create_letbind_0(void) {


  int64_t zgsz30;
  zgsz30 = INT64_C(64);
  zaddress_sizze = zgsz30;

let_end_6: ;
}
static void kill_letbind_0(void) {
}

int64_t zinteger_sizze;
static void create_letbind_1(void) {


  int64_t zgsz31;
  zgsz31 = INT64_C(64);
  zinteger_sizze = zgsz31;

let_end_7: ;
}
static void kill_letbind_1(void) {
}

int64_t zcap_sizze;
static void create_letbind_2(void) {


  int64_t zgsz32;
  zgsz32 = INT64_C(194);
  zcap_sizze = zgsz32;

let_end_8: ;
}
static void kill_letbind_2(void) {
}

enum zPermission zundefined_Permission(unit);

enum zPermission zundefined_Permission(unit zgsz33)
{
  __label__ end_function_10, end_block_exception_11;

  enum zPermission zcbz32;
  zz5listz8z5enumz0zzPermissionz9 zgsz34;
  CREATE(zz5listz8z5enumz0zzPermissionz9)(&zgsz34);
  zconsz3z5enumz0zzPermission(&zgsz34, zE, zgsz34);
  zconsz3z5enumz0zzPermission(&zgsz34, zRW, zgsz34);
  zconsz3z5enumz0zzPermission(&zgsz34, zR, zgsz34);
  zconsz3z5enumz0zzPermission(&zgsz34, zO, zgsz34);
  zcbz32 = pick_zPermission(zgsz34);
  KILL(zz5listz8z5enumz0zzPermissionz9)(&zgsz34);
end_function_10: ;
  return zcbz32;
end_block_exception_11: ;

  return zE;
}

struct zCapability zundefined_Capability(unit);

struct zCapability zundefined_Capability(unit zgsz35)
{
  __label__ end_function_13, end_block_exception_14;

  struct zCapability zcbz33;
  enum zPermission zgaz32;
  zgaz32 = zundefined_Permission(UNIT);
  uint64_t zgaz33;
  {
    sail_int zgsz3303;
    CREATE(sail_int)(&zgsz3303);
    CONVERT_OF(sail_int, mach_int)(&zgsz3303, INT64_C(64));
    lbits zgsz3304;
    CREATE(lbits)(&zgsz3304);
    UNDEFINED(lbits)(&zgsz3304, zgsz3303);
    zgaz33 = CONVERT_OF(fbits, lbits)(zgsz3304, true);
    KILL(lbits)(&zgsz3304);
    KILL(sail_int)(&zgsz3303);
  }
  uint64_t zgaz34;
  {
    sail_int zgsz3301;
    CREATE(sail_int)(&zgsz3301);
    CONVERT_OF(sail_int, mach_int)(&zgsz3301, INT64_C(64));
    lbits zgsz3302;
    CREATE(lbits)(&zgsz3302);
    UNDEFINED(lbits)(&zgsz3302, zgsz3301);
    zgaz34 = CONVERT_OF(fbits, lbits)(zgsz3302, true);
    KILL(lbits)(&zgsz3302);
    KILL(sail_int)(&zgsz3301);
  }
  uint64_t zgaz35;
  {
    sail_int zgsz3299;
    CREATE(sail_int)(&zgsz3299);
    CONVERT_OF(sail_int, mach_int)(&zgsz3299, INT64_C(64));
    lbits zgsz3300;
    CREATE(lbits)(&zgsz3300);
    UNDEFINED(lbits)(&zgsz3300, zgsz3299);
    zgaz35 = CONVERT_OF(fbits, lbits)(zgsz3300, true);
    KILL(lbits)(&zgsz3300);
    KILL(sail_int)(&zgsz3299);
  }
  struct zCapability zgsz36;
  zgsz36.zcap_begin = zgaz33;
  zgsz36.zcap_cursor = zgaz35;
  zgsz36.zcap_end = zgaz34;
  zgsz36.zcap_permission = zgaz32;
  zcbz33 = zgsz36;





end_function_13: ;
  return zcbz33;
end_block_exception_14: ;
  struct zCapability zcbz337 = { .zcap_begin = UINT64_C(0xdeadc0de), .zcap_cursor = UINT64_C(0xdeadc0de), .zcap_end = UINT64_C(0xdeadc0de), .zcap_permission = zE };
  return zcbz337;
}

void zundefined_word(struct zword *rop, unit);

void zundefined_word(struct zword *zcbz34, unit zgsz37)
{
  __label__ end_function_16, end_block_exception_17, end_function_258;

  struct zCapability zu_0;
  zu_0 = zundefined_Capability(UNIT);
  uint64_t zu_1;
  {
    sail_int zgsz3305;
    CREATE(sail_int)(&zgsz3305);
    CONVERT_OF(sail_int, mach_int)(&zgsz3305, INT64_C(64));
    lbits zgsz3306;
    CREATE(lbits)(&zgsz3306);
    UNDEFINED(lbits)(&zgsz3306, zgsz3305);
    zu_1 = CONVERT_OF(fbits, lbits)(zgsz3306, true);
    KILL(lbits)(&zgsz3306);
    KILL(sail_int)(&zgsz3305);
  }
  zz5listz8z5unionz0zzwordz9 zgaz38;
  CREATE(zz5listz8z5unionz0zzwordz9)(&zgaz38);
  {
    struct zword zgaz36;
    CREATE(zword)(&zgaz36);
    zCap(&zgaz36, zu_0);
    struct zword zgaz37;
    CREATE(zword)(&zgaz37);
    zNum(&zgaz37, zu_1);
    zz5listz8z5unionz0zzwordz9 zgsz38;
    CREATE(zz5listz8z5unionz0zzwordz9)(&zgsz38);
    zconsz3z5unionz0zzword(&zgsz38, zgaz37, zgsz38);
    zconsz3z5unionz0zzword(&zgsz38, zgaz36, zgsz38);
    COPY(zz5listz8z5unionz0zzwordz9)(&zgaz38, zgsz38);
    KILL(zz5listz8z5unionz0zzwordz9)(&zgsz38);
    KILL(zword)(&zgaz37);
    KILL(zword)(&zgaz36);
  }
  pick_zword((*(&zcbz34)), zgaz38);
  KILL(zz5listz8z5unionz0zzwordz9)(&zgaz38);


end_function_16: ;
  goto end_function_258;
end_block_exception_17: ;
  goto end_function_258;
end_function_258: ;
}

// register PC
struct zCapability zPC;

// register R0
struct zword zR0;

// register R1
struct zword zR1;

// register R2
struct zword zR2;

// register R3
struct zword zR3;

struct zCapability znextPC(unit);

struct zCapability znextPC(unit zgsz39)
{
  __label__ end_function_19, end_block_exception_20;

  struct zCapability zcbz35;
  uint64_t zcursor;
  zcursor = zPC.zcap_cursor;
  uint64_t zgaz39;
  {
    lbits zgsz3307;
    CREATE(lbits)(&zgsz3307);
    CONVERT_OF(lbits, fbits)(&zgsz3307, zcursor, UINT64_C(64) , true);
    sail_int zgsz3308;
    CREATE(sail_int)(&zgsz3308);
    CONVERT_OF(sail_int, mach_int)(&zgsz3308, INT64_C(4));
    lbits zgsz3309;
    CREATE(lbits)(&zgsz3309);
    add_bits_int(&zgsz3309, zgsz3307, zgsz3308);
    zgaz39 = CONVERT_OF(fbits, lbits)(zgsz3309, true);
    KILL(lbits)(&zgsz3309);
    KILL(sail_int)(&zgsz3308);
    KILL(lbits)(&zgsz3307);
  }
  struct zCapability zgsz310;
  zgsz310 = zPC;
  zgsz310.zcap_cursor = zgaz39;
  zcbz35 = zgsz310;



end_function_19: ;
  return zcbz35;
end_block_exception_20: ;
  struct zCapability zcbz338 = { .zcap_begin = UINT64_C(0xdeadc0de), .zcap_cursor = UINT64_C(0xdeadc0de), .zcap_end = UINT64_C(0xdeadc0de), .zcap_permission = zE };
  return zcbz338;
}

unit zupdatePC(unit);

unit zupdatePC(unit zgsz311)
{
  __label__ end_function_22, end_block_exception_23;

  unit zcbz36;
  struct zCapability zc;
  zc = znextPC(UNIT);
  zPC = zc;
  zcbz36 = UNIT;

end_function_22: ;
  return zcbz36;
end_block_exception_23: ;

  return UNIT;
}

unit zaddPC(uint64_t);

unit zaddPC(uint64_t zoffset)
{
  __label__ end_function_25, end_block_exception_26;

  unit zcbz37;
  uint64_t zcursor;
  zcursor = zPC.zcap_cursor;
  uint64_t zgaz310;
  {
    lbits zgsz3310;
    CREATE(lbits)(&zgsz3310);
    CONVERT_OF(lbits, fbits)(&zgsz3310, zcursor, UINT64_C(64) , true);
    lbits zgsz3311;
    CREATE(lbits)(&zgsz3311);
    CONVERT_OF(lbits, fbits)(&zgsz3311, zoffset, UINT64_C(64) , true);
    lbits zgsz3312;
    CREATE(lbits)(&zgsz3312);
    add_bits(&zgsz3312, zgsz3310, zgsz3311);
    zgaz310 = CONVERT_OF(fbits, lbits)(zgsz3312, true);
    KILL(lbits)(&zgsz3312);
    KILL(lbits)(&zgsz3311);
    KILL(lbits)(&zgsz3310);
  }
  struct zCapability zgsz312;
  zgsz312 = zPC;
  zgsz312.zcap_cursor = zgaz310;
  zPC = zgsz312;
  zcbz37 = UNIT;



end_function_25: ;
  return zcbz37;
end_block_exception_26: ;

  return UNIT;
}

bool zMEMw(uint64_t, struct zword);

bool zMEMw(uint64_t zaddr, struct zword zv)
{
  __label__ end_function_28, end_block_exception_29;

  bool zcbz38;
  zcbz38 = true;
end_function_28: ;
  return zcbz38;
end_block_exception_29: ;

  return false;
}

void zMEMr(struct zword *rop, uint64_t);

void zMEMr(struct zword *zcbz39, uint64_t zaddr)
{
  __label__ end_function_31, end_block_exception_32, end_function_257;

  uint64_t zgaz311;
  {
    sail_int zgsz3313;
    CREATE(sail_int)(&zgsz3313);
    CONVERT_OF(sail_int, mach_int)(&zgsz3313, zinteger_sizze);
    lbits zgsz3314;
    CREATE(lbits)(&zgsz3314);
    zeros(&zgsz3314, zgsz3313);
    zgaz311 = CONVERT_OF(fbits, lbits)(zgsz3314, true);
    KILL(lbits)(&zgsz3314);
    KILL(sail_int)(&zgsz3313);
  }
  zNum((*(&zcbz39)), zgaz311);

end_function_31: ;
  goto end_function_257;
end_block_exception_32: ;
  goto end_function_257;
end_function_257: ;
}

uint64_t zMEMri(uint64_t);

uint64_t zMEMri(uint64_t zaddr)
{
  __label__ end_function_34, end_block_exception_35;

  uint64_t zcbz310;
  {
    sail_int zgsz3315;
    CREATE(sail_int)(&zgsz3315);
    CONVERT_OF(sail_int, mach_int)(&zgsz3315, INT64_C(32));
    lbits zgsz3316;
    CREATE(lbits)(&zgsz3316);
    zeros(&zgsz3316, zgsz3315);
    zcbz310 = CONVERT_OF(fbits, lbits)(zgsz3316, true);
    KILL(lbits)(&zgsz3316);
    KILL(sail_int)(&zgsz3315);
  }
end_function_34: ;
  return zcbz310;
end_block_exception_35: ;

  return UINT64_C(0xdeadc0de);
}



struct zCapability zdefault_capability;
static void create_letbind_3(void) {


  struct zCapability zgsz314;
  uint64_t zgaz312;
  {
    sail_int zgsz3319;
    CREATE(sail_int)(&zgsz3319);
    CONVERT_OF(sail_int, mach_int)(&zgsz3319, INT64_C(64));
    lbits zgsz3320;
    CREATE(lbits)(&zgsz3320);
    zeros(&zgsz3320, zgsz3319);
    zgaz312 = CONVERT_OF(fbits, lbits)(zgsz3320, true);
    KILL(lbits)(&zgsz3320);
    KILL(sail_int)(&zgsz3319);
  }
  uint64_t zgaz313;
  {
    sail_int zgsz3317;
    CREATE(sail_int)(&zgsz3317);
    CONVERT_OF(sail_int, mach_int)(&zgsz3317, INT64_C(64));
    lbits zgsz3318;
    CREATE(lbits)(&zgsz3318);
    zeros(&zgsz3318, zgsz3317);
    zgaz313 = CONVERT_OF(fbits, lbits)(zgsz3318, true);
    KILL(lbits)(&zgsz3318);
    KILL(sail_int)(&zgsz3317);
  }
  struct zCapability zgsz313;
  zgsz313.zcap_begin = zgaz312;
  zgsz313.zcap_cursor = zgaz313;
  zgsz313.zcap_end = UINT64_C(0xFFFFFFFFFFFFFFFF);
  zgsz313.zcap_permission = zRW;
  zgsz314 = zgsz313;



  zdefault_capability = zgsz314;

let_end_36: ;
}
static void kill_letbind_3(void) {
}

bool zis_sub_perm(enum zPermission, enum zPermission);

bool zis_sub_perm(enum zPermission zp, enum zPermission zpz7)
{
  __label__ case_39, case_40, case_44, case_49, finish_match_38, end_function_53, end_block_exception_54;

  bool zcbz311;
  /* Case with num_cases: 4 */
  bool zgsz315;
  {
    if ((zO != zp)) goto case_39;
    zgsz315 = true;
    goto finish_match_38;
  }
case_39: ;
  {
    __label__ case_42, case_43, finish_match_41;

    if ((zE != zp)) goto case_40;
    /* Case with num_cases: 2 */
    bool zgsz317;
    {
      if ((zO != zpz7)) goto case_42;
      zgsz317 = false;
      goto finish_match_41;
    }
  case_42: ;
    {
      zgsz317 = true;
      goto finish_match_41;
    }
  case_43: ;
    sail_match_failure("is_sub_perm");
  finish_match_41: ;
    zgsz315 = zgsz317;
    goto finish_match_38;
  }
case_40: ;
  {
    __label__ case_46, case_47, case_48, finish_match_45;

    if ((zR != zp)) goto case_44;
    /* Case with num_cases: 3 */
    bool zgsz321;
    {
      if ((zO != zpz7)) goto case_46;
      zgsz321 = false;
      goto finish_match_45;
    }
  case_46: ;
    {
      if ((zE != zpz7)) goto case_47;
      zgsz321 = false;
      goto finish_match_45;
    }
  case_47: ;
    {
      zgsz321 = true;
      goto finish_match_45;
    }
  case_48: ;
    sail_match_failure("is_sub_perm");
  finish_match_45: ;
    zgsz315 = zgsz321;
    goto finish_match_38;
  }
case_44: ;
  {
    __label__ case_51, case_52, finish_match_50;

    if ((zRW != zp)) goto case_49;
    /* Case with num_cases: 2 */
    bool zgsz326;
    {
      enum zPermission zRWshadowz30;
      zRWshadowz30 = zpz7;
      zgsz326 = true;
      goto finish_match_50;
    }
  case_51: ;
    {
      zgsz326 = false;
      goto finish_match_50;
    }
  case_52: ;
    sail_match_failure("is_sub_perm");
  finish_match_50: ;
    zgsz315 = zgsz326;
    goto finish_match_38;
  }
case_49: ;
  sail_match_failure("is_sub_perm");
finish_match_38: ;
  zcbz311 = zgsz315;

end_function_53: ;
  return zcbz311;
end_block_exception_54: ;

  return false;
}

bool zwriteAllowed(enum zPermission);

bool zwriteAllowed(enum zPermission zp)
{
  __label__ end_function_56, end_block_exception_57;

  bool zcbz312;
  zcbz312 = zis_sub_perm(zRW, zp);
end_function_56: ;
  return zcbz312;
end_block_exception_57: ;

  return false;
}

bool zreadAllowed(enum zPermission);

bool zreadAllowed(enum zPermission zp)
{
  __label__ end_function_59, end_block_exception_60;

  bool zcbz313;
  zcbz313 = zis_sub_perm(zR, zp);
end_function_59: ;
  return zcbz313;
end_block_exception_60: ;

  return false;
}

bool zwithinBounds(struct zCapability);

bool zwithinBounds(struct zCapability zc)
{
  __label__ end_function_62, end_block_exception_63;

  bool zcbz314;
  sail_int zcursor;
  CREATE(sail_int)(&zcursor);
  {
    uint64_t zgaz317;
    zgaz317 = zc.zcap_cursor;
    {
      lbits zgsz3321;
      CREATE(lbits)(&zgsz3321);
      CONVERT_OF(lbits, fbits)(&zgsz3321, zgaz317, UINT64_C(64) , true);
      sail_unsigned(&zcursor, zgsz3321);
      KILL(lbits)(&zgsz3321);
    }
  }
  sail_int zbegin;
  CREATE(sail_int)(&zbegin);
  {
    uint64_t zgaz316;
    zgaz316 = zc.zcap_begin;
    {
      lbits zgsz3322;
      CREATE(lbits)(&zgsz3322);
      CONVERT_OF(lbits, fbits)(&zgsz3322, zgaz316, UINT64_C(64) , true);
      sail_unsigned(&zbegin, zgsz3322);
      KILL(lbits)(&zgsz3322);
    }
  }
  sail_int ztop;
  CREATE(sail_int)(&ztop);
  {
    uint64_t zgaz315;
    zgaz315 = zc.zcap_end;
    {
      lbits zgsz3323;
      CREATE(lbits)(&zgsz3323);
      CONVERT_OF(lbits, fbits)(&zgsz3323, zgaz315, UINT64_C(64) , true);
      sail_unsigned(&ztop, zgsz3323);
      KILL(lbits)(&zgsz3323);
    }
  }
  bool zgaz314;
  zgaz314 = lteq(zbegin, zcursor);
  bool zgsz330;
  if (zgaz314) {  zgsz330 = lteq(zcursor, ztop);  } else {  zgsz330 = false;  }
  zcbz314 = zgsz330;

  KILL(sail_int)(&ztop);
  KILL(sail_int)(&zbegin);
  KILL(sail_int)(&zcursor);
end_function_62: ;
  return zcbz314;
end_block_exception_63: ;

  return false;
}

bool zisWithinRange(uint64_t, uint64_t, uint64_t, uint64_t);

bool zisWithinRange(uint64_t zbz7, uint64_t zez7, uint64_t zb, uint64_t ze)
{
  __label__ end_function_65, end_block_exception_66;

  bool zcbz315;
  bool zgaz322;
  {
    sail_int zgaz318;
    CREATE(sail_int)(&zgaz318);
    {
      lbits zgsz3325;
      CREATE(lbits)(&zgsz3325);
      CONVERT_OF(lbits, fbits)(&zgsz3325, zb, UINT64_C(64) , true);
      sail_unsigned(&zgaz318, zgsz3325);
      KILL(lbits)(&zgsz3325);
    }
    sail_int zgaz319;
    CREATE(sail_int)(&zgaz319);
    {
      lbits zgsz3324;
      CREATE(lbits)(&zgsz3324);
      CONVERT_OF(lbits, fbits)(&zgsz3324, zbz7, UINT64_C(64) , true);
      sail_unsigned(&zgaz319, zgsz3324);
      KILL(lbits)(&zgsz3324);
    }
    zgaz322 = lteq(zgaz318, zgaz319);
    KILL(sail_int)(&zgaz319);
    KILL(sail_int)(&zgaz318);
  }
  bool zgsz331;
  if (zgaz322) {
    sail_int zgaz320;
    CREATE(sail_int)(&zgaz320);
    {
      lbits zgsz3327;
      CREATE(lbits)(&zgsz3327);
      CONVERT_OF(lbits, fbits)(&zgsz3327, zez7, UINT64_C(64) , true);
      sail_unsigned(&zgaz320, zgsz3327);
      KILL(lbits)(&zgsz3327);
    }
    sail_int zgaz321;
    CREATE(sail_int)(&zgaz321);
    {
      lbits zgsz3326;
      CREATE(lbits)(&zgsz3326);
      CONVERT_OF(lbits, fbits)(&zgsz3326, ze, UINT64_C(64) , true);
      sail_unsigned(&zgaz321, zgsz3326);
      KILL(lbits)(&zgsz3326);
    }
    zgsz331 = lteq(zgaz320, zgaz321);
    KILL(sail_int)(&zgaz321);
    KILL(sail_int)(&zgaz320);
  } else {  zgsz331 = false;  }
  zcbz315 = zgsz331;

end_function_65: ;
  return zcbz315;
end_block_exception_66: ;

  return false;
}

void zread_mem(struct zword *rop, struct zCapability);

void zread_mem(struct zword *zcbz316, struct zCapability zc)
{
  __label__ end_function_68, end_block_exception_69, end_function_256;

  bool zp;
  {
    enum zPermission zgaz324;
    zgaz324 = zc.zcap_permission;
    zp = zreadAllowed(zgaz324);
  }
  {
    unit zgsz332;
    zgsz332 = sail_assert(zp, "Err: [read_mem] no read permission");
  }
  bool zq;
  zq = zwithinBounds(zc);
  {
    unit zgsz333;
    zgsz333 = sail_assert(zq, "Err: [read_mem] out of bounds");
  }
  uint64_t zgaz323;
  zgaz323 = zc.zcap_cursor;
  zMEMr((*(&zcbz316)), zgaz323);



end_function_68: ;
  goto end_function_256;
end_block_exception_69: ;
  goto end_function_256;
end_function_256: ;
}

unit zwrite_mem(struct zCapability, struct zword);

unit zwrite_mem(struct zCapability zc, struct zword zw)
{
  __label__ end_function_71, end_block_exception_72;

  unit zcbz317;
  bool zp;
  {
    enum zPermission zgaz327;
    zgaz327 = zc.zcap_permission;
    zp = zwriteAllowed(zgaz327);
  }
  {
    unit zgsz334;
    zgsz334 = sail_assert(zp, "Err: [write_mem] no read permission");
  }
  bool zq;
  zq = zwithinBounds(zc);
  {
    unit zgsz335;
    zgsz335 = sail_assert(zq, "Err: [write_mem] out of bounds");
  }
  bool zgaz326;
  {
    uint64_t zgaz325;
    zgaz325 = zc.zcap_cursor;
    zgaz326 = zMEMw(zgaz325, zw);
  }
  zcbz317 = sail_assert(zgaz326, "./capabilities.sail:58.30-58.31");



end_function_71: ;
  return zcbz317;
end_block_exception_72: ;

  return UNIT;
}

uint64_t zperm_bits_forwards(enum zPermission);

enum zPermission zperm_bits_backwards(uint64_t);

uint64_t zperm_bits_forwards(enum zPermission zargz3)
{
  __label__ case_75, case_76, case_77, case_78, finish_match_74, end_function_79, end_block_exception_80;

  uint64_t zcbz318;
  /* Case with num_cases: 4 */
  uint64_t zgsz336;
  {
    if ((zO != zargz3)) goto case_75;
    zgsz336 = UINT64_C(0b000);
    goto finish_match_74;
  }
case_75: ;
  {
    if ((zR != zargz3)) goto case_76;
    zgsz336 = UINT64_C(0b010);
    goto finish_match_74;
  }
case_76: ;
  {
    if ((zRW != zargz3)) goto case_77;
    zgsz336 = UINT64_C(0b011);
    goto finish_match_74;
  }
case_77: ;
  {
    if ((zE != zargz3)) goto case_78;
    zgsz336 = UINT64_C(0b100);
    goto finish_match_74;
  }
case_78: ;
  sail_match_failure("perm_bits_forwards");
finish_match_74: ;
  zcbz318 = zgsz336;

end_function_79: ;
  return zcbz318;
end_block_exception_80: ;

  return UINT64_C(0xdeadc0de);
}

enum zPermission zperm_bits_backwards(uint64_t zargz3)
{
  __label__ case_83, case_84, case_85, case_86, finish_match_82, end_function_87, end_block_exception_88;

  enum zPermission zcbz319;
  /* Case with num_cases: 4 */
  enum zPermission zgsz341;
  {
    uint64_t zb__0;
    zb__0 = zargz3;
    bool zgsz342;
    {
      lbits zgsz3328;
      CREATE(lbits)(&zgsz3328);
      CONVERT_OF(lbits, fbits)(&zgsz3328, zb__0, UINT64_C(3) , true);
      lbits zgsz3329;
      CREATE(lbits)(&zgsz3329);
      CONVERT_OF(lbits, fbits)(&zgsz3329, UINT64_C(0b000), UINT64_C(3) , true);
      zgsz342 = eq_bits(zgsz3328, zgsz3329);
      KILL(lbits)(&zgsz3329);
      KILL(lbits)(&zgsz3328);
    }
    if (!(zgsz342)) {

      goto case_83;
    }
    zgsz341 = zO;
    goto finish_match_82;
  }
case_83: ;
  {
    uint64_t zb__1;
    zb__1 = zargz3;
    bool zgsz343;
    {
      lbits zgsz3330;
      CREATE(lbits)(&zgsz3330);
      CONVERT_OF(lbits, fbits)(&zgsz3330, zb__1, UINT64_C(3) , true);
      lbits zgsz3331;
      CREATE(lbits)(&zgsz3331);
      CONVERT_OF(lbits, fbits)(&zgsz3331, UINT64_C(0b010), UINT64_C(3) , true);
      zgsz343 = eq_bits(zgsz3330, zgsz3331);
      KILL(lbits)(&zgsz3331);
      KILL(lbits)(&zgsz3330);
    }
    if (!(zgsz343)) {

      goto case_84;
    }
    zgsz341 = zR;
    goto finish_match_82;
  }
case_84: ;
  {
    uint64_t zb__2;
    zb__2 = zargz3;
    bool zgsz344;
    {
      lbits zgsz3332;
      CREATE(lbits)(&zgsz3332);
      CONVERT_OF(lbits, fbits)(&zgsz3332, zb__2, UINT64_C(3) , true);
      lbits zgsz3333;
      CREATE(lbits)(&zgsz3333);
      CONVERT_OF(lbits, fbits)(&zgsz3333, UINT64_C(0b011), UINT64_C(3) , true);
      zgsz344 = eq_bits(zgsz3332, zgsz3333);
      KILL(lbits)(&zgsz3333);
      KILL(lbits)(&zgsz3332);
    }
    if (!(zgsz344)) {

      goto case_85;
    }
    zgsz341 = zRW;
    goto finish_match_82;
  }
case_85: ;
  {
    uint64_t zb__3;
    zb__3 = zargz3;
    bool zgsz345;
    {
      lbits zgsz3334;
      CREATE(lbits)(&zgsz3334);
      CONVERT_OF(lbits, fbits)(&zgsz3334, zb__3, UINT64_C(3) , true);
      lbits zgsz3335;
      CREATE(lbits)(&zgsz3335);
      CONVERT_OF(lbits, fbits)(&zgsz3335, UINT64_C(0b100), UINT64_C(3) , true);
      zgsz345 = eq_bits(zgsz3334, zgsz3335);
      KILL(lbits)(&zgsz3335);
      KILL(lbits)(&zgsz3334);
    }
    if (!(zgsz345)) {

      goto case_86;
    }
    zgsz341 = zE;
    goto finish_match_82;
  }
case_86: ;
  sail_match_failure("perm_bits_backwards");
finish_match_82: ;
  zcbz319 = zgsz341;

end_function_87: ;
  return zcbz319;
end_block_exception_88: ;

  return zE;
}

int64_t zimm_sizze;
static void create_letbind_4(void) {


  int64_t zgsz346;
  zgsz346 = INT64_C(12);
  zimm_sizze = zgsz346;

let_end_89: ;
}
static void kill_letbind_4(void) {
}

int64_t zimm_ext_sizze;
static void create_letbind_5(void) {


  int64_t zgsz347;
  zgsz347 = INT64_C(20);
  zimm_ext_sizze = zgsz347;

let_end_90: ;
}
static void kill_letbind_5(void) {
}

unit zwriteReg(uint64_t, struct zword);

unit zwriteReg(uint64_t zrs, struct zword zw)
{
  __label__ case_93, case_94, case_95, case_96, finish_match_92, end_function_97, end_block_exception_98;

  unit zcbz320;
  /* Case with num_cases: 4 */
  unit zgsz348;
  {
    uint64_t zb__0;
    zb__0 = zrs;
    bool zgsz349;
    {
      lbits zgsz3336;
      CREATE(lbits)(&zgsz3336);
      CONVERT_OF(lbits, fbits)(&zgsz3336, zb__0, UINT64_C(2) , true);
      lbits zgsz3337;
      CREATE(lbits)(&zgsz3337);
      CONVERT_OF(lbits, fbits)(&zgsz3337, UINT64_C(0b00), UINT64_C(2) , true);
      zgsz349 = eq_bits(zgsz3336, zgsz3337);
      KILL(lbits)(&zgsz3337);
      KILL(lbits)(&zgsz3336);
    }
    if (!(zgsz349)) {

      goto case_93;
    }
    COPY(zword)(&zR0, zw);
    zgsz348 = UNIT;
    goto finish_match_92;
  }
case_93: ;
  {
    uint64_t zb__1;
    zb__1 = zrs;
    bool zgsz350;
    {
      lbits zgsz3338;
      CREATE(lbits)(&zgsz3338);
      CONVERT_OF(lbits, fbits)(&zgsz3338, zb__1, UINT64_C(2) , true);
      lbits zgsz3339;
      CREATE(lbits)(&zgsz3339);
      CONVERT_OF(lbits, fbits)(&zgsz3339, UINT64_C(0b01), UINT64_C(2) , true);
      zgsz350 = eq_bits(zgsz3338, zgsz3339);
      KILL(lbits)(&zgsz3339);
      KILL(lbits)(&zgsz3338);
    }
    if (!(zgsz350)) {

      goto case_94;
    }
    COPY(zword)(&zR1, zw);
    zgsz348 = UNIT;
    goto finish_match_92;
  }
case_94: ;
  {
    uint64_t zb__2;
    zb__2 = zrs;
    bool zgsz351;
    {
      lbits zgsz3340;
      CREATE(lbits)(&zgsz3340);
      CONVERT_OF(lbits, fbits)(&zgsz3340, zb__2, UINT64_C(2) , true);
      lbits zgsz3341;
      CREATE(lbits)(&zgsz3341);
      CONVERT_OF(lbits, fbits)(&zgsz3341, UINT64_C(0b10), UINT64_C(2) , true);
      zgsz351 = eq_bits(zgsz3340, zgsz3341);
      KILL(lbits)(&zgsz3341);
      KILL(lbits)(&zgsz3340);
    }
    if (!(zgsz351)) {

      goto case_95;
    }
    COPY(zword)(&zR2, zw);
    zgsz348 = UNIT;
    goto finish_match_92;
  }
case_95: ;
  {
    COPY(zword)(&zR3, zw);
    zgsz348 = UNIT;
    goto finish_match_92;
  }
case_96: ;
  sail_match_failure("writeReg");
finish_match_92: ;
  zcbz320 = zgsz348;

end_function_97: ;
  return zcbz320;
end_block_exception_98: ;

  return UNIT;
}

void zreadReg(struct zword *rop, uint64_t);

void zreadReg(struct zword *zcbz321, uint64_t zrs)
{
  __label__ case_101, case_102, case_103, case_104, finish_match_100, end_function_105, end_block_exception_106, end_function_255;

  /* Case with num_cases: 4 */
  struct zword zgsz353;
  CREATE(zword)(&zgsz353);
  {
    uint64_t zb__0;
    zb__0 = zrs;
    bool zgsz354;
    {
      lbits zgsz3344;
      CREATE(lbits)(&zgsz3344);
      CONVERT_OF(lbits, fbits)(&zgsz3344, zb__0, UINT64_C(2) , true);
      lbits zgsz3345;
      CREATE(lbits)(&zgsz3345);
      CONVERT_OF(lbits, fbits)(&zgsz3345, UINT64_C(0b00), UINT64_C(2) , true);
      zgsz354 = eq_bits(zgsz3344, zgsz3345);
      KILL(lbits)(&zgsz3345);
      KILL(lbits)(&zgsz3344);
    }
    if (!(zgsz354)) {

      goto case_101;
    }
    uint64_t zgaz328;
    {
      sail_int zgsz3342;
      CREATE(sail_int)(&zgsz3342);
      CONVERT_OF(sail_int, mach_int)(&zgsz3342, zinteger_sizze);
      lbits zgsz3343;
      CREATE(lbits)(&zgsz3343);
      zeros(&zgsz3343, zgsz3342);
      zgaz328 = CONVERT_OF(fbits, lbits)(zgsz3343, true);
      KILL(lbits)(&zgsz3343);
      KILL(sail_int)(&zgsz3342);
    }
    zNum(&zgsz353, zgaz328);
    goto finish_match_100;
  }
case_101: ;
  {
    uint64_t zb__1;
    zb__1 = zrs;
    bool zgsz355;
    {
      lbits zgsz3346;
      CREATE(lbits)(&zgsz3346);
      CONVERT_OF(lbits, fbits)(&zgsz3346, zb__1, UINT64_C(2) , true);
      lbits zgsz3347;
      CREATE(lbits)(&zgsz3347);
      CONVERT_OF(lbits, fbits)(&zgsz3347, UINT64_C(0b01), UINT64_C(2) , true);
      zgsz355 = eq_bits(zgsz3346, zgsz3347);
      KILL(lbits)(&zgsz3347);
      KILL(lbits)(&zgsz3346);
    }
    if (!(zgsz355)) {

      goto case_102;
    }
    COPY(zword)(&zgsz353, zR1);
    goto finish_match_100;
  }
case_102: ;
  {
    uint64_t zb__2;
    zb__2 = zrs;
    bool zgsz356;
    {
      lbits zgsz3348;
      CREATE(lbits)(&zgsz3348);
      CONVERT_OF(lbits, fbits)(&zgsz3348, zb__2, UINT64_C(2) , true);
      lbits zgsz3349;
      CREATE(lbits)(&zgsz3349);
      CONVERT_OF(lbits, fbits)(&zgsz3349, UINT64_C(0b10), UINT64_C(2) , true);
      zgsz356 = eq_bits(zgsz3348, zgsz3349);
      KILL(lbits)(&zgsz3349);
      KILL(lbits)(&zgsz3348);
    }
    if (!(zgsz356)) {

      goto case_103;
    }
    COPY(zword)(&zgsz353, zR2);
    goto finish_match_100;
  }
case_103: ;
  {
    COPY(zword)(&zgsz353, zR3);
    goto finish_match_100;
  }
case_104: ;
  sail_match_failure("readReg");
finish_match_100: ;
  COPY(zword)((*(&zcbz321)), zgsz353);
  KILL(zword)(&zgsz353);
end_function_105: ;
  goto end_function_255;
end_block_exception_106: ;
  goto end_function_255;
end_function_255: ;
}

struct zCapability zreadRegCap(uint64_t);

struct zCapability zreadRegCap(uint64_t zcs)
{
  __label__ case_109, case_110, finish_match_108, end_function_111, end_block_exception_112;

  struct zCapability zcbz322;
  struct zword zw;
  CREATE(zword)(&zw);
  zreadReg(&zw, zcs);
  /* Case with num_cases: 2 */
  struct zCapability zgsz358;
  {
    if (zw.kind != Kind_zCap) goto case_109;
    struct zCapability zc;
    zc = zw.zCap;
    zgsz358 = zc;
    goto finish_match_108;
  }
case_109: ;
  {
    struct zexception zgaz329;
    CREATE(zexception)(&zgaz329);
    zExpectedCapabilityRegisterContents(&zgaz329, UNIT);
    COPY(zexception)(current_exception, zgaz329);
    have_exception = true;
    COPY(sail_string)(throw_location, "./instructions.sail:68.9-68.52");
    KILL(zword)(&zw);
    KILL(zexception)(&zgaz329);
    goto end_block_exception_112;
    /* unreachable after throw */
    KILL(zexception)(&zgaz329);
    goto finish_match_108;
  }
case_110: ;
  sail_match_failure("readRegCap");
finish_match_108: ;
  zcbz322 = zgsz358;

  KILL(zword)(&zw);
end_function_111: ;
  return zcbz322;
end_block_exception_112: ;
  struct zCapability zcbz339 = { .zcap_begin = UINT64_C(0xdeadc0de), .zcap_cursor = UINT64_C(0xdeadc0de), .zcap_end = UINT64_C(0xdeadc0de), .zcap_permission = zE };
  return zcbz339;
}

uint64_t zreadRegNum(uint64_t);

uint64_t zreadRegNum(uint64_t zrs)
{
  __label__ case_115, case_116, finish_match_114, end_function_117, end_block_exception_118;

  uint64_t zcbz323;
  struct zword zw;
  CREATE(zword)(&zw);
  zreadReg(&zw, zrs);
  /* Case with num_cases: 2 */
  uint64_t zgsz361;
  {
    if (zw.kind != Kind_zNum) goto case_115;
    uint64_t zi;
    zi = zw.zNum;
    zgsz361 = zi;
    goto finish_match_114;
  }
case_115: ;
  {
    struct zexception zgaz330;
    CREATE(zexception)(&zgaz330);
    zExpectedNumberRegisterContents(&zgaz330, UNIT);
    COPY(zexception)(current_exception, zgaz330);
    have_exception = true;
    COPY(sail_string)(throw_location, "./instructions.sail:77.9-77.48");
    KILL(zword)(&zw);
    KILL(zexception)(&zgaz330);
    goto end_block_exception_118;
    /* unreachable after throw */
    KILL(zexception)(&zgaz330);
    goto finish_match_114;
  }
case_116: ;
  sail_match_failure("readRegNum");
finish_match_114: ;
  zcbz323 = zgsz361;

  KILL(zword)(&zw);
end_function_117: ;
  return zcbz323;
end_block_exception_118: ;

  return UINT64_C(0xdeadc0de);
}

bool zisPerm(enum zPermission, enum zPermission);

bool zisPerm(enum zPermission zp, enum zPermission zpz7)
{
  __label__ case_121, case_125, case_129, case_133, finish_match_120, end_function_137, end_block_exception_138;

  bool zcbz324;
  /* Case with num_cases: 4 */
  bool zgsz364;
  {
    __label__ case_123, case_124, finish_match_122;

    if ((zO != zp)) goto case_121;
    /* Case with num_cases: 2 */
    bool zgsz365;
    {
      enum zPermission zOshadowz31;
      zOshadowz31 = zpz7;
      zgsz365 = true;
      goto finish_match_122;
    }
  case_123: ;
    {
      zgsz365 = false;
      goto finish_match_122;
    }
  case_124: ;
    sail_match_failure("isPerm");
  finish_match_122: ;
    zgsz364 = zgsz365;
    goto finish_match_120;
  }
case_121: ;
  {
    __label__ case_127, case_128, finish_match_126;

    if ((zR != zp)) goto case_125;
    /* Case with num_cases: 2 */
    bool zgsz369;
    {
      enum zPermission zRshadowz32;
      zRshadowz32 = zpz7;
      zgsz369 = true;
      goto finish_match_126;
    }
  case_127: ;
    {
      zgsz369 = false;
      goto finish_match_126;
    }
  case_128: ;
    sail_match_failure("isPerm");
  finish_match_126: ;
    zgsz364 = zgsz369;
    goto finish_match_120;
  }
case_125: ;
  {
    __label__ case_131, case_132, finish_match_130;

    if ((zRW != zp)) goto case_129;
    /* Case with num_cases: 2 */
    bool zgsz373;
    {
      enum zPermission zRWshadowz33;
      zRWshadowz33 = zpz7;
      zgsz373 = true;
      goto finish_match_130;
    }
  case_131: ;
    {
      zgsz373 = false;
      goto finish_match_130;
    }
  case_132: ;
    sail_match_failure("isPerm");
  finish_match_130: ;
    zgsz364 = zgsz373;
    goto finish_match_120;
  }
case_129: ;
  {
    __label__ case_135, case_136, finish_match_134;

    if ((zE != zp)) goto case_133;
    /* Case with num_cases: 2 */
    bool zgsz377;
    {
      enum zPermission zEshadowz34;
      zEshadowz34 = zpz7;
      zgsz377 = true;
      goto finish_match_134;
    }
  case_135: ;
    {
      zgsz377 = false;
      goto finish_match_134;
    }
  case_136: ;
    sail_match_failure("isPerm");
  finish_match_134: ;
    zgsz364 = zgsz377;
    goto finish_match_120;
  }
case_133: ;
  sail_match_failure("isPerm");
finish_match_120: ;
  zcbz324 = zgsz364;

end_function_137: ;
  return zcbz324;
end_block_exception_138: ;

  return false;
}

bool zisNotZero(uint64_t);

bool zisNotZero(uint64_t zi)
{
  __label__ end_function_140, end_block_exception_141;

  bool zcbz325;
  uint64_t zgaz331;
  {
    sail_int zgsz3352;
    CREATE(sail_int)(&zgsz3352);
    CONVERT_OF(sail_int, mach_int)(&zgsz3352, zimm_sizze);
    lbits zgsz3353;
    CREATE(lbits)(&zgsz3353);
    zeros(&zgsz3353, zgsz3352);
    zgaz331 = CONVERT_OF(fbits, lbits)(zgsz3353, true);
    KILL(lbits)(&zgsz3353);
    KILL(sail_int)(&zgsz3352);
  }
  {
    lbits zgsz3350;
    CREATE(lbits)(&zgsz3350);
    CONVERT_OF(lbits, fbits)(&zgsz3350, zi, UINT64_C(12) , true);
    lbits zgsz3351;
    CREATE(lbits)(&zgsz3351);
    CONVERT_OF(lbits, fbits)(&zgsz3351, zgaz331, UINT64_C(12) , true);
    zcbz325 = neq_bits(zgsz3350, zgsz3351);
    KILL(lbits)(&zgsz3351);
    KILL(lbits)(&zgsz3350);
  }

end_function_140: ;
  return zcbz325;
end_block_exception_141: ;

  return false;
}

bool zcanIncrCursor(struct zCapability, uint64_t);

bool zcanIncrCursor(struct zCapability zc, uint64_t zimm)
{
  __label__ end_function_143, end_block_exception_144;

  bool zcbz326;
  bool zgaz335;
  {
    bool zgaz333;
    {
      enum zPermission zgaz332;
      zgaz332 = zc.zcap_permission;
      zgaz333 = zisPerm(zgaz332, zE);
    }
    zgaz335 = not(zgaz333);
  }
  bool zgsz381;
  if (zgaz335) {  zgsz381 = true;  } else {
    bool zgaz334;
    zgaz334 = zisNotZero(zimm);
    zgsz381 = not(zgaz334);
  }
  zcbz326 = zgsz381;

end_function_143: ;
  return zcbz326;
end_block_exception_144: ;

  return false;
}

struct zCapability zupdatePCPerm(struct zCapability);

struct zCapability zupdatePCPerm(struct zCapability zc)
{
  __label__ case_147, case_148, finish_match_146, end_function_149, end_block_exception_150;

  struct zCapability zcbz327;
  enum zPermission zgaz336;
  zgaz336 = zc.zcap_permission;
  /* Case with num_cases: 2 */
  struct zCapability zgsz382;
  {
    if ((zE != zgaz336)) goto case_147;
    struct zCapability zgsz383;
    zgsz383 = zc;
    zgsz383.zcap_permission = zR;
    zgsz382 = zgsz383;
    goto finish_match_146;
  }
case_147: ;
  {
    zgsz382 = zc;
    goto finish_match_146;
  }
case_148: ;
  sail_match_failure("updatePCPerm");
finish_match_146: ;
  zcbz327 = zgsz382;


end_function_149: ;
  return zcbz327;
end_block_exception_150: ;
  struct zCapability zcbz340 = { .zcap_begin = UINT64_C(0xdeadc0de), .zcap_cursor = UINT64_C(0xdeadc0de), .zcap_end = UINT64_C(0xdeadc0de), .zcap_permission = zE };
  return zcbz340;
}























bool zexecute(struct zast);
























bool zexecute(struct zast zmergez3var)
{
  __label__ case_153, case_154, case_155, case_156, case_157, case_158, case_162, case_163, case_164, case_165, case_166, case_167, case_168, case_172, case_173, case_174, case_175, case_176, case_177, case_178, case_179, case_180, case_184, case_188, case_189, finish_match_152, end_function_190, end_block_exception_191;

  bool zcbz328;
  /* Case with num_cases: 25 */
  bool zgsz386;
  {
    if (zmergez3var.kind != Kind_zJALR_CAP) goto case_153;
    uint64_t zcd;
    zcd = zmergez3var.zJALR_CAP.ztup0;
    uint64_t zcs;
    zcs = zmergez3var.zJALR_CAP.ztup1;
    struct zast zgaz339;
    CREATE(zast)(&zgaz339);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz338;
      {
        uint64_t zgaz337;
        {
          sail_int zgsz3354;
          CREATE(sail_int)(&zgsz3354);
          CONVERT_OF(sail_int, mach_int)(&zgsz3354, zimm_sizze);
          lbits zgsz3355;
          CREATE(lbits)(&zgsz3355);
          zeros(&zgsz3355, zgsz3354);
          zgaz337 = CONVERT_OF(fbits, lbits)(zgsz3355, true);
          KILL(lbits)(&zgsz3355);
          KILL(sail_int)(&zgsz3354);
        }
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz387;
        zgsz387.ztup0 = zcd;
        zgsz387.ztup1 = zcs;
        zgsz387.ztup2 = zgaz337;
        zgaz338 = zgsz387;
      }
      zCJALR(&zgaz339, zgaz338);
    }
    {
      zgsz386 = zexecute(zgaz339);
      if (have_exception) {



        KILL(zast)(&zgaz339);
        goto end_block_exception_191;
      }
    }
    KILL(zast)(&zgaz339);
    goto finish_match_152;
  }
case_153: ;
  {
    if (zmergez3var.kind != Kind_zCJALR) goto case_154;
    uint64_t zuz30;
    zuz30 = zmergez3var.zCJALR.ztup0;
    uint64_t zuz31;
    zuz31 = zmergez3var.zCJALR.ztup1;
    uint64_t zimm;
    zimm = zmergez3var.zCJALR.ztup2;
    struct zCapability znpc;
    znpc = znextPC(UNIT);
    {
      struct zword zgaz340;
      CREATE(zword)(&zgaz340);
      zCap(&zgaz340, znpc);
      unit zgsz389;
      zgsz389 = zwriteReg(zuz30, zgaz340);
      KILL(zword)(&zgaz340);
    }
    struct zCapability zc;
    {
      zc = zreadRegCap(zuz31);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    bool zgaz342;
    {
      bool zgaz341;
      zgaz341 = zcanIncrCursor(zc, zimm);
      zgaz342 = not(zgaz341);
    }
    if (zgaz342) {
      struct zexception zgaz343;
      CREATE(zexception)(&zgaz343);
      zCapabilityCursorCannotBeModified(&zgaz343, UNIT);
      COPY(zexception)(current_exception, zgaz343);
      have_exception = true;
      COPY(sail_string)(throw_location, "./instructions.sail:124.7-124.48");
      KILL(zexception)(&zgaz343);
      goto end_block_exception_191;
      /* unreachable after throw */
      KILL(zexception)(&zgaz343);
    } else {
      uint64_t zimmshadowz35;
      {
        lbits zgsz3359;
        CREATE(lbits)(&zgsz3359);
        CONVERT_OF(lbits, fbits)(&zgsz3359, zimm, UINT64_C(12) , true);
        sail_int zgsz3360;
        CREATE(sail_int)(&zgsz3360);
        CONVERT_OF(sail_int, mach_int)(&zgsz3360, zinteger_sizze);
        lbits zgsz3361;
        CREATE(lbits)(&zgsz3361);
        zero_extend(&zgsz3361, zgsz3359, zgsz3360);
        zimmshadowz35 = CONVERT_OF(fbits, lbits)(zgsz3361, true);
        KILL(lbits)(&zgsz3361);
        KILL(sail_int)(&zgsz3360);
        KILL(lbits)(&zgsz3359);
      }
      struct zCapability zcz7;
      {
        struct zCapability zgaz346;
        {
          uint64_t zgaz345;
          {
            uint64_t zgaz344;
            zgaz344 = zc.zcap_cursor;
            {
              lbits zgsz3356;
              CREATE(lbits)(&zgsz3356);
              CONVERT_OF(lbits, fbits)(&zgsz3356, zgaz344, UINT64_C(64) , true);
              lbits zgsz3357;
              CREATE(lbits)(&zgsz3357);
              CONVERT_OF(lbits, fbits)(&zgsz3357, zimmshadowz35, UINT64_C(64) , true);
              lbits zgsz3358;
              CREATE(lbits)(&zgsz3358);
              add_bits(&zgsz3358, zgsz3356, zgsz3357);
              zgaz345 = CONVERT_OF(fbits, lbits)(zgsz3358, true);
              KILL(lbits)(&zgsz3358);
              KILL(lbits)(&zgsz3357);
              KILL(lbits)(&zgsz3356);
            }
          }
          struct zCapability zgsz391;
          zgsz391 = zc;
          zgsz391.zcap_cursor = zgaz345;
          zgaz346 = zgsz391;
        }
        zcz7 = zupdatePCPerm(zgaz346);
      }
      {
        zPC = zcz7;
        unit zgsz392;
        zgsz392 = UNIT;
      }
      zgsz386 = true;
    }
    goto finish_match_152;
  }
case_154: ;
  {
    if (zmergez3var.kind != Kind_zCJAL) goto case_155;
    uint64_t zuz32;
    zuz32 = zmergez3var.zCJAL.ztup0;
    uint64_t zuz33;
    zuz33 = zmergez3var.zCJAL.ztup1;
    struct zCapability zuz34;
    zuz34 = znextPC(UNIT);
    {
      struct zword zgaz347;
      CREATE(zword)(&zgaz347);
      zCap(&zgaz347, zuz34);
      unit zgsz394;
      zgsz394 = zwriteReg(zuz32, zgaz347);
      KILL(zword)(&zgaz347);
    }
    {
      uint64_t zgaz349;
      {
        uint64_t zgaz348;
        {
          lbits zgsz3365;
          CREATE(lbits)(&zgsz3365);
          CONVERT_OF(lbits, fbits)(&zgsz3365, zuz33, UINT64_C(20) , true);
          sail_int zgsz3366;
          CREATE(sail_int)(&zgsz3366);
          CONVERT_OF(sail_int, mach_int)(&zgsz3366, zinteger_sizze);
          lbits zgsz3367;
          CREATE(lbits)(&zgsz3367);
          zero_extend(&zgsz3367, zgsz3365, zgsz3366);
          zgaz348 = CONVERT_OF(fbits, lbits)(zgsz3367, true);
          KILL(lbits)(&zgsz3367);
          KILL(sail_int)(&zgsz3366);
          KILL(lbits)(&zgsz3365);
        }
        {
          lbits zgsz3362;
          CREATE(lbits)(&zgsz3362);
          CONVERT_OF(lbits, fbits)(&zgsz3362, zgaz348, UINT64_C(64) , true);
          sail_int zgsz3363;
          CREATE(sail_int)(&zgsz3363);
          CONVERT_OF(sail_int, mach_int)(&zgsz3363, INT64_C(1));
          lbits zgsz3364;
          CREATE(lbits)(&zgsz3364);
          shiftl(&zgsz3364, zgsz3362, zgsz3363);
          zgaz349 = CONVERT_OF(fbits, lbits)(zgsz3364, true);
          KILL(lbits)(&zgsz3364);
          KILL(sail_int)(&zgsz3363);
          KILL(lbits)(&zgsz3362);
        }
      }
      unit zgsz393;
      zgsz393 = zaddPC(zgaz349);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_155: ;
  {
    if (zmergez3var.kind != Kind_zBNE) goto case_156;
    uint64_t zrs1;
    zrs1 = zmergez3var.zBNE.ztup0;
    uint64_t zrs2;
    zrs2 = zmergez3var.zBNE.ztup1;
    uint64_t zuz35;
    zuz35 = zmergez3var.zBNE.ztup2;
    uint64_t za;
    {
      za = zreadRegNum(zrs1);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zb;
    {
      zb = zreadRegNum(zrs2);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    {
      bool zgaz350;
      {
        lbits zgsz3371;
        CREATE(lbits)(&zgsz3371);
        CONVERT_OF(lbits, fbits)(&zgsz3371, za, UINT64_C(64) , true);
        lbits zgsz3372;
        CREATE(lbits)(&zgsz3372);
        CONVERT_OF(lbits, fbits)(&zgsz3372, zb, UINT64_C(64) , true);
        zgaz350 = eq_bits(zgsz3371, zgsz3372);
        KILL(lbits)(&zgsz3372);
        KILL(lbits)(&zgsz3371);
      }
      unit zgsz396;
      if (zgaz350) {  zgsz396 = zupdatePC(UNIT);  } else {
        uint64_t zgaz351;
        {
          lbits zgsz3368;
          CREATE(lbits)(&zgsz3368);
          CONVERT_OF(lbits, fbits)(&zgsz3368, zuz35, UINT64_C(12) , true);
          sail_int zgsz3369;
          CREATE(sail_int)(&zgsz3369);
          CONVERT_OF(sail_int, mach_int)(&zgsz3369, zinteger_sizze);
          lbits zgsz3370;
          CREATE(lbits)(&zgsz3370);
          zero_extend(&zgsz3370, zgsz3368, zgsz3369);
          zgaz351 = CONVERT_OF(fbits, lbits)(zgsz3370, true);
          KILL(lbits)(&zgsz3370);
          KILL(sail_int)(&zgsz3369);
          KILL(lbits)(&zgsz3368);
        }
        zgsz396 = zaddPC(zgaz351);
      }
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_156: ;
  {
    if (zmergez3var.kind != Kind_zCMOVE) goto case_157;
    uint64_t zuz36;
    zuz36 = zmergez3var.zCMOVE.ztup0;
    uint64_t zuz37;
    zuz37 = zmergez3var.zCMOVE.ztup1;
    {
      struct zword zgaz352;
      CREATE(zword)(&zgaz352);
      zreadReg(&zgaz352, zuz37);
      unit zgsz399;
      zgsz399 = zwriteReg(zuz36, zgaz352);
      KILL(zword)(&zgaz352);
    }
    {
      unit zgsz398;
      zgsz398 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_157: ;
  {
    if (zmergez3var.kind != Kind_zCGETTAG) goto case_158;
    uint64_t zrd;
    zrd = zmergez3var.zCGETTAG.ztup0;
    uint64_t zuz38;
    zuz38 = zmergez3var.zCGETTAG.ztup1;
    struct zword zw;
    CREATE(zword)(&zw);
    zreadReg(&zw, zuz38);
    {
      __label__ case_160, case_161, finish_match_159;

      /* Case with num_cases: 2 */
      unit zgsz3101;
      {
        if (zw.kind != Kind_zNum) goto case_160;
        struct zword zgaz354;
        CREATE(zword)(&zgaz354);
        {
          uint64_t zgaz353;
          {
            lbits zgsz3373;
            CREATE(lbits)(&zgsz3373);
            CONVERT_OF(lbits, fbits)(&zgsz3373, UINT64_C(0b0), UINT64_C(1) , true);
            sail_int zgsz3374;
            CREATE(sail_int)(&zgsz3374);
            CONVERT_OF(sail_int, mach_int)(&zgsz3374, zinteger_sizze);
            lbits zgsz3375;
            CREATE(lbits)(&zgsz3375);
            zero_extend(&zgsz3375, zgsz3373, zgsz3374);
            zgaz353 = CONVERT_OF(fbits, lbits)(zgsz3375, true);
            KILL(lbits)(&zgsz3375);
            KILL(sail_int)(&zgsz3374);
            KILL(lbits)(&zgsz3373);
          }
          zNum(&zgaz354, zgaz353);
        }
        zgsz3101 = zwriteReg(zrd, zgaz354);
        KILL(zword)(&zgaz354);
        goto finish_match_159;
      }
    case_160: ;
      {
        if (zw.kind != Kind_zCap) goto case_161;
        struct zword zgaz356;
        CREATE(zword)(&zgaz356);
        {
          uint64_t zgaz355;
          {
            lbits zgsz3376;
            CREATE(lbits)(&zgsz3376);
            CONVERT_OF(lbits, fbits)(&zgsz3376, UINT64_C(0b1), UINT64_C(1) , true);
            sail_int zgsz3377;
            CREATE(sail_int)(&zgsz3377);
            CONVERT_OF(sail_int, mach_int)(&zgsz3377, zinteger_sizze);
            lbits zgsz3378;
            CREATE(lbits)(&zgsz3378);
            zero_extend(&zgsz3378, zgsz3376, zgsz3377);
            zgaz355 = CONVERT_OF(fbits, lbits)(zgsz3378, true);
            KILL(lbits)(&zgsz3378);
            KILL(sail_int)(&zgsz3377);
            KILL(lbits)(&zgsz3376);
          }
          zNum(&zgaz356, zgaz355);
        }
        zgsz3101 = zwriteReg(zrd, zgaz356);
        KILL(zword)(&zgaz356);
        goto finish_match_159;
      }
    case_161: ;
      sail_match_failure("execute");
    finish_match_159: ;
      unit zgsz3105;
      zgsz3105 = zgsz3101;
    }
    {
      unit zgsz3104;
      zgsz3104 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(zword)(&zw);
    goto finish_match_152;
  }
case_158: ;
  {
    if (zmergez3var.kind != Kind_zCGETPERM) goto case_162;
    uint64_t zuz39;
    zuz39 = zmergez3var.zCGETPERM.ztup0;
    uint64_t zuz310;
    zuz310 = zmergez3var.zCGETPERM.ztup1;
    struct zCapability zuz311;
    {
      zuz311 = zreadRegCap(zuz310);
      if (have_exception) {




        goto end_block_exception_191;
      }
    }
    uint64_t zi;
    {
      uint64_t zgaz359;
      {
        enum zPermission zgaz358;
        zgaz358 = zuz311.zcap_permission;
        zgaz359 = zperm_bits_forwards(zgaz358);
      }
      {
        lbits zgsz3379;
        CREATE(lbits)(&zgsz3379);
        CONVERT_OF(lbits, fbits)(&zgsz3379, zgaz359, UINT64_C(3) , true);
        sail_int zgsz3380;
        CREATE(sail_int)(&zgsz3380);
        CONVERT_OF(sail_int, mach_int)(&zgsz3380, zinteger_sizze);
        lbits zgsz3381;
        CREATE(lbits)(&zgsz3381);
        zero_extend(&zgsz3381, zgsz3379, zgsz3380);
        zi = CONVERT_OF(fbits, lbits)(zgsz3381, true);
        KILL(lbits)(&zgsz3381);
        KILL(sail_int)(&zgsz3380);
        KILL(lbits)(&zgsz3379);
      }
    }
    {
      struct zword zgaz357;
      CREATE(zword)(&zgaz357);
      zNum(&zgaz357, zi);
      unit zgsz3108;
      zgsz3108 = zwriteReg(zuz39, zgaz357);
      KILL(zword)(&zgaz357);
    }
    {
      unit zgsz3107;
      zgsz3107 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_162: ;
  {
    if (zmergez3var.kind != Kind_zCGETBASE) goto case_163;
    uint64_t zuz312;
    zuz312 = zmergez3var.zCGETBASE.ztup0;
    uint64_t zuz313;
    zuz313 = zmergez3var.zCGETBASE.ztup1;
    struct zCapability zuz314;
    {
      zuz314 = zreadRegCap(zuz313);
      if (have_exception) {




        goto end_block_exception_191;
      }
    }
    {
      struct zword zgaz361;
      CREATE(zword)(&zgaz361);
      {
        uint64_t zgaz360;
        zgaz360 = zuz314.zcap_begin;
        zNum(&zgaz361, zgaz360);
      }
      unit zgsz3111;
      zgsz3111 = zwriteReg(zuz312, zgaz361);
      KILL(zword)(&zgaz361);
    }
    {
      unit zgsz3110;
      zgsz3110 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_163: ;
  {
    if (zmergez3var.kind != Kind_zCGETLEN) goto case_164;
    uint64_t zuz315;
    zuz315 = zmergez3var.zCGETLEN.ztup0;
    uint64_t zuz316;
    zuz316 = zmergez3var.zCGETLEN.ztup1;
    struct zCapability zuz317;
    {
      zuz317 = zreadRegCap(zuz316);
      if (have_exception) {




        goto end_block_exception_191;
      }
    }
    uint64_t zres;
    {
      uint64_t zgaz363;
      zgaz363 = zuz317.zcap_end;
      uint64_t zgaz364;
      zgaz364 = zuz317.zcap_begin;
      {
        lbits zgsz3382;
        CREATE(lbits)(&zgsz3382);
        CONVERT_OF(lbits, fbits)(&zgsz3382, zgaz363, UINT64_C(64) , true);
        lbits zgsz3383;
        CREATE(lbits)(&zgsz3383);
        CONVERT_OF(lbits, fbits)(&zgsz3383, zgaz364, UINT64_C(64) , true);
        lbits zgsz3384;
        CREATE(lbits)(&zgsz3384);
        sub_bits(&zgsz3384, zgsz3382, zgsz3383);
        zres = CONVERT_OF(fbits, lbits)(zgsz3384, true);
        KILL(lbits)(&zgsz3384);
        KILL(lbits)(&zgsz3383);
        KILL(lbits)(&zgsz3382);
      }
    }
    {
      struct zword zgaz362;
      CREATE(zword)(&zgaz362);
      zNum(&zgaz362, zres);
      unit zgsz3114;
      zgsz3114 = zwriteReg(zuz315, zgaz362);
      KILL(zword)(&zgaz362);
    }
    {
      unit zgsz3113;
      zgsz3113 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_164: ;
  {
    if (zmergez3var.kind != Kind_zCGETADDR) goto case_165;
    uint64_t zuz318;
    zuz318 = zmergez3var.zCGETADDR.ztup0;
    uint64_t zuz319;
    zuz319 = zmergez3var.zCGETADDR.ztup1;
    struct zCapability zuz320;
    {
      zuz320 = zreadRegCap(zuz319);
      if (have_exception) {




        goto end_block_exception_191;
      }
    }
    {
      struct zword zgaz366;
      CREATE(zword)(&zgaz366);
      {
        uint64_t zgaz365;
        zgaz365 = zuz320.zcap_cursor;
        zNum(&zgaz366, zgaz365);
      }
      unit zgsz3117;
      zgsz3117 = zwriteReg(zuz318, zgaz366);
      KILL(zword)(&zgaz366);
    }
    {
      unit zgsz3116;
      zgsz3116 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_165: ;
  {
    if (zmergez3var.kind != Kind_zSD) goto case_166;
    uint64_t zuz321;
    zuz321 = zmergez3var.zSD.ztup0;
    uint64_t zuz322;
    zuz322 = zmergez3var.zSD.ztup1;
    uint64_t zuz323;
    zuz323 = zmergez3var.zSD.ztup2;
    struct zCapability zbase_cap;
    {
      zbase_cap = zreadRegCap(zuz321);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    struct zCapability zuz324;
    {
      uint64_t zgaz371;
      {
        uint64_t zgaz369;
        zgaz369 = zbase_cap.zcap_cursor;
        uint64_t zgaz370;
        {
          lbits zgsz3388;
          CREATE(lbits)(&zgsz3388);
          CONVERT_OF(lbits, fbits)(&zgsz3388, zuz323, UINT64_C(12) , true);
          sail_int zgsz3389;
          CREATE(sail_int)(&zgsz3389);
          CONVERT_OF(sail_int, mach_int)(&zgsz3389, zinteger_sizze);
          lbits zgsz3390;
          CREATE(lbits)(&zgsz3390);
          sign_extend(&zgsz3390, zgsz3388, zgsz3389);
          zgaz370 = CONVERT_OF(fbits, lbits)(zgsz3390, true);
          KILL(lbits)(&zgsz3390);
          KILL(sail_int)(&zgsz3389);
          KILL(lbits)(&zgsz3388);
        }
        {
          lbits zgsz3385;
          CREATE(lbits)(&zgsz3385);
          CONVERT_OF(lbits, fbits)(&zgsz3385, zgaz369, UINT64_C(64) , true);
          lbits zgsz3386;
          CREATE(lbits)(&zgsz3386);
          CONVERT_OF(lbits, fbits)(&zgsz3386, zgaz370, UINT64_C(64) , true);
          lbits zgsz3387;
          CREATE(lbits)(&zgsz3387);
          add_bits(&zgsz3387, zgsz3385, zgsz3386);
          zgaz371 = CONVERT_OF(fbits, lbits)(zgsz3387, true);
          KILL(lbits)(&zgsz3387);
          KILL(lbits)(&zgsz3386);
          KILL(lbits)(&zgsz3385);
        }
      }
      struct zCapability zgsz3119;
      zgsz3119 = zbase_cap;
      zgsz3119.zcap_cursor = zgaz371;
      zuz324 = zgsz3119;
    }
    {
      bool zgaz368;
      {
        enum zPermission zgaz367;
        zgaz367 = zuz324.zcap_permission;
        zgaz368 = zwriteAllowed(zgaz367);
      }
      unit zgsz3120;
      zgsz3120 = sail_assert(zgaz368, "Err: [store] no read permission");
    }
    struct zword zuz325;
    CREATE(zword)(&zuz325);
    zreadReg(&zuz325, zuz322);
    {
      unit zgsz3122;
      zgsz3122 = zwrite_mem(zuz324, zuz325);
    }
    {
      unit zgsz3121;
      zgsz3121 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(zword)(&zuz325);
    goto finish_match_152;
  }
case_166: ;
  {
    if (zmergez3var.kind != Kind_zLD) goto case_167;
    uint64_t zuz326;
    zuz326 = zmergez3var.zLD.ztup0;
    uint64_t zuz327;
    zuz327 = zmergez3var.zLD.ztup1;
    uint64_t zuz328;
    zuz328 = zmergez3var.zLD.ztup2;
    struct zCapability zuz329;
    {
      zuz329 = zreadRegCap(zuz327);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    struct zCapability zuz330;
    {
      uint64_t zgaz376;
      {
        uint64_t zgaz374;
        zgaz374 = zuz329.zcap_cursor;
        uint64_t zgaz375;
        {
          lbits zgsz3394;
          CREATE(lbits)(&zgsz3394);
          CONVERT_OF(lbits, fbits)(&zgsz3394, zuz328, UINT64_C(12) , true);
          sail_int zgsz3395;
          CREATE(sail_int)(&zgsz3395);
          CONVERT_OF(sail_int, mach_int)(&zgsz3395, zinteger_sizze);
          lbits zgsz3396;
          CREATE(lbits)(&zgsz3396);
          sign_extend(&zgsz3396, zgsz3394, zgsz3395);
          zgaz375 = CONVERT_OF(fbits, lbits)(zgsz3396, true);
          KILL(lbits)(&zgsz3396);
          KILL(sail_int)(&zgsz3395);
          KILL(lbits)(&zgsz3394);
        }
        {
          lbits zgsz3391;
          CREATE(lbits)(&zgsz3391);
          CONVERT_OF(lbits, fbits)(&zgsz3391, zgaz374, UINT64_C(64) , true);
          lbits zgsz3392;
          CREATE(lbits)(&zgsz3392);
          CONVERT_OF(lbits, fbits)(&zgsz3392, zgaz375, UINT64_C(64) , true);
          lbits zgsz3393;
          CREATE(lbits)(&zgsz3393);
          add_bits(&zgsz3393, zgsz3391, zgsz3392);
          zgaz376 = CONVERT_OF(fbits, lbits)(zgsz3393, true);
          KILL(lbits)(&zgsz3393);
          KILL(lbits)(&zgsz3392);
          KILL(lbits)(&zgsz3391);
        }
      }
      struct zCapability zgsz3124;
      zgsz3124 = zuz329;
      zgsz3124.zcap_cursor = zgaz376;
      zuz330 = zgsz3124;
    }
    {
      bool zgaz373;
      {
        enum zPermission zgaz372;
        zgaz372 = zuz330.zcap_permission;
        zgaz373 = zreadAllowed(zgaz372);
      }
      unit zgsz3125;
      zgsz3125 = sail_assert(zgaz373, "Err: [load] no read permission");
    }
    struct zword zn;
    CREATE(zword)(&zn);
    zread_mem(&zn, zuz330);
    {
      unit zgsz3127;
      zgsz3127 = zwriteReg(zuz326, zn);
    }
    {
      unit zgsz3126;
      zgsz3126 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(zword)(&zn);
    goto finish_match_152;
  }
case_167: ;
  {
    __label__ case_170, case_171, finish_match_169;

    if (zmergez3var.kind != Kind_zCINCOFFSET) goto case_168;
    uint64_t zuz331;
    zuz331 = zmergez3var.zCINCOFFSET.ztup0;
    uint64_t zuz332;
    zuz332 = zmergez3var.zCINCOFFSET.ztup1;
    uint64_t zrs;
    zrs = zmergez3var.zCINCOFFSET.ztup2;
    struct zCapability zuz333;
    {
      zuz333 = zreadRegCap(zuz332);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zoffset;
    {
      zoffset = zreadRegNum(zrs);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    enum zPermission zgaz377;
    zgaz377 = zuz333.zcap_permission;
    /* Case with num_cases: 2 */
    bool zgsz3129;
    {
      if ((zE != zgaz377)) goto case_170;
      struct zexception zgaz378;
      CREATE(zexception)(&zgaz378);
      zCINCOFFSETOnEnterCapability(&zgaz378, UNIT);
      COPY(zexception)(current_exception, zgaz378);
      have_exception = true;
      COPY(sail_string)(throw_location, "./instructions.sail:219.9-219.45");
      KILL(zexception)(&zgaz378);
      goto end_block_exception_191;
      /* unreachable after throw */
      KILL(zexception)(&zgaz378);
      goto finish_match_169;
    }
  case_170: ;
    {
      struct zCapability zuz334;
      {
        uint64_t zgaz381;
        {
          uint64_t zgaz380;
          zgaz380 = zuz333.zcap_cursor;
          {
            lbits zgsz3397;
            CREATE(lbits)(&zgsz3397);
            CONVERT_OF(lbits, fbits)(&zgsz3397, zgaz380, UINT64_C(64) , true);
            lbits zgsz3398;
            CREATE(lbits)(&zgsz3398);
            CONVERT_OF(lbits, fbits)(&zgsz3398, zoffset, UINT64_C(64) , true);
            lbits zgsz3399;
            CREATE(lbits)(&zgsz3399);
            add_bits(&zgsz3399, zgsz3397, zgsz3398);
            zgaz381 = CONVERT_OF(fbits, lbits)(zgsz3399, true);
            KILL(lbits)(&zgsz3399);
            KILL(lbits)(&zgsz3398);
            KILL(lbits)(&zgsz3397);
          }
        }
        struct zCapability zgsz3131;
        zgsz3131 = zuz333;
        zgsz3131.zcap_cursor = zgaz381;
        zuz334 = zgsz3131;
      }
      {
        struct zword zgaz379;
        CREATE(zword)(&zgaz379);
        zCap(&zgaz379, zuz334);
        unit zgsz3133;
        zgsz3133 = zwriteReg(zuz331, zgaz379);
        KILL(zword)(&zgaz379);
      }
      {
        unit zgsz3132;
        zgsz3132 = zupdatePC(UNIT);
      }
      zgsz3129 = true;
      goto finish_match_169;
    }
  case_171: ;
    sail_match_failure("execute");
  finish_match_169: ;
    zgsz386 = zgsz3129;
    goto finish_match_152;
  }
case_168: ;
  {
    if (zmergez3var.kind != Kind_zCANDPERM) goto case_172;
    uint64_t zuz335;
    zuz335 = zmergez3var.zCANDPERM.ztup0;
    uint64_t zuz336;
    zuz336 = zmergez3var.zCANDPERM.ztup1;
    uint64_t zuz337;
    zuz337 = zmergez3var.zCANDPERM.ztup2;
    struct zCapability zcs_val;
    {
      zcs_val = zreadRegCap(zuz336);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zrs_val;
    {
      zrs_val = zreadRegNum(zuz337);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    uint64_t zp;
    {
      enum zPermission zgaz384;
      zgaz384 = zcs_val.zcap_permission;
      zp = zperm_bits_forwards(zgaz384);
    }
    uint64_t zpz7;
    {
      lbits zgsz3403;
      CREATE(lbits)(&zgsz3403);
      CONVERT_OF(lbits, fbits)(&zgsz3403, zrs_val, UINT64_C(64) , true);
      sail_int zgsz3404;
      CREATE(sail_int)(&zgsz3404);
      CONVERT_OF(sail_int, mach_int)(&zgsz3404, INT64_C(2));
      sail_int zgsz3405;
      CREATE(sail_int)(&zgsz3405);
      CONVERT_OF(sail_int, mach_int)(&zgsz3405, INT64_C(0));
      lbits zgsz3406;
      CREATE(lbits)(&zgsz3406);
      vector_subrange_lbits(&zgsz3406, zgsz3403, zgsz3404, zgsz3405);
      zpz7 = CONVERT_OF(fbits, lbits)(zgsz3406, true);
      KILL(lbits)(&zgsz3406);
      KILL(sail_int)(&zgsz3405);
      KILL(sail_int)(&zgsz3404);
      KILL(lbits)(&zgsz3403);
    }
    enum zPermission znew_p;
    {
      uint64_t zgaz383;
      {
        lbits zgsz3400;
        CREATE(lbits)(&zgsz3400);
        CONVERT_OF(lbits, fbits)(&zgsz3400, zp, UINT64_C(3) , true);
        lbits zgsz3401;
        CREATE(lbits)(&zgsz3401);
        CONVERT_OF(lbits, fbits)(&zgsz3401, zpz7, UINT64_C(3) , true);
        lbits zgsz3402;
        CREATE(lbits)(&zgsz3402);
        and_bits(&zgsz3402, zgsz3400, zgsz3401);
        zgaz383 = CONVERT_OF(fbits, lbits)(zgsz3402, true);
        KILL(lbits)(&zgsz3402);
        KILL(lbits)(&zgsz3401);
        KILL(lbits)(&zgsz3400);
      }
      znew_p = zperm_bits_backwards(zgaz383);
    }
    struct zCapability znew_cap;
    {
      struct zCapability zgsz3136;
      zgsz3136 = zcs_val;
      zgsz3136.zcap_permission = znew_p;
      znew_cap = zgsz3136;
    }
    {
      struct zword zgaz382;
      CREATE(zword)(&zgaz382);
      zCap(&zgaz382, znew_cap);
      unit zgsz3138;
      zgsz3138 = zwriteReg(zuz335, zgaz382);
      KILL(zword)(&zgaz382);
    }
    {
      unit zgsz3137;
      zgsz3137 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_172: ;
  {
    if (zmergez3var.kind != Kind_zADDI) goto case_173;
    uint64_t zuz338;
    zuz338 = zmergez3var.zADDI.ztup0;
    uint64_t zuz339;
    zuz339 = zmergez3var.zADDI.ztup1;
    uint64_t zuz340;
    zuz340 = zmergez3var.zADDI.ztup2;
    uint64_t zv;
    {
      zv = zreadRegNum(zuz339);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zuz341;
    {
      uint64_t zgaz386;
      {
        lbits zgsz3410;
        CREATE(lbits)(&zgsz3410);
        CONVERT_OF(lbits, fbits)(&zgsz3410, zuz340, UINT64_C(12) , true);
        sail_int zgsz3411;
        CREATE(sail_int)(&zgsz3411);
        CONVERT_OF(sail_int, mach_int)(&zgsz3411, zinteger_sizze);
        lbits zgsz3412;
        CREATE(lbits)(&zgsz3412);
        sign_extend(&zgsz3412, zgsz3410, zgsz3411);
        zgaz386 = CONVERT_OF(fbits, lbits)(zgsz3412, true);
        KILL(lbits)(&zgsz3412);
        KILL(sail_int)(&zgsz3411);
        KILL(lbits)(&zgsz3410);
      }
      {
        lbits zgsz3407;
        CREATE(lbits)(&zgsz3407);
        CONVERT_OF(lbits, fbits)(&zgsz3407, zv, UINT64_C(64) , true);
        lbits zgsz3408;
        CREATE(lbits)(&zgsz3408);
        CONVERT_OF(lbits, fbits)(&zgsz3408, zgaz386, UINT64_C(64) , true);
        lbits zgsz3409;
        CREATE(lbits)(&zgsz3409);
        add_bits(&zgsz3409, zgsz3407, zgsz3408);
        zuz341 = CONVERT_OF(fbits, lbits)(zgsz3409, true);
        KILL(lbits)(&zgsz3409);
        KILL(lbits)(&zgsz3408);
        KILL(lbits)(&zgsz3407);
      }
    }
    {
      struct zword zgaz385;
      CREATE(zword)(&zgaz385);
      zNum(&zgaz385, zuz341);
      unit zgsz3141;
      zgsz3141 = zwriteReg(zuz338, zgaz385);
      KILL(zword)(&zgaz385);
    }
    {
      unit zgsz3140;
      zgsz3140 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_173: ;
  {
    if (zmergez3var.kind != Kind_zADD) goto case_174;
    uint64_t zuz342;
    zuz342 = zmergez3var.zADD.ztup0;
    uint64_t zuz343;
    zuz343 = zmergez3var.zADD.ztup1;
    uint64_t zuz344;
    zuz344 = zmergez3var.zADD.ztup2;
    uint64_t zv1;
    {
      zv1 = zreadRegNum(zuz343);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zv2;
    {
      zv2 = zreadRegNum(zuz344);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    uint64_t zuz345;
    {
      lbits zgsz3413;
      CREATE(lbits)(&zgsz3413);
      CONVERT_OF(lbits, fbits)(&zgsz3413, zv1, UINT64_C(64) , true);
      lbits zgsz3414;
      CREATE(lbits)(&zgsz3414);
      CONVERT_OF(lbits, fbits)(&zgsz3414, zv2, UINT64_C(64) , true);
      lbits zgsz3415;
      CREATE(lbits)(&zgsz3415);
      add_bits(&zgsz3415, zgsz3413, zgsz3414);
      zuz345 = CONVERT_OF(fbits, lbits)(zgsz3415, true);
      KILL(lbits)(&zgsz3415);
      KILL(lbits)(&zgsz3414);
      KILL(lbits)(&zgsz3413);
    }
    {
      struct zword zgaz387;
      CREATE(zword)(&zgaz387);
      zNum(&zgaz387, zuz345);
      unit zgsz3144;
      zgsz3144 = zwriteReg(zuz342, zgaz387);
      KILL(zword)(&zgaz387);
    }
    {
      unit zgsz3143;
      zgsz3143 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_174: ;
  {
    if (zmergez3var.kind != Kind_zSUB) goto case_175;
    uint64_t zuz346;
    zuz346 = zmergez3var.zSUB.ztup0;
    uint64_t zuz347;
    zuz347 = zmergez3var.zSUB.ztup1;
    uint64_t zuz348;
    zuz348 = zmergez3var.zSUB.ztup2;
    uint64_t zuz349;
    {
      zuz349 = zreadRegNum(zuz347);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zuz350;
    {
      zuz350 = zreadRegNum(zuz348);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    sail_int zuz351;
    CREATE(sail_int)(&zuz351);
    {
      int64_t zgaz390;
      {
        lbits zgsz3420;
        CREATE(lbits)(&zgsz3420);
        CONVERT_OF(lbits, fbits)(&zgsz3420, zuz349, UINT64_C(64) , true);
        sail_int zgsz3421;
        CREATE(sail_int)(&zgsz3421);
        sail_signed(&zgsz3421, zgsz3420);
        zgaz390 = CONVERT_OF(mach_int, sail_int)(zgsz3421);
        KILL(sail_int)(&zgsz3421);
        KILL(lbits)(&zgsz3420);
      }
      int64_t zgaz391;
      {
        lbits zgsz3418;
        CREATE(lbits)(&zgsz3418);
        CONVERT_OF(lbits, fbits)(&zgsz3418, zuz350, UINT64_C(64) , true);
        sail_int zgsz3419;
        CREATE(sail_int)(&zgsz3419);
        sail_signed(&zgsz3419, zgsz3418);
        zgaz391 = CONVERT_OF(mach_int, sail_int)(zgsz3419);
        KILL(sail_int)(&zgsz3419);
        KILL(lbits)(&zgsz3418);
      }
      {
        sail_int zgsz3416;
        CREATE(sail_int)(&zgsz3416);
        CONVERT_OF(sail_int, mach_int)(&zgsz3416, zgaz390);
        sail_int zgsz3417;
        CREATE(sail_int)(&zgsz3417);
        CONVERT_OF(sail_int, mach_int)(&zgsz3417, zgaz391);
        sub_int(&zuz351, zgsz3416, zgsz3417);
        KILL(sail_int)(&zgsz3417);
        KILL(sail_int)(&zgsz3416);
      }
    }
    {
      struct zword zgaz389;
      CREATE(zword)(&zgaz389);
      {
        uint64_t zgaz388;
        {
          sail_int zgsz3422;
          CREATE(sail_int)(&zgsz3422);
          CONVERT_OF(sail_int, mach_int)(&zgsz3422, zinteger_sizze);
          sail_int zgsz3423;
          CREATE(sail_int)(&zgsz3423);
          CONVERT_OF(sail_int, mach_int)(&zgsz3423, INT64_C(0));
          lbits zgsz3424;
          CREATE(lbits)(&zgsz3424);
          get_slice_int(&zgsz3424, zgsz3422, zuz351, zgsz3423);
          zgaz388 = CONVERT_OF(fbits, lbits)(zgsz3424, true);
          KILL(lbits)(&zgsz3424);
          KILL(sail_int)(&zgsz3423);
          KILL(sail_int)(&zgsz3422);
        }
        zNum(&zgaz389, zgaz388);
      }
      unit zgsz3147;
      zgsz3147 = zwriteReg(zuz346, zgaz389);
      KILL(zword)(&zgaz389);
    }
    {
      unit zgsz3146;
      zgsz3146 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(sail_int)(&zuz351);
    goto finish_match_152;
  }
case_175: ;
  {
    if (zmergez3var.kind != Kind_zSLT) goto case_176;
    uint64_t zuz352;
    zuz352 = zmergez3var.zSLT.ztup0;
    uint64_t zuz353;
    zuz353 = zmergez3var.zSLT.ztup1;
    uint64_t zuz354;
    zuz354 = zmergez3var.zSLT.ztup2;
    uint64_t zuz355;
    {
      zuz355 = zreadRegNum(zuz353);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zuz356;
    {
      zuz356 = zreadRegNum(zuz354);
      if (have_exception) {






        goto end_block_exception_191;
      }
    }
    {
      bool zgaz394;
      {
        int64_t zgaz392;
        {
          lbits zgsz3429;
          CREATE(lbits)(&zgsz3429);
          CONVERT_OF(lbits, fbits)(&zgsz3429, zuz355, UINT64_C(64) , true);
          sail_int zgsz3430;
          CREATE(sail_int)(&zgsz3430);
          sail_signed(&zgsz3430, zgsz3429);
          zgaz392 = CONVERT_OF(mach_int, sail_int)(zgsz3430);
          KILL(sail_int)(&zgsz3430);
          KILL(lbits)(&zgsz3429);
        }
        int64_t zgaz393;
        {
          lbits zgsz3427;
          CREATE(lbits)(&zgsz3427);
          CONVERT_OF(lbits, fbits)(&zgsz3427, zuz356, UINT64_C(64) , true);
          sail_int zgsz3428;
          CREATE(sail_int)(&zgsz3428);
          sail_signed(&zgsz3428, zgsz3427);
          zgaz393 = CONVERT_OF(mach_int, sail_int)(zgsz3428);
          KILL(sail_int)(&zgsz3428);
          KILL(lbits)(&zgsz3427);
        }
        {
          sail_int zgsz3425;
          CREATE(sail_int)(&zgsz3425);
          CONVERT_OF(sail_int, mach_int)(&zgsz3425, zgaz392);
          sail_int zgsz3426;
          CREATE(sail_int)(&zgsz3426);
          CONVERT_OF(sail_int, mach_int)(&zgsz3426, zgaz393);
          zgaz394 = lt(zgsz3425, zgsz3426);
          KILL(sail_int)(&zgsz3426);
          KILL(sail_int)(&zgsz3425);
        }
      }
      unit zgsz3150;
      if (zgaz394) {
        struct zword zgaz396;
        CREATE(zword)(&zgaz396);
        {
          uint64_t zgaz395;
          {
            lbits zgsz3434;
            CREATE(lbits)(&zgsz3434);
            CONVERT_OF(lbits, fbits)(&zgsz3434, UINT64_C(0b1), UINT64_C(1) , true);
            sail_int zgsz3435;
            CREATE(sail_int)(&zgsz3435);
            CONVERT_OF(sail_int, mach_int)(&zgsz3435, zinteger_sizze);
            lbits zgsz3436;
            CREATE(lbits)(&zgsz3436);
            zero_extend(&zgsz3436, zgsz3434, zgsz3435);
            zgaz395 = CONVERT_OF(fbits, lbits)(zgsz3436, true);
            KILL(lbits)(&zgsz3436);
            KILL(sail_int)(&zgsz3435);
            KILL(lbits)(&zgsz3434);
          }
          zNum(&zgaz396, zgaz395);
        }
        zgsz3150 = zwriteReg(zuz352, zgaz396);
        KILL(zword)(&zgaz396);
      } else {
        struct zword zgaz398;
        CREATE(zword)(&zgaz398);
        {
          uint64_t zgaz397;
          {
            lbits zgsz3431;
            CREATE(lbits)(&zgsz3431);
            CONVERT_OF(lbits, fbits)(&zgsz3431, UINT64_C(0b0), UINT64_C(1) , true);
            sail_int zgsz3432;
            CREATE(sail_int)(&zgsz3432);
            CONVERT_OF(sail_int, mach_int)(&zgsz3432, zinteger_sizze);
            lbits zgsz3433;
            CREATE(lbits)(&zgsz3433);
            zero_extend(&zgsz3433, zgsz3431, zgsz3432);
            zgaz397 = CONVERT_OF(fbits, lbits)(zgsz3433, true);
            KILL(lbits)(&zgsz3433);
            KILL(sail_int)(&zgsz3432);
            KILL(lbits)(&zgsz3431);
          }
          zNum(&zgaz398, zgaz397);
        }
        zgsz3150 = zwriteReg(zuz352, zgaz398);
        KILL(zword)(&zgaz398);
      }
    }
    {
      unit zgsz3149;
      zgsz3149 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_176: ;
  {
    if (zmergez3var.kind != Kind_zSLTI) goto case_177;
    uint64_t zuz357;
    zuz357 = zmergez3var.zSLTI.ztup0;
    uint64_t zuz358;
    zuz358 = zmergez3var.zSLTI.ztup1;
    uint64_t zuz359;
    zuz359 = zmergez3var.zSLTI.ztup2;
    uint64_t zuz360;
    {
      zuz360 = zreadRegNum(zuz358);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zuz361;
    {
      lbits zgsz3449;
      CREATE(lbits)(&zgsz3449);
      CONVERT_OF(lbits, fbits)(&zgsz3449, zuz359, UINT64_C(12) , true);
      sail_int zgsz3450;
      CREATE(sail_int)(&zgsz3450);
      CONVERT_OF(sail_int, mach_int)(&zgsz3450, zinteger_sizze);
      lbits zgsz3451;
      CREATE(lbits)(&zgsz3451);
      sign_extend(&zgsz3451, zgsz3449, zgsz3450);
      zuz361 = CONVERT_OF(fbits, lbits)(zgsz3451, true);
      KILL(lbits)(&zgsz3451);
      KILL(sail_int)(&zgsz3450);
      KILL(lbits)(&zgsz3449);
    }
    {
      bool zgaz3101;
      {
        int64_t zgaz399;
        {
          lbits zgsz3441;
          CREATE(lbits)(&zgsz3441);
          CONVERT_OF(lbits, fbits)(&zgsz3441, zuz360, UINT64_C(64) , true);
          sail_int zgsz3442;
          CREATE(sail_int)(&zgsz3442);
          sail_signed(&zgsz3442, zgsz3441);
          zgaz399 = CONVERT_OF(mach_int, sail_int)(zgsz3442);
          KILL(sail_int)(&zgsz3442);
          KILL(lbits)(&zgsz3441);
        }
        int64_t zgaz3100;
        {
          lbits zgsz3439;
          CREATE(lbits)(&zgsz3439);
          CONVERT_OF(lbits, fbits)(&zgsz3439, zuz361, UINT64_C(64) , true);
          sail_int zgsz3440;
          CREATE(sail_int)(&zgsz3440);
          sail_signed(&zgsz3440, zgsz3439);
          zgaz3100 = CONVERT_OF(mach_int, sail_int)(zgsz3440);
          KILL(sail_int)(&zgsz3440);
          KILL(lbits)(&zgsz3439);
        }
        {
          sail_int zgsz3437;
          CREATE(sail_int)(&zgsz3437);
          CONVERT_OF(sail_int, mach_int)(&zgsz3437, zgaz399);
          sail_int zgsz3438;
          CREATE(sail_int)(&zgsz3438);
          CONVERT_OF(sail_int, mach_int)(&zgsz3438, zgaz3100);
          zgaz3101 = lt(zgsz3437, zgsz3438);
          KILL(sail_int)(&zgsz3438);
          KILL(sail_int)(&zgsz3437);
        }
      }
      unit zgsz3153;
      if (zgaz3101) {
        struct zword zgaz3103;
        CREATE(zword)(&zgaz3103);
        {
          uint64_t zgaz3102;
          {
            lbits zgsz3446;
            CREATE(lbits)(&zgsz3446);
            CONVERT_OF(lbits, fbits)(&zgsz3446, UINT64_C(0b1), UINT64_C(1) , true);
            sail_int zgsz3447;
            CREATE(sail_int)(&zgsz3447);
            CONVERT_OF(sail_int, mach_int)(&zgsz3447, zinteger_sizze);
            lbits zgsz3448;
            CREATE(lbits)(&zgsz3448);
            zero_extend(&zgsz3448, zgsz3446, zgsz3447);
            zgaz3102 = CONVERT_OF(fbits, lbits)(zgsz3448, true);
            KILL(lbits)(&zgsz3448);
            KILL(sail_int)(&zgsz3447);
            KILL(lbits)(&zgsz3446);
          }
          zNum(&zgaz3103, zgaz3102);
        }
        zgsz3153 = zwriteReg(zuz357, zgaz3103);
        KILL(zword)(&zgaz3103);
      } else {
        struct zword zgaz3105;
        CREATE(zword)(&zgaz3105);
        {
          uint64_t zgaz3104;
          {
            lbits zgsz3443;
            CREATE(lbits)(&zgsz3443);
            CONVERT_OF(lbits, fbits)(&zgsz3443, UINT64_C(0b0), UINT64_C(1) , true);
            sail_int zgsz3444;
            CREATE(sail_int)(&zgsz3444);
            CONVERT_OF(sail_int, mach_int)(&zgsz3444, zinteger_sizze);
            lbits zgsz3445;
            CREATE(lbits)(&zgsz3445);
            zero_extend(&zgsz3445, zgsz3443, zgsz3444);
            zgaz3104 = CONVERT_OF(fbits, lbits)(zgsz3445, true);
            KILL(lbits)(&zgsz3445);
            KILL(sail_int)(&zgsz3444);
            KILL(lbits)(&zgsz3443);
          }
          zNum(&zgaz3105, zgaz3104);
        }
        zgsz3153 = zwriteReg(zuz357, zgaz3105);
        KILL(zword)(&zgaz3105);
      }
    }
    {
      unit zgsz3152;
      zgsz3152 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    goto finish_match_152;
  }
case_177: ;
  {
    if (zmergez3var.kind != Kind_zSLTU) goto case_178;
    uint64_t zuz362;
    zuz362 = zmergez3var.zSLTU.ztup0;
    uint64_t zuz363;
    zuz363 = zmergez3var.zSLTU.ztup1;
    uint64_t zrb;
    zrb = zmergez3var.zSLTU.ztup2;
    uint64_t zuz364;
    {
      zuz364 = zreadRegNum(zuz363);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    sail_int zuv1;
    CREATE(sail_int)(&zuv1);
    {
      lbits zgsz3459;
      CREATE(lbits)(&zgsz3459);
      CONVERT_OF(lbits, fbits)(&zgsz3459, zuz364, UINT64_C(64) , true);
      sail_unsigned(&zuv1, zgsz3459);
      KILL(lbits)(&zgsz3459);
    }
    uint64_t zuz365;
    {
      zuz365 = zreadRegNum(zrb);
      if (have_exception) {





        KILL(sail_int)(&zuv1);

        goto end_block_exception_191;
      }
    }
    sail_int zuv2;
    CREATE(sail_int)(&zuv2);
    {
      lbits zgsz3458;
      CREATE(lbits)(&zgsz3458);
      CONVERT_OF(lbits, fbits)(&zgsz3458, zuz365, UINT64_C(64) , true);
      sail_unsigned(&zuv2, zgsz3458);
      KILL(lbits)(&zgsz3458);
    }
    {
      bool zgaz3106;
      zgaz3106 = lt(zuv1, zuv2);
      unit zgsz3156;
      if (zgaz3106) {
        struct zword zgaz3108;
        CREATE(zword)(&zgaz3108);
        {
          uint64_t zgaz3107;
          {
            lbits zgsz3455;
            CREATE(lbits)(&zgsz3455);
            CONVERT_OF(lbits, fbits)(&zgsz3455, UINT64_C(0b1), UINT64_C(1) , true);
            sail_int zgsz3456;
            CREATE(sail_int)(&zgsz3456);
            CONVERT_OF(sail_int, mach_int)(&zgsz3456, zinteger_sizze);
            lbits zgsz3457;
            CREATE(lbits)(&zgsz3457);
            zero_extend(&zgsz3457, zgsz3455, zgsz3456);
            zgaz3107 = CONVERT_OF(fbits, lbits)(zgsz3457, true);
            KILL(lbits)(&zgsz3457);
            KILL(sail_int)(&zgsz3456);
            KILL(lbits)(&zgsz3455);
          }
          zNum(&zgaz3108, zgaz3107);
        }
        zgsz3156 = zwriteReg(zuz362, zgaz3108);
        KILL(zword)(&zgaz3108);
      } else {
        struct zword zgaz3110;
        CREATE(zword)(&zgaz3110);
        {
          uint64_t zgaz3109;
          {
            lbits zgsz3452;
            CREATE(lbits)(&zgsz3452);
            CONVERT_OF(lbits, fbits)(&zgsz3452, UINT64_C(0b0), UINT64_C(1) , true);
            sail_int zgsz3453;
            CREATE(sail_int)(&zgsz3453);
            CONVERT_OF(sail_int, mach_int)(&zgsz3453, zinteger_sizze);
            lbits zgsz3454;
            CREATE(lbits)(&zgsz3454);
            zero_extend(&zgsz3454, zgsz3452, zgsz3453);
            zgaz3109 = CONVERT_OF(fbits, lbits)(zgsz3454, true);
            KILL(lbits)(&zgsz3454);
            KILL(sail_int)(&zgsz3453);
            KILL(lbits)(&zgsz3452);
          }
          zNum(&zgaz3110, zgaz3109);
        }
        zgsz3156 = zwriteReg(zuz362, zgaz3110);
        KILL(zword)(&zgaz3110);
      }
    }
    {
      unit zgsz3155;
      zgsz3155 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(sail_int)(&zuv2);
    KILL(sail_int)(&zuv1);
    goto finish_match_152;
  }
case_178: ;
  {
    if (zmergez3var.kind != Kind_zSLTIU) goto case_179;
    uint64_t zuz366;
    zuz366 = zmergez3var.zSLTIU.ztup0;
    uint64_t zuz367;
    zuz367 = zmergez3var.zSLTIU.ztup1;
    uint64_t zimmediate;
    zimmediate = zmergez3var.zSLTIU.ztup2;
    uint64_t zuz368;
    {
      zuz368 = zreadRegNum(zuz367);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    sail_int zuz369;
    CREATE(sail_int)(&zuz369);
    {
      lbits zgsz3470;
      CREATE(lbits)(&zgsz3470);
      CONVERT_OF(lbits, fbits)(&zgsz3470, zuz368, UINT64_C(64) , true);
      sail_unsigned(&zuz369, zgsz3470);
      KILL(lbits)(&zgsz3470);
    }
    uint64_t zuz370;
    {
      lbits zgsz3467;
      CREATE(lbits)(&zgsz3467);
      CONVERT_OF(lbits, fbits)(&zgsz3467, zimmediate, UINT64_C(12) , true);
      sail_int zgsz3468;
      CREATE(sail_int)(&zgsz3468);
      CONVERT_OF(sail_int, mach_int)(&zgsz3468, zinteger_sizze);
      lbits zgsz3469;
      CREATE(lbits)(&zgsz3469);
      sign_extend(&zgsz3469, zgsz3467, zgsz3468);
      zuz370 = CONVERT_OF(fbits, lbits)(zgsz3469, true);
      KILL(lbits)(&zgsz3469);
      KILL(sail_int)(&zgsz3468);
      KILL(lbits)(&zgsz3467);
    }
    sail_int zuz371;
    CREATE(sail_int)(&zuz371);
    {
      lbits zgsz3466;
      CREATE(lbits)(&zgsz3466);
      CONVERT_OF(lbits, fbits)(&zgsz3466, zuz370, UINT64_C(64) , true);
      sail_unsigned(&zuz371, zgsz3466);
      KILL(lbits)(&zgsz3466);
    }
    {
      bool zgaz3111;
      zgaz3111 = lt(zuz369, zuz371);
      unit zgsz3159;
      if (zgaz3111) {
        struct zword zgaz3113;
        CREATE(zword)(&zgaz3113);
        {
          uint64_t zgaz3112;
          {
            lbits zgsz3463;
            CREATE(lbits)(&zgsz3463);
            CONVERT_OF(lbits, fbits)(&zgsz3463, UINT64_C(0b1), UINT64_C(1) , true);
            sail_int zgsz3464;
            CREATE(sail_int)(&zgsz3464);
            CONVERT_OF(sail_int, mach_int)(&zgsz3464, zinteger_sizze);
            lbits zgsz3465;
            CREATE(lbits)(&zgsz3465);
            zero_extend(&zgsz3465, zgsz3463, zgsz3464);
            zgaz3112 = CONVERT_OF(fbits, lbits)(zgsz3465, true);
            KILL(lbits)(&zgsz3465);
            KILL(sail_int)(&zgsz3464);
            KILL(lbits)(&zgsz3463);
          }
          zNum(&zgaz3113, zgaz3112);
        }
        zgsz3159 = zwriteReg(zuz366, zgaz3113);
        KILL(zword)(&zgaz3113);
      } else {
        struct zword zgaz3115;
        CREATE(zword)(&zgaz3115);
        {
          uint64_t zgaz3114;
          {
            lbits zgsz3460;
            CREATE(lbits)(&zgsz3460);
            CONVERT_OF(lbits, fbits)(&zgsz3460, UINT64_C(0b0), UINT64_C(1) , true);
            sail_int zgsz3461;
            CREATE(sail_int)(&zgsz3461);
            CONVERT_OF(sail_int, mach_int)(&zgsz3461, zinteger_sizze);
            lbits zgsz3462;
            CREATE(lbits)(&zgsz3462);
            zero_extend(&zgsz3462, zgsz3460, zgsz3461);
            zgaz3114 = CONVERT_OF(fbits, lbits)(zgsz3462, true);
            KILL(lbits)(&zgsz3462);
            KILL(sail_int)(&zgsz3461);
            KILL(lbits)(&zgsz3460);
          }
          zNum(&zgaz3115, zgaz3114);
        }
        zgsz3159 = zwriteReg(zuz366, zgaz3115);
        KILL(zword)(&zgaz3115);
      }
    }
    {
      unit zgsz3158;
      zgsz3158 = zupdatePC(UNIT);
    }
    zgsz386 = true;
    KILL(sail_int)(&zuz371);
    KILL(sail_int)(&zuz369);
    goto finish_match_152;
  }
case_179: ;
  {
    __label__ case_182, case_183, finish_match_181;

    if (zmergez3var.kind != Kind_zCSETBOUNDS) goto case_180;
    uint64_t zuz372;
    zuz372 = zmergez3var.zCSETBOUNDS.ztup0;
    uint64_t zuz373;
    zuz373 = zmergez3var.zCSETBOUNDS.ztup1;
    uint64_t zuz374;
    zuz374 = zmergez3var.zCSETBOUNDS.ztup2;
    struct zCapability zuz375;
    {
      zuz375 = zreadRegCap(zuz373);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t znew_begin;
    znew_begin = zuz375.zcap_cursor;
    uint64_t zuz376;
    {
      zuz376 = zreadRegNum(zuz374);
      if (have_exception) {







        goto end_block_exception_191;
      }
    }
    uint64_t znew_end;
    {
      lbits zgsz3471;
      CREATE(lbits)(&zgsz3471);
      CONVERT_OF(lbits, fbits)(&zgsz3471, znew_begin, UINT64_C(64) , true);
      lbits zgsz3472;
      CREATE(lbits)(&zgsz3472);
      CONVERT_OF(lbits, fbits)(&zgsz3472, zuz376, UINT64_C(64) , true);
      lbits zgsz3473;
      CREATE(lbits)(&zgsz3473);
      add_bits(&zgsz3473, zgsz3471, zgsz3472);
      znew_end = CONVERT_OF(fbits, lbits)(zgsz3473, true);
      KILL(lbits)(&zgsz3473);
      KILL(lbits)(&zgsz3472);
      KILL(lbits)(&zgsz3471);
    }
    enum zPermission zgaz3116;
    zgaz3116 = zuz375.zcap_permission;
    /* Case with num_cases: 2 */
    bool zgsz3161;
    {
      if ((zE != zgaz3116)) goto case_182;
      struct zexception zgaz3117;
      CREATE(zexception)(&zgaz3117);
      zCSETBOUNDSOnEnterCapability(&zgaz3117, UNIT);
      COPY(zexception)(current_exception, zgaz3117);
      have_exception = true;
      COPY(sail_string)(throw_location, "./instructions.sail:321.9-321.45");
      KILL(zexception)(&zgaz3117);
      goto end_block_exception_191;
      /* unreachable after throw */
      KILL(zexception)(&zgaz3117);
      goto finish_match_181;
    }
  case_182: ;
    {
      bool zuz377;
      {
        uint64_t zgaz3119;
        zgaz3119 = zuz375.zcap_begin;
        uint64_t zgaz3120;
        zgaz3120 = zuz375.zcap_end;
        zuz377 = zisWithinRange(znew_begin, znew_end, zgaz3119, zgaz3120);
      }
      {
        unit zgsz3163;
        zgsz3163 = sail_assert(zuz377, "Err: [csetbounds] tried to increase range of authority");
      }
      struct zCapability zuz378;
      {
        struct zCapability zgsz3164;
        zgsz3164 = zuz375;
        zgsz3164.zcap_begin = znew_begin;
        zgsz3164.zcap_end = znew_end;
        zuz378 = zgsz3164;
      }
      {
        struct zword zgaz3118;
        CREATE(zword)(&zgaz3118);
        zCap(&zgaz3118, zuz378);
        unit zgsz3166;
        zgsz3166 = zwriteReg(zuz372, zgaz3118);
        KILL(zword)(&zgaz3118);
      }
      {
        unit zgsz3165;
        zgsz3165 = zupdatePC(UNIT);
      }
      zgsz3161 = true;
      goto finish_match_181;
    }
  case_183: ;
    sail_match_failure("execute");
  finish_match_181: ;
    zgsz386 = zgsz3161;
    goto finish_match_152;
  }
case_180: ;
  {
    __label__ case_186, case_187, finish_match_185;

    if (zmergez3var.kind != Kind_zCSETBOUNDSIMM) goto case_184;
    uint64_t zuz379;
    zuz379 = zmergez3var.zCSETBOUNDSIMM.ztup0;
    uint64_t zuz380;
    zuz380 = zmergez3var.zCSETBOUNDSIMM.ztup1;
    uint64_t zuz381;
    zuz381 = zmergez3var.zCSETBOUNDSIMM.ztup2;
    struct zCapability zuz382;
    {
      zuz382 = zreadRegCap(zuz380);
      if (have_exception) {





        goto end_block_exception_191;
      }
    }
    uint64_t zuz383;
    zuz383 = zuz382.zcap_cursor;
    uint64_t zuz384;
    {
      uint64_t zgaz3126;
      {
        lbits zgsz3477;
        CREATE(lbits)(&zgsz3477);
        CONVERT_OF(lbits, fbits)(&zgsz3477, zuz381, UINT64_C(12) , true);
        sail_int zgsz3478;
        CREATE(sail_int)(&zgsz3478);
        CONVERT_OF(sail_int, mach_int)(&zgsz3478, zinteger_sizze);
        lbits zgsz3479;
        CREATE(lbits)(&zgsz3479);
        zero_extend(&zgsz3479, zgsz3477, zgsz3478);
        zgaz3126 = CONVERT_OF(fbits, lbits)(zgsz3479, true);
        KILL(lbits)(&zgsz3479);
        KILL(sail_int)(&zgsz3478);
        KILL(lbits)(&zgsz3477);
      }
      {
        lbits zgsz3474;
        CREATE(lbits)(&zgsz3474);
        CONVERT_OF(lbits, fbits)(&zgsz3474, zuz383, UINT64_C(64) , true);
        lbits zgsz3475;
        CREATE(lbits)(&zgsz3475);
        CONVERT_OF(lbits, fbits)(&zgsz3475, zgaz3126, UINT64_C(64) , true);
        lbits zgsz3476;
        CREATE(lbits)(&zgsz3476);
        add_bits(&zgsz3476, zgsz3474, zgsz3475);
        zuz384 = CONVERT_OF(fbits, lbits)(zgsz3476, true);
        KILL(lbits)(&zgsz3476);
        KILL(lbits)(&zgsz3475);
        KILL(lbits)(&zgsz3474);
      }
    }
    enum zPermission zgaz3121;
    zgaz3121 = zuz382.zcap_permission;
    /* Case with num_cases: 2 */
    bool zgsz3169;
    {
      if ((zE != zgaz3121)) goto case_186;
      struct zexception zgaz3122;
      CREATE(zexception)(&zgaz3122);
      zCSETBOUNDSOnEnterCapability(&zgaz3122, UNIT);
      COPY(zexception)(current_exception, zgaz3122);
      have_exception = true;
      COPY(sail_string)(throw_location, "./instructions.sail:338.9-338.45");
      KILL(zexception)(&zgaz3122);
      goto end_block_exception_191;
      /* unreachable after throw */
      KILL(zexception)(&zgaz3122);
      goto finish_match_185;
    }
  case_186: ;
    {
      bool zuz385;
      {
        uint64_t zgaz3124;
        zgaz3124 = zuz382.zcap_begin;
        uint64_t zgaz3125;
        zgaz3125 = zuz382.zcap_end;
        zuz385 = zisWithinRange(zuz383, zuz384, zgaz3124, zgaz3125);
      }
      {
        unit zgsz3171;
        zgsz3171 = sail_assert(zuz385, "Err: [csetbounds] tried to increase range of authority");
      }
      struct zCapability zuz386;
      {
        struct zCapability zgsz3172;
        zgsz3172 = zuz382;
        zgsz3172.zcap_begin = zuz383;
        zgsz3172.zcap_end = zuz384;
        zuz386 = zgsz3172;
      }
      {
        struct zword zgaz3123;
        CREATE(zword)(&zgaz3123);
        zCap(&zgaz3123, zuz386);
        unit zgsz3174;
        zgsz3174 = zwriteReg(zuz379, zgaz3123);
        KILL(zword)(&zgaz3123);
      }
      {
        unit zgsz3173;
        zgsz3173 = zupdatePC(UNIT);
      }
      zgsz3169 = true;
      goto finish_match_185;
    }
  case_187: ;
    sail_match_failure("execute");
  finish_match_185: ;
    zgsz386 = zgsz3169;
    goto finish_match_152;
  }
case_184: ;
  {
    if (zmergez3var.kind != Kind_zRET) goto case_188;
    zgsz386 = false;
    goto finish_match_152;
  }
case_188: ;
  {
    if (zmergez3var.kind != Kind_zFAIL) goto case_189;
    struct zexception zgaz3127;
    CREATE(zexception)(&zgaz3127);
    zFail(&zgaz3127, UNIT);
    COPY(zexception)(current_exception, zgaz3127);
    have_exception = true;
    COPY(sail_string)(throw_location, "./instructions.sail:352.34-352.47");
    KILL(zexception)(&zgaz3127);
    goto end_block_exception_191;
    /* unreachable after throw */
    KILL(zexception)(&zgaz3127);
    goto finish_match_152;
  }
case_189: ;
  sail_match_failure("execute");
finish_match_152: ;
  zcbz328 = zgsz386;

end_function_190: ;
  return zcbz328;
end_block_exception_191: ;

  return false;
}

uint64_t ztREG(uint64_t);

uint64_t ztREG(uint64_t zrs)
{
  __label__ case_194, case_195, case_196, case_197, case_198, finish_match_193, end_function_199, end_block_exception_200;

  uint64_t zcbz329;
  /* Case with num_cases: 5 */
  uint64_t zgsz3179;
  {
    uint64_t zb__0;
    zb__0 = zrs;
    bool zgsz3180;
    {
      lbits zgsz3480;
      CREATE(lbits)(&zgsz3480);
      CONVERT_OF(lbits, fbits)(&zgsz3480, zb__0, UINT64_C(5) , true);
      lbits zgsz3481;
      CREATE(lbits)(&zgsz3481);
      CONVERT_OF(lbits, fbits)(&zgsz3481, UINT64_C(0b01010), UINT64_C(5) , true);
      zgsz3180 = eq_bits(zgsz3480, zgsz3481);
      KILL(lbits)(&zgsz3481);
      KILL(lbits)(&zgsz3480);
    }
    if (!(zgsz3180)) {

      goto case_194;
    }
    zgsz3179 = UINT64_C(0b00);
    goto finish_match_193;
  }
case_194: ;
  {
    uint64_t zb__1;
    zb__1 = zrs;
    bool zgsz3181;
    {
      lbits zgsz3482;
      CREATE(lbits)(&zgsz3482);
      CONVERT_OF(lbits, fbits)(&zgsz3482, zb__1, UINT64_C(5) , true);
      lbits zgsz3483;
      CREATE(lbits)(&zgsz3483);
      CONVERT_OF(lbits, fbits)(&zgsz3483, UINT64_C(0b01011), UINT64_C(5) , true);
      zgsz3181 = eq_bits(zgsz3482, zgsz3483);
      KILL(lbits)(&zgsz3483);
      KILL(lbits)(&zgsz3482);
    }
    if (!(zgsz3181)) {

      goto case_195;
    }
    zgsz3179 = UINT64_C(0b01);
    goto finish_match_193;
  }
case_195: ;
  {
    uint64_t zb__2;
    zb__2 = zrs;
    bool zgsz3182;
    {
      lbits zgsz3484;
      CREATE(lbits)(&zgsz3484);
      CONVERT_OF(lbits, fbits)(&zgsz3484, zb__2, UINT64_C(5) , true);
      lbits zgsz3485;
      CREATE(lbits)(&zgsz3485);
      CONVERT_OF(lbits, fbits)(&zgsz3485, UINT64_C(0b01100), UINT64_C(5) , true);
      zgsz3182 = eq_bits(zgsz3484, zgsz3485);
      KILL(lbits)(&zgsz3485);
      KILL(lbits)(&zgsz3484);
    }
    if (!(zgsz3182)) {

      goto case_196;
    }
    zgsz3179 = UINT64_C(0b10);
    goto finish_match_193;
  }
case_196: ;
  {
    uint64_t zb__3;
    zb__3 = zrs;
    bool zgsz3183;
    {
      lbits zgsz3486;
      CREATE(lbits)(&zgsz3486);
      CONVERT_OF(lbits, fbits)(&zgsz3486, zb__3, UINT64_C(5) , true);
      lbits zgsz3487;
      CREATE(lbits)(&zgsz3487);
      CONVERT_OF(lbits, fbits)(&zgsz3487, UINT64_C(0b01101), UINT64_C(5) , true);
      zgsz3183 = eq_bits(zgsz3486, zgsz3487);
      KILL(lbits)(&zgsz3487);
      KILL(lbits)(&zgsz3486);
    }
    if (!(zgsz3183)) {

      goto case_197;
    }
    zgsz3179 = UINT64_C(0b11);
    goto finish_match_193;
  }
case_197: ;
  {
    zgsz3179 = UINT64_C(0b00);
    goto finish_match_193;
  }
case_198: ;
  sail_match_failure("tREG");
finish_match_193: ;
  zcbz329 = zgsz3179;

end_function_199: ;
  return zcbz329;
end_block_exception_200: ;

  return UINT64_C(0xdeadc0de);
}























void zdecode(struct zoptionzIUastzIzKzK *rop, uint64_t);


















































void zdecode(struct zoptionzIUastzIzKzK *zcbz330, uint64_t zmergez3var)
{
  __label__ case_203, case_204, case_205, case_206, case_207, case_208, case_209, case_210, case_211, case_212, case_213, case_214, case_215, case_216, case_217, case_218, case_219, case_220, case_221, case_222, case_223, case_224, case_225, case_226, case_227, case_228, finish_match_202, end_function_229, end_block_exception_230, end_function_254;

  /* Case with num_cases: 26 */
  struct zoptionzIUastzIzKzK zgsz3185;
  CREATE(zoptionzIUastzIzKzK)(&zgsz3185);
  {
    uint64_t zv__0;
    zv__0 = zmergez3var;
    bool zgsz3186;
    {
      lbits zgsz3488;
      CREATE(lbits)(&zgsz3488);
      CONVERT_OF(lbits, fbits)(&zgsz3488, zv__0, UINT64_C(32) , true);
      lbits zgsz3489;
      CREATE(lbits)(&zgsz3489);
      CONVERT_OF(lbits, fbits)(&zgsz3489, UINT64_C(0x00008067), UINT64_C(32) , true);
      zgsz3186 = eq_bits(zgsz3488, zgsz3489);
      KILL(lbits)(&zgsz3489);
      KILL(lbits)(&zgsz3488);
    }
    if (!(zgsz3186)) {

      goto case_203;
    }
    struct zast zgaz3128;
    CREATE(zast)(&zgaz3128);
    zRET(&zgaz3128, UNIT);
    zSomezIUastzIzKzK(&zgsz3185, zgaz3128);
    KILL(zast)(&zgaz3128);
    goto finish_match_202;
  }
case_203: ;
  {
    uint64_t zv__6;
    zv__6 = zmergez3var;
    bool zgsz3187;
    {
      lbits zgsz3490;
      CREATE(lbits)(&zgsz3490);
      CONVERT_OF(lbits, fbits)(&zgsz3490, zv__6, UINT64_C(32) , true);
      lbits zgsz3491;
      CREATE(lbits)(&zgsz3491);
      CONVERT_OF(lbits, fbits)(&zgsz3491, UINT64_C(0x00000067), UINT64_C(32) , true);
      zgsz3187 = eq_bits(zgsz3490, zgsz3491);
      KILL(lbits)(&zgsz3491);
      KILL(lbits)(&zgsz3490);
    }
    if (!(zgsz3187)) {

      goto case_204;
    }
    struct zast zgaz3129;
    CREATE(zast)(&zgaz3129);
    zFAIL(&zgaz3129, UNIT);
    zSomezIUastzIzKzK(&zgsz3185, zgaz3129);
    KILL(zast)(&zgaz3129);
    goto finish_match_202;
  }
case_204: ;
  {
    uint64_t zv__12;
    zv__12 = zmergez3var;
    bool zgaz3138;
    {
      uint64_t zgaz3134;
      {
        lbits zgsz3494;
        CREATE(lbits)(&zgsz3494);
        CONVERT_OF(lbits, fbits)(&zgsz3494, zv__12, UINT64_C(32) , true);
        sail_int zgsz3495;
        CREATE(sail_int)(&zgsz3495);
        CONVERT_OF(sail_int, mach_int)(&zgsz3495, INT64_C(31));
        sail_int zgsz3496;
        CREATE(sail_int)(&zgsz3496);
        CONVERT_OF(sail_int, mach_int)(&zgsz3496, INT64_C(20));
        lbits zgsz3497;
        CREATE(lbits)(&zgsz3497);
        vector_subrange_lbits(&zgsz3497, zgsz3494, zgsz3495, zgsz3496);
        zgaz3134 = CONVERT_OF(fbits, lbits)(zgsz3497, true);
        KILL(lbits)(&zgsz3497);
        KILL(sail_int)(&zgsz3496);
        KILL(sail_int)(&zgsz3495);
        KILL(lbits)(&zgsz3494);
      }
      {
        lbits zgsz3492;
        CREATE(lbits)(&zgsz3492);
        CONVERT_OF(lbits, fbits)(&zgsz3492, zgaz3134, UINT64_C(12) , true);
        lbits zgsz3493;
        CREATE(lbits)(&zgsz3493);
        CONVERT_OF(lbits, fbits)(&zgsz3493, UINT64_C(0xFEC), UINT64_C(12) , true);
        zgaz3138 = eq_bits(zgsz3492, zgsz3493);
        KILL(lbits)(&zgsz3493);
        KILL(lbits)(&zgsz3492);
      }
    }
    bool zgsz3189;
    if (zgaz3138) {
      bool zgaz3137;
      {
        uint64_t zgaz3135;
        {
          lbits zgsz3500;
          CREATE(lbits)(&zgsz3500);
          CONVERT_OF(lbits, fbits)(&zgsz3500, zv__12, UINT64_C(32) , true);
          sail_int zgsz3501;
          CREATE(sail_int)(&zgsz3501);
          CONVERT_OF(sail_int, mach_int)(&zgsz3501, INT64_C(14));
          sail_int zgsz3502;
          CREATE(sail_int)(&zgsz3502);
          CONVERT_OF(sail_int, mach_int)(&zgsz3502, INT64_C(12));
          lbits zgsz3503;
          CREATE(lbits)(&zgsz3503);
          vector_subrange_lbits(&zgsz3503, zgsz3500, zgsz3501, zgsz3502);
          zgaz3135 = CONVERT_OF(fbits, lbits)(zgsz3503, true);
          KILL(lbits)(&zgsz3503);
          KILL(sail_int)(&zgsz3502);
          KILL(sail_int)(&zgsz3501);
          KILL(lbits)(&zgsz3500);
        }
        {
          lbits zgsz3498;
          CREATE(lbits)(&zgsz3498);
          CONVERT_OF(lbits, fbits)(&zgsz3498, zgaz3135, UINT64_C(3) , true);
          lbits zgsz3499;
          CREATE(lbits)(&zgsz3499);
          CONVERT_OF(lbits, fbits)(&zgsz3499, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3137 = eq_bits(zgsz3498, zgsz3499);
          KILL(lbits)(&zgsz3499);
          KILL(lbits)(&zgsz3498);
        }
      }
      bool zgsz3188;
      if (zgaz3137) {
        uint64_t zgaz3136;
        {
          lbits zgsz3506;
          CREATE(lbits)(&zgsz3506);
          CONVERT_OF(lbits, fbits)(&zgsz3506, zv__12, UINT64_C(32) , true);
          sail_int zgsz3507;
          CREATE(sail_int)(&zgsz3507);
          CONVERT_OF(sail_int, mach_int)(&zgsz3507, INT64_C(6));
          sail_int zgsz3508;
          CREATE(sail_int)(&zgsz3508);
          CONVERT_OF(sail_int, mach_int)(&zgsz3508, INT64_C(0));
          lbits zgsz3509;
          CREATE(lbits)(&zgsz3509);
          vector_subrange_lbits(&zgsz3509, zgsz3506, zgsz3507, zgsz3508);
          zgaz3136 = CONVERT_OF(fbits, lbits)(zgsz3509, true);
          KILL(lbits)(&zgsz3509);
          KILL(sail_int)(&zgsz3508);
          KILL(sail_int)(&zgsz3507);
          KILL(lbits)(&zgsz3506);
        }
        {
          lbits zgsz3504;
          CREATE(lbits)(&zgsz3504);
          CONVERT_OF(lbits, fbits)(&zgsz3504, zgaz3136, UINT64_C(7) , true);
          lbits zgsz3505;
          CREATE(lbits)(&zgsz3505);
          CONVERT_OF(lbits, fbits)(&zgsz3505, UINT64_C(0b1011011), UINT64_C(7) , true);
          zgsz3188 = eq_bits(zgsz3504, zgsz3505);
          KILL(lbits)(&zgsz3505);
          KILL(lbits)(&zgsz3504);
        }
      } else {  zgsz3188 = false;  }
      zgsz3189 = zgsz3188;
    } else {  zgsz3189 = false;  }
    bool zgsz3191;
    zgsz3191 = zgsz3189;
    if (!(zgsz3191)) {

      goto case_205;
    }
    uint64_t zcs;
    {
      lbits zgsz3514;
      CREATE(lbits)(&zgsz3514);
      CONVERT_OF(lbits, fbits)(&zgsz3514, zv__12, UINT64_C(32) , true);
      sail_int zgsz3515;
      CREATE(sail_int)(&zgsz3515);
      CONVERT_OF(sail_int, mach_int)(&zgsz3515, INT64_C(19));
      sail_int zgsz3516;
      CREATE(sail_int)(&zgsz3516);
      CONVERT_OF(sail_int, mach_int)(&zgsz3516, INT64_C(15));
      lbits zgsz3517;
      CREATE(lbits)(&zgsz3517);
      vector_subrange_lbits(&zgsz3517, zgsz3514, zgsz3515, zgsz3516);
      zcs = CONVERT_OF(fbits, lbits)(zgsz3517, true);
      KILL(lbits)(&zgsz3517);
      KILL(sail_int)(&zgsz3516);
      KILL(sail_int)(&zgsz3515);
      KILL(lbits)(&zgsz3514);
    }
    uint64_t zcd;
    {
      lbits zgsz3510;
      CREATE(lbits)(&zgsz3510);
      CONVERT_OF(lbits, fbits)(&zgsz3510, zv__12, UINT64_C(32) , true);
      sail_int zgsz3511;
      CREATE(sail_int)(&zgsz3511);
      CONVERT_OF(sail_int, mach_int)(&zgsz3511, INT64_C(11));
      sail_int zgsz3512;
      CREATE(sail_int)(&zgsz3512);
      CONVERT_OF(sail_int, mach_int)(&zgsz3512, INT64_C(7));
      lbits zgsz3513;
      CREATE(lbits)(&zgsz3513);
      vector_subrange_lbits(&zgsz3513, zgsz3510, zgsz3511, zgsz3512);
      zcd = CONVERT_OF(fbits, lbits)(zgsz3513, true);
      KILL(lbits)(&zgsz3513);
      KILL(sail_int)(&zgsz3512);
      KILL(sail_int)(&zgsz3511);
      KILL(lbits)(&zgsz3510);
    }
    struct zast zgaz3133;
    CREATE(zast)(&zgaz3133);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3132;
      {
        uint64_t zgaz3130;
        zgaz3130 = ztREG(zcd);
        uint64_t zgaz3131;
        zgaz3131 = ztREG(zcs);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3190;
        zgsz3190.ztup0 = zgaz3130;
        zgsz3190.ztup1 = zgaz3131;
        zgaz3132 = zgsz3190;
      }
      zJALR_CAP(&zgaz3133, zgaz3132);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3133);
    KILL(zast)(&zgaz3133);
    goto finish_match_202;
  }
case_205: ;
  {
    uint64_t zv__17;
    zv__17 = zmergez3var;
    bool zgaz3145;
    {
      uint64_t zgaz3143;
      {
        lbits zgsz3520;
        CREATE(lbits)(&zgsz3520);
        CONVERT_OF(lbits, fbits)(&zgsz3520, zv__17, UINT64_C(32) , true);
        sail_int zgsz3521;
        CREATE(sail_int)(&zgsz3521);
        CONVERT_OF(sail_int, mach_int)(&zgsz3521, INT64_C(14));
        sail_int zgsz3522;
        CREATE(sail_int)(&zgsz3522);
        CONVERT_OF(sail_int, mach_int)(&zgsz3522, INT64_C(12));
        lbits zgsz3523;
        CREATE(lbits)(&zgsz3523);
        vector_subrange_lbits(&zgsz3523, zgsz3520, zgsz3521, zgsz3522);
        zgaz3143 = CONVERT_OF(fbits, lbits)(zgsz3523, true);
        KILL(lbits)(&zgsz3523);
        KILL(sail_int)(&zgsz3522);
        KILL(sail_int)(&zgsz3521);
        KILL(lbits)(&zgsz3520);
      }
      {
        lbits zgsz3518;
        CREATE(lbits)(&zgsz3518);
        CONVERT_OF(lbits, fbits)(&zgsz3518, zgaz3143, UINT64_C(3) , true);
        lbits zgsz3519;
        CREATE(lbits)(&zgsz3519);
        CONVERT_OF(lbits, fbits)(&zgsz3519, UINT64_C(0b000), UINT64_C(3) , true);
        zgaz3145 = eq_bits(zgsz3518, zgsz3519);
        KILL(lbits)(&zgsz3519);
        KILL(lbits)(&zgsz3518);
      }
    }
    bool zgsz3192;
    if (zgaz3145) {
      uint64_t zgaz3144;
      {
        lbits zgsz3526;
        CREATE(lbits)(&zgsz3526);
        CONVERT_OF(lbits, fbits)(&zgsz3526, zv__17, UINT64_C(32) , true);
        sail_int zgsz3527;
        CREATE(sail_int)(&zgsz3527);
        CONVERT_OF(sail_int, mach_int)(&zgsz3527, INT64_C(6));
        sail_int zgsz3528;
        CREATE(sail_int)(&zgsz3528);
        CONVERT_OF(sail_int, mach_int)(&zgsz3528, INT64_C(0));
        lbits zgsz3529;
        CREATE(lbits)(&zgsz3529);
        vector_subrange_lbits(&zgsz3529, zgsz3526, zgsz3527, zgsz3528);
        zgaz3144 = CONVERT_OF(fbits, lbits)(zgsz3529, true);
        KILL(lbits)(&zgsz3529);
        KILL(sail_int)(&zgsz3528);
        KILL(sail_int)(&zgsz3527);
        KILL(lbits)(&zgsz3526);
      }
      {
        lbits zgsz3524;
        CREATE(lbits)(&zgsz3524);
        CONVERT_OF(lbits, fbits)(&zgsz3524, zgaz3144, UINT64_C(7) , true);
        lbits zgsz3525;
        CREATE(lbits)(&zgsz3525);
        CONVERT_OF(lbits, fbits)(&zgsz3525, UINT64_C(0b1100111), UINT64_C(7) , true);
        zgsz3192 = eq_bits(zgsz3524, zgsz3525);
        KILL(lbits)(&zgsz3525);
        KILL(lbits)(&zgsz3524);
      }
    } else {  zgsz3192 = false;  }
    bool zgsz3194;
    zgsz3194 = zgsz3192;
    if (!(zgsz3194)) {

      goto case_206;
    }
    uint64_t zimm;
    {
      lbits zgsz3542;
      CREATE(lbits)(&zgsz3542);
      CONVERT_OF(lbits, fbits)(&zgsz3542, zv__17, UINT64_C(32) , true);
      sail_int zgsz3543;
      CREATE(sail_int)(&zgsz3543);
      CONVERT_OF(sail_int, mach_int)(&zgsz3543, INT64_C(31));
      sail_int zgsz3544;
      CREATE(sail_int)(&zgsz3544);
      CONVERT_OF(sail_int, mach_int)(&zgsz3544, INT64_C(20));
      lbits zgsz3545;
      CREATE(lbits)(&zgsz3545);
      vector_subrange_lbits(&zgsz3545, zgsz3542, zgsz3543, zgsz3544);
      zimm = CONVERT_OF(fbits, lbits)(zgsz3545, true);
      KILL(lbits)(&zgsz3545);
      KILL(sail_int)(&zgsz3544);
      KILL(sail_int)(&zgsz3543);
      KILL(lbits)(&zgsz3542);
    }
    uint64_t zimmshadowz36;
    {
      lbits zgsz3538;
      CREATE(lbits)(&zgsz3538);
      CONVERT_OF(lbits, fbits)(&zgsz3538, zv__17, UINT64_C(32) , true);
      sail_int zgsz3539;
      CREATE(sail_int)(&zgsz3539);
      CONVERT_OF(sail_int, mach_int)(&zgsz3539, INT64_C(31));
      sail_int zgsz3540;
      CREATE(sail_int)(&zgsz3540);
      CONVERT_OF(sail_int, mach_int)(&zgsz3540, INT64_C(20));
      lbits zgsz3541;
      CREATE(lbits)(&zgsz3541);
      vector_subrange_lbits(&zgsz3541, zgsz3538, zgsz3539, zgsz3540);
      zimmshadowz36 = CONVERT_OF(fbits, lbits)(zgsz3541, true);
      KILL(lbits)(&zgsz3541);
      KILL(sail_int)(&zgsz3540);
      KILL(sail_int)(&zgsz3539);
      KILL(lbits)(&zgsz3538);
    }
    uint64_t zuz387;
    {
      lbits zgsz3534;
      CREATE(lbits)(&zgsz3534);
      CONVERT_OF(lbits, fbits)(&zgsz3534, zv__17, UINT64_C(32) , true);
      sail_int zgsz3535;
      CREATE(sail_int)(&zgsz3535);
      CONVERT_OF(sail_int, mach_int)(&zgsz3535, INT64_C(19));
      sail_int zgsz3536;
      CREATE(sail_int)(&zgsz3536);
      CONVERT_OF(sail_int, mach_int)(&zgsz3536, INT64_C(15));
      lbits zgsz3537;
      CREATE(lbits)(&zgsz3537);
      vector_subrange_lbits(&zgsz3537, zgsz3534, zgsz3535, zgsz3536);
      zuz387 = CONVERT_OF(fbits, lbits)(zgsz3537, true);
      KILL(lbits)(&zgsz3537);
      KILL(sail_int)(&zgsz3536);
      KILL(sail_int)(&zgsz3535);
      KILL(lbits)(&zgsz3534);
    }
    uint64_t zuz388;
    {
      lbits zgsz3530;
      CREATE(lbits)(&zgsz3530);
      CONVERT_OF(lbits, fbits)(&zgsz3530, zv__17, UINT64_C(32) , true);
      sail_int zgsz3531;
      CREATE(sail_int)(&zgsz3531);
      CONVERT_OF(sail_int, mach_int)(&zgsz3531, INT64_C(11));
      sail_int zgsz3532;
      CREATE(sail_int)(&zgsz3532);
      CONVERT_OF(sail_int, mach_int)(&zgsz3532, INT64_C(7));
      lbits zgsz3533;
      CREATE(lbits)(&zgsz3533);
      vector_subrange_lbits(&zgsz3533, zgsz3530, zgsz3531, zgsz3532);
      zuz388 = CONVERT_OF(fbits, lbits)(zgsz3533, true);
      KILL(lbits)(&zgsz3533);
      KILL(sail_int)(&zgsz3532);
      KILL(sail_int)(&zgsz3531);
      KILL(lbits)(&zgsz3530);
    }
    struct zast zgaz3142;
    CREATE(zast)(&zgaz3142);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3141;
      {
        uint64_t zgaz3139;
        zgaz3139 = ztREG(zuz388);
        uint64_t zgaz3140;
        zgaz3140 = ztREG(zuz387);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3193;
        zgsz3193.ztup0 = zgaz3139;
        zgsz3193.ztup1 = zgaz3140;
        zgsz3193.ztup2 = zimmshadowz36;
        zgaz3141 = zgsz3193;
      }
      zCJALR(&zgaz3142, zgaz3141);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3142);
    KILL(zast)(&zgaz3142);
    goto finish_match_202;
  }
case_206: ;
  {
    uint64_t zv__20;
    zv__20 = zmergez3var;
    uint64_t zgaz3153;
    {
      lbits zgsz3588;
      CREATE(lbits)(&zgsz3588);
      CONVERT_OF(lbits, fbits)(&zgsz3588, zv__20, UINT64_C(32) , true);
      sail_int zgsz3589;
      CREATE(sail_int)(&zgsz3589);
      CONVERT_OF(sail_int, mach_int)(&zgsz3589, INT64_C(6));
      sail_int zgsz3590;
      CREATE(sail_int)(&zgsz3590);
      CONVERT_OF(sail_int, mach_int)(&zgsz3590, INT64_C(0));
      lbits zgsz3591;
      CREATE(lbits)(&zgsz3591);
      vector_subrange_lbits(&zgsz3591, zgsz3588, zgsz3589, zgsz3590);
      zgaz3153 = CONVERT_OF(fbits, lbits)(zgsz3591, true);
      KILL(lbits)(&zgsz3591);
      KILL(sail_int)(&zgsz3590);
      KILL(sail_int)(&zgsz3589);
      KILL(lbits)(&zgsz3588);
    }
    bool zgsz3196;
    {
      lbits zgsz3586;
      CREATE(lbits)(&zgsz3586);
      CONVERT_OF(lbits, fbits)(&zgsz3586, zgaz3153, UINT64_C(7) , true);
      lbits zgsz3587;
      CREATE(lbits)(&zgsz3587);
      CONVERT_OF(lbits, fbits)(&zgsz3587, UINT64_C(0b1101111), UINT64_C(7) , true);
      zgsz3196 = eq_bits(zgsz3586, zgsz3587);
      KILL(lbits)(&zgsz3587);
      KILL(lbits)(&zgsz3586);
    }
    if (!(zgsz3196)) {

      goto case_207;
    }
    uint64_t zimm_19;
    {
      lbits zgsz3582;
      CREATE(lbits)(&zgsz3582);
      CONVERT_OF(lbits, fbits)(&zgsz3582, zv__20, UINT64_C(32) , true);
      sail_int zgsz3583;
      CREATE(sail_int)(&zgsz3583);
      CONVERT_OF(sail_int, mach_int)(&zgsz3583, INT64_C(31));
      sail_int zgsz3584;
      CREATE(sail_int)(&zgsz3584);
      CONVERT_OF(sail_int, mach_int)(&zgsz3584, INT64_C(31));
      lbits zgsz3585;
      CREATE(lbits)(&zgsz3585);
      vector_subrange_lbits(&zgsz3585, zgsz3582, zgsz3583, zgsz3584);
      zimm_19 = CONVERT_OF(fbits, lbits)(zgsz3585, true);
      KILL(lbits)(&zgsz3585);
      KILL(sail_int)(&zgsz3584);
      KILL(sail_int)(&zgsz3583);
      KILL(lbits)(&zgsz3582);
    }
    uint64_t zimm_8;
    {
      lbits zgsz3578;
      CREATE(lbits)(&zgsz3578);
      CONVERT_OF(lbits, fbits)(&zgsz3578, zv__20, UINT64_C(32) , true);
      sail_int zgsz3579;
      CREATE(sail_int)(&zgsz3579);
      CONVERT_OF(sail_int, mach_int)(&zgsz3579, INT64_C(20));
      sail_int zgsz3580;
      CREATE(sail_int)(&zgsz3580);
      CONVERT_OF(sail_int, mach_int)(&zgsz3580, INT64_C(20));
      lbits zgsz3581;
      CREATE(lbits)(&zgsz3581);
      vector_subrange_lbits(&zgsz3581, zgsz3578, zgsz3579, zgsz3580);
      zimm_8 = CONVERT_OF(fbits, lbits)(zgsz3581, true);
      KILL(lbits)(&zgsz3581);
      KILL(sail_int)(&zgsz3580);
      KILL(sail_int)(&zgsz3579);
      KILL(lbits)(&zgsz3578);
    }
    uint64_t zimm_7_0;
    {
      lbits zgsz3574;
      CREATE(lbits)(&zgsz3574);
      CONVERT_OF(lbits, fbits)(&zgsz3574, zv__20, UINT64_C(32) , true);
      sail_int zgsz3575;
      CREATE(sail_int)(&zgsz3575);
      CONVERT_OF(sail_int, mach_int)(&zgsz3575, INT64_C(19));
      sail_int zgsz3576;
      CREATE(sail_int)(&zgsz3576);
      CONVERT_OF(sail_int, mach_int)(&zgsz3576, INT64_C(12));
      lbits zgsz3577;
      CREATE(lbits)(&zgsz3577);
      vector_subrange_lbits(&zgsz3577, zgsz3574, zgsz3575, zgsz3576);
      zimm_7_0 = CONVERT_OF(fbits, lbits)(zgsz3577, true);
      KILL(lbits)(&zgsz3577);
      KILL(sail_int)(&zgsz3576);
      KILL(sail_int)(&zgsz3575);
      KILL(lbits)(&zgsz3574);
    }
    uint64_t zimm_19shadowz37;
    {
      lbits zgsz3570;
      CREATE(lbits)(&zgsz3570);
      CONVERT_OF(lbits, fbits)(&zgsz3570, zv__20, UINT64_C(32) , true);
      sail_int zgsz3571;
      CREATE(sail_int)(&zgsz3571);
      CONVERT_OF(sail_int, mach_int)(&zgsz3571, INT64_C(31));
      sail_int zgsz3572;
      CREATE(sail_int)(&zgsz3572);
      CONVERT_OF(sail_int, mach_int)(&zgsz3572, INT64_C(31));
      lbits zgsz3573;
      CREATE(lbits)(&zgsz3573);
      vector_subrange_lbits(&zgsz3573, zgsz3570, zgsz3571, zgsz3572);
      zimm_19shadowz37 = CONVERT_OF(fbits, lbits)(zgsz3573, true);
      KILL(lbits)(&zgsz3573);
      KILL(sail_int)(&zgsz3572);
      KILL(sail_int)(&zgsz3571);
      KILL(lbits)(&zgsz3570);
    }
    uint64_t zimm_18_13;
    {
      lbits zgsz3566;
      CREATE(lbits)(&zgsz3566);
      CONVERT_OF(lbits, fbits)(&zgsz3566, zv__20, UINT64_C(32) , true);
      sail_int zgsz3567;
      CREATE(sail_int)(&zgsz3567);
      CONVERT_OF(sail_int, mach_int)(&zgsz3567, INT64_C(30));
      sail_int zgsz3568;
      CREATE(sail_int)(&zgsz3568);
      CONVERT_OF(sail_int, mach_int)(&zgsz3568, INT64_C(25));
      lbits zgsz3569;
      CREATE(lbits)(&zgsz3569);
      vector_subrange_lbits(&zgsz3569, zgsz3566, zgsz3567, zgsz3568);
      zimm_18_13 = CONVERT_OF(fbits, lbits)(zgsz3569, true);
      KILL(lbits)(&zgsz3569);
      KILL(sail_int)(&zgsz3568);
      KILL(sail_int)(&zgsz3567);
      KILL(lbits)(&zgsz3566);
    }
    uint64_t zimm_12_9;
    {
      lbits zgsz3562;
      CREATE(lbits)(&zgsz3562);
      CONVERT_OF(lbits, fbits)(&zgsz3562, zv__20, UINT64_C(32) , true);
      sail_int zgsz3563;
      CREATE(sail_int)(&zgsz3563);
      CONVERT_OF(sail_int, mach_int)(&zgsz3563, INT64_C(24));
      sail_int zgsz3564;
      CREATE(sail_int)(&zgsz3564);
      CONVERT_OF(sail_int, mach_int)(&zgsz3564, INT64_C(21));
      lbits zgsz3565;
      CREATE(lbits)(&zgsz3565);
      vector_subrange_lbits(&zgsz3565, zgsz3562, zgsz3563, zgsz3564);
      zimm_12_9 = CONVERT_OF(fbits, lbits)(zgsz3565, true);
      KILL(lbits)(&zgsz3565);
      KILL(sail_int)(&zgsz3564);
      KILL(sail_int)(&zgsz3563);
      KILL(lbits)(&zgsz3562);
    }
    uint64_t zuz389;
    {
      lbits zgsz3558;
      CREATE(lbits)(&zgsz3558);
      CONVERT_OF(lbits, fbits)(&zgsz3558, zv__20, UINT64_C(32) , true);
      sail_int zgsz3559;
      CREATE(sail_int)(&zgsz3559);
      CONVERT_OF(sail_int, mach_int)(&zgsz3559, INT64_C(11));
      sail_int zgsz3560;
      CREATE(sail_int)(&zgsz3560);
      CONVERT_OF(sail_int, mach_int)(&zgsz3560, INT64_C(7));
      lbits zgsz3561;
      CREATE(lbits)(&zgsz3561);
      vector_subrange_lbits(&zgsz3561, zgsz3558, zgsz3559, zgsz3560);
      zuz389 = CONVERT_OF(fbits, lbits)(zgsz3561, true);
      KILL(lbits)(&zgsz3561);
      KILL(sail_int)(&zgsz3560);
      KILL(sail_int)(&zgsz3559);
      KILL(lbits)(&zgsz3558);
    }
    struct zast zgaz3152;
    CREATE(zast)(&zgaz3152);
    {
      struct ztuple_z8z5bv2zCz0z5bv20z9 zgaz3151;
      {
        uint64_t zgaz3149;
        zgaz3149 = ztREG(zuz389);
        uint64_t zgaz3150;
        {
          uint64_t zgaz3148;
          {
            uint64_t zgaz3147;
            {
              uint64_t zgaz3146;
              {
                lbits zgsz3549;
                CREATE(lbits)(&zgsz3549);
                CONVERT_OF(lbits, fbits)(&zgsz3549, zimm_18_13, UINT64_C(6) , true);
                lbits zgsz3550;
                CREATE(lbits)(&zgsz3550);
                CONVERT_OF(lbits, fbits)(&zgsz3550, zimm_12_9, UINT64_C(4) , true);
                lbits zgsz3551;
                CREATE(lbits)(&zgsz3551);
                append(&zgsz3551, zgsz3549, zgsz3550);
                zgaz3146 = CONVERT_OF(fbits, lbits)(zgsz3551, true);
                KILL(lbits)(&zgsz3551);
                KILL(lbits)(&zgsz3550);
                KILL(lbits)(&zgsz3549);
              }
              {
                lbits zgsz3546;
                CREATE(lbits)(&zgsz3546);
                CONVERT_OF(lbits, fbits)(&zgsz3546, zimm_8, UINT64_C(1) , true);
                lbits zgsz3547;
                CREATE(lbits)(&zgsz3547);
                CONVERT_OF(lbits, fbits)(&zgsz3547, zgaz3146, UINT64_C(10) , true);
                lbits zgsz3548;
                CREATE(lbits)(&zgsz3548);
                append(&zgsz3548, zgsz3546, zgsz3547);
                zgaz3147 = CONVERT_OF(fbits, lbits)(zgsz3548, true);
                KILL(lbits)(&zgsz3548);
                KILL(lbits)(&zgsz3547);
                KILL(lbits)(&zgsz3546);
              }
            }
            {
              lbits zgsz3552;
              CREATE(lbits)(&zgsz3552);
              CONVERT_OF(lbits, fbits)(&zgsz3552, zimm_7_0, UINT64_C(8) , true);
              lbits zgsz3553;
              CREATE(lbits)(&zgsz3553);
              CONVERT_OF(lbits, fbits)(&zgsz3553, zgaz3147, UINT64_C(11) , true);
              lbits zgsz3554;
              CREATE(lbits)(&zgsz3554);
              append(&zgsz3554, zgsz3552, zgsz3553);
              zgaz3148 = CONVERT_OF(fbits, lbits)(zgsz3554, true);
              KILL(lbits)(&zgsz3554);
              KILL(lbits)(&zgsz3553);
              KILL(lbits)(&zgsz3552);
            }
          }
          {
            lbits zgsz3555;
            CREATE(lbits)(&zgsz3555);
            CONVERT_OF(lbits, fbits)(&zgsz3555, zimm_19shadowz37, UINT64_C(1) , true);
            lbits zgsz3556;
            CREATE(lbits)(&zgsz3556);
            CONVERT_OF(lbits, fbits)(&zgsz3556, zgaz3148, UINT64_C(19) , true);
            lbits zgsz3557;
            CREATE(lbits)(&zgsz3557);
            append(&zgsz3557, zgsz3555, zgsz3556);
            zgaz3150 = CONVERT_OF(fbits, lbits)(zgsz3557, true);
            KILL(lbits)(&zgsz3557);
            KILL(lbits)(&zgsz3556);
            KILL(lbits)(&zgsz3555);
          }
        }
        struct ztuple_z8z5bv2zCz0z5bv20z9 zgsz3195;
        zgsz3195.ztup0 = zgaz3149;
        zgsz3195.ztup1 = zgaz3150;
        zgaz3151 = zgsz3195;
      }
      zCJAL(&zgaz3152, zgaz3151);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3152);
    KILL(zast)(&zgaz3152);
    goto finish_match_202;
  }
case_207: ;
  {
    uint64_t zv__22;
    zv__22 = zmergez3var;
    bool zgaz3163;
    {
      uint64_t zgaz3161;
      {
        lbits zgsz3594;
        CREATE(lbits)(&zgsz3594);
        CONVERT_OF(lbits, fbits)(&zgsz3594, zv__22, UINT64_C(32) , true);
        sail_int zgsz3595;
        CREATE(sail_int)(&zgsz3595);
        CONVERT_OF(sail_int, mach_int)(&zgsz3595, INT64_C(14));
        sail_int zgsz3596;
        CREATE(sail_int)(&zgsz3596);
        CONVERT_OF(sail_int, mach_int)(&zgsz3596, INT64_C(12));
        lbits zgsz3597;
        CREATE(lbits)(&zgsz3597);
        vector_subrange_lbits(&zgsz3597, zgsz3594, zgsz3595, zgsz3596);
        zgaz3161 = CONVERT_OF(fbits, lbits)(zgsz3597, true);
        KILL(lbits)(&zgsz3597);
        KILL(sail_int)(&zgsz3596);
        KILL(sail_int)(&zgsz3595);
        KILL(lbits)(&zgsz3594);
      }
      {
        lbits zgsz3592;
        CREATE(lbits)(&zgsz3592);
        CONVERT_OF(lbits, fbits)(&zgsz3592, zgaz3161, UINT64_C(3) , true);
        lbits zgsz3593;
        CREATE(lbits)(&zgsz3593);
        CONVERT_OF(lbits, fbits)(&zgsz3593, UINT64_C(0b001), UINT64_C(3) , true);
        zgaz3163 = eq_bits(zgsz3592, zgsz3593);
        KILL(lbits)(&zgsz3593);
        KILL(lbits)(&zgsz3592);
      }
    }
    bool zgsz3197;
    if (zgaz3163) {
      uint64_t zgaz3162;
      {
        lbits zgsz3600;
        CREATE(lbits)(&zgsz3600);
        CONVERT_OF(lbits, fbits)(&zgsz3600, zv__22, UINT64_C(32) , true);
        sail_int zgsz3601;
        CREATE(sail_int)(&zgsz3601);
        CONVERT_OF(sail_int, mach_int)(&zgsz3601, INT64_C(6));
        sail_int zgsz3602;
        CREATE(sail_int)(&zgsz3602);
        CONVERT_OF(sail_int, mach_int)(&zgsz3602, INT64_C(0));
        lbits zgsz3603;
        CREATE(lbits)(&zgsz3603);
        vector_subrange_lbits(&zgsz3603, zgsz3600, zgsz3601, zgsz3602);
        zgaz3162 = CONVERT_OF(fbits, lbits)(zgsz3603, true);
        KILL(lbits)(&zgsz3603);
        KILL(sail_int)(&zgsz3602);
        KILL(sail_int)(&zgsz3601);
        KILL(lbits)(&zgsz3600);
      }
      {
        lbits zgsz3598;
        CREATE(lbits)(&zgsz3598);
        CONVERT_OF(lbits, fbits)(&zgsz3598, zgaz3162, UINT64_C(7) , true);
        lbits zgsz3599;
        CREATE(lbits)(&zgsz3599);
        CONVERT_OF(lbits, fbits)(&zgsz3599, UINT64_C(0b1100011), UINT64_C(7) , true);
        zgsz3197 = eq_bits(zgsz3598, zgsz3599);
        KILL(lbits)(&zgsz3599);
        KILL(lbits)(&zgsz3598);
      }
    } else {  zgsz3197 = false;  }
    bool zgsz3199;
    zgsz3199 = zgsz3197;
    if (!(zgsz3199)) {

      goto case_208;
    }
    uint64_t zimm7_6;
    {
      lbits zgsz3637;
      CREATE(lbits)(&zgsz3637);
      CONVERT_OF(lbits, fbits)(&zgsz3637, zv__22, UINT64_C(32) , true);
      sail_int zgsz3638;
      CREATE(sail_int)(&zgsz3638);
      CONVERT_OF(sail_int, mach_int)(&zgsz3638, INT64_C(31));
      sail_int zgsz3639;
      CREATE(sail_int)(&zgsz3639);
      CONVERT_OF(sail_int, mach_int)(&zgsz3639, INT64_C(31));
      lbits zgsz3640;
      CREATE(lbits)(&zgsz3640);
      vector_subrange_lbits(&zgsz3640, zgsz3637, zgsz3638, zgsz3639);
      zimm7_6 = CONVERT_OF(fbits, lbits)(zgsz3640, true);
      KILL(lbits)(&zgsz3640);
      KILL(sail_int)(&zgsz3639);
      KILL(sail_int)(&zgsz3638);
      KILL(lbits)(&zgsz3637);
    }
    uint64_t zrs2;
    {
      lbits zgsz3633;
      CREATE(lbits)(&zgsz3633);
      CONVERT_OF(lbits, fbits)(&zgsz3633, zv__22, UINT64_C(32) , true);
      sail_int zgsz3634;
      CREATE(sail_int)(&zgsz3634);
      CONVERT_OF(sail_int, mach_int)(&zgsz3634, INT64_C(24));
      sail_int zgsz3635;
      CREATE(sail_int)(&zgsz3635);
      CONVERT_OF(sail_int, mach_int)(&zgsz3635, INT64_C(20));
      lbits zgsz3636;
      CREATE(lbits)(&zgsz3636);
      vector_subrange_lbits(&zgsz3636, zgsz3633, zgsz3634, zgsz3635);
      zrs2 = CONVERT_OF(fbits, lbits)(zgsz3636, true);
      KILL(lbits)(&zgsz3636);
      KILL(sail_int)(&zgsz3635);
      KILL(sail_int)(&zgsz3634);
      KILL(lbits)(&zgsz3633);
    }
    uint64_t zrs1;
    {
      lbits zgsz3629;
      CREATE(lbits)(&zgsz3629);
      CONVERT_OF(lbits, fbits)(&zgsz3629, zv__22, UINT64_C(32) , true);
      sail_int zgsz3630;
      CREATE(sail_int)(&zgsz3630);
      CONVERT_OF(sail_int, mach_int)(&zgsz3630, INT64_C(19));
      sail_int zgsz3631;
      CREATE(sail_int)(&zgsz3631);
      CONVERT_OF(sail_int, mach_int)(&zgsz3631, INT64_C(15));
      lbits zgsz3632;
      CREATE(lbits)(&zgsz3632);
      vector_subrange_lbits(&zgsz3632, zgsz3629, zgsz3630, zgsz3631);
      zrs1 = CONVERT_OF(fbits, lbits)(zgsz3632, true);
      KILL(lbits)(&zgsz3632);
      KILL(sail_int)(&zgsz3631);
      KILL(sail_int)(&zgsz3630);
      KILL(lbits)(&zgsz3629);
    }
    uint64_t zimm7_6shadowz38;
    {
      lbits zgsz3625;
      CREATE(lbits)(&zgsz3625);
      CONVERT_OF(lbits, fbits)(&zgsz3625, zv__22, UINT64_C(32) , true);
      sail_int zgsz3626;
      CREATE(sail_int)(&zgsz3626);
      CONVERT_OF(sail_int, mach_int)(&zgsz3626, INT64_C(31));
      sail_int zgsz3627;
      CREATE(sail_int)(&zgsz3627);
      CONVERT_OF(sail_int, mach_int)(&zgsz3627, INT64_C(31));
      lbits zgsz3628;
      CREATE(lbits)(&zgsz3628);
      vector_subrange_lbits(&zgsz3628, zgsz3625, zgsz3626, zgsz3627);
      zimm7_6shadowz38 = CONVERT_OF(fbits, lbits)(zgsz3628, true);
      KILL(lbits)(&zgsz3628);
      KILL(sail_int)(&zgsz3627);
      KILL(sail_int)(&zgsz3626);
      KILL(lbits)(&zgsz3625);
    }
    uint64_t zimm7_5_0;
    {
      lbits zgsz3621;
      CREATE(lbits)(&zgsz3621);
      CONVERT_OF(lbits, fbits)(&zgsz3621, zv__22, UINT64_C(32) , true);
      sail_int zgsz3622;
      CREATE(sail_int)(&zgsz3622);
      CONVERT_OF(sail_int, mach_int)(&zgsz3622, INT64_C(30));
      sail_int zgsz3623;
      CREATE(sail_int)(&zgsz3623);
      CONVERT_OF(sail_int, mach_int)(&zgsz3623, INT64_C(25));
      lbits zgsz3624;
      CREATE(lbits)(&zgsz3624);
      vector_subrange_lbits(&zgsz3624, zgsz3621, zgsz3622, zgsz3623);
      zimm7_5_0 = CONVERT_OF(fbits, lbits)(zgsz3624, true);
      KILL(lbits)(&zgsz3624);
      KILL(sail_int)(&zgsz3623);
      KILL(sail_int)(&zgsz3622);
      KILL(lbits)(&zgsz3621);
    }
    uint64_t zimm5_4_1;
    {
      lbits zgsz3617;
      CREATE(lbits)(&zgsz3617);
      CONVERT_OF(lbits, fbits)(&zgsz3617, zv__22, UINT64_C(32) , true);
      sail_int zgsz3618;
      CREATE(sail_int)(&zgsz3618);
      CONVERT_OF(sail_int, mach_int)(&zgsz3618, INT64_C(11));
      sail_int zgsz3619;
      CREATE(sail_int)(&zgsz3619);
      CONVERT_OF(sail_int, mach_int)(&zgsz3619, INT64_C(8));
      lbits zgsz3620;
      CREATE(lbits)(&zgsz3620);
      vector_subrange_lbits(&zgsz3620, zgsz3617, zgsz3618, zgsz3619);
      zimm5_4_1 = CONVERT_OF(fbits, lbits)(zgsz3620, true);
      KILL(lbits)(&zgsz3620);
      KILL(sail_int)(&zgsz3619);
      KILL(sail_int)(&zgsz3618);
      KILL(lbits)(&zgsz3617);
    }
    uint64_t zimm5_0;
    {
      lbits zgsz3613;
      CREATE(lbits)(&zgsz3613);
      CONVERT_OF(lbits, fbits)(&zgsz3613, zv__22, UINT64_C(32) , true);
      sail_int zgsz3614;
      CREATE(sail_int)(&zgsz3614);
      CONVERT_OF(sail_int, mach_int)(&zgsz3614, INT64_C(7));
      sail_int zgsz3615;
      CREATE(sail_int)(&zgsz3615);
      CONVERT_OF(sail_int, mach_int)(&zgsz3615, INT64_C(7));
      lbits zgsz3616;
      CREATE(lbits)(&zgsz3616);
      vector_subrange_lbits(&zgsz3616, zgsz3613, zgsz3614, zgsz3615);
      zimm5_0 = CONVERT_OF(fbits, lbits)(zgsz3616, true);
      KILL(lbits)(&zgsz3616);
      KILL(sail_int)(&zgsz3615);
      KILL(sail_int)(&zgsz3614);
      KILL(lbits)(&zgsz3613);
    }
    struct zast zgaz3160;
    CREATE(zast)(&zgaz3160);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3159;
      {
        uint64_t zgaz3156;
        zgaz3156 = ztREG(zrs1);
        uint64_t zgaz3157;
        zgaz3157 = ztREG(zrs2);
        uint64_t zgaz3158;
        {
          uint64_t zgaz3155;
          {
            uint64_t zgaz3154;
            {
              lbits zgsz3607;
              CREATE(lbits)(&zgsz3607);
              CONVERT_OF(lbits, fbits)(&zgsz3607, zimm7_5_0, UINT64_C(6) , true);
              lbits zgsz3608;
              CREATE(lbits)(&zgsz3608);
              CONVERT_OF(lbits, fbits)(&zgsz3608, zimm5_4_1, UINT64_C(4) , true);
              lbits zgsz3609;
              CREATE(lbits)(&zgsz3609);
              append(&zgsz3609, zgsz3607, zgsz3608);
              zgaz3154 = CONVERT_OF(fbits, lbits)(zgsz3609, true);
              KILL(lbits)(&zgsz3609);
              KILL(lbits)(&zgsz3608);
              KILL(lbits)(&zgsz3607);
            }
            {
              lbits zgsz3604;
              CREATE(lbits)(&zgsz3604);
              CONVERT_OF(lbits, fbits)(&zgsz3604, zimm5_0, UINT64_C(1) , true);
              lbits zgsz3605;
              CREATE(lbits)(&zgsz3605);
              CONVERT_OF(lbits, fbits)(&zgsz3605, zgaz3154, UINT64_C(10) , true);
              lbits zgsz3606;
              CREATE(lbits)(&zgsz3606);
              append(&zgsz3606, zgsz3604, zgsz3605);
              zgaz3155 = CONVERT_OF(fbits, lbits)(zgsz3606, true);
              KILL(lbits)(&zgsz3606);
              KILL(lbits)(&zgsz3605);
              KILL(lbits)(&zgsz3604);
            }
          }
          {
            lbits zgsz3610;
            CREATE(lbits)(&zgsz3610);
            CONVERT_OF(lbits, fbits)(&zgsz3610, zimm7_6shadowz38, UINT64_C(1) , true);
            lbits zgsz3611;
            CREATE(lbits)(&zgsz3611);
            CONVERT_OF(lbits, fbits)(&zgsz3611, zgaz3155, UINT64_C(11) , true);
            lbits zgsz3612;
            CREATE(lbits)(&zgsz3612);
            append(&zgsz3612, zgsz3610, zgsz3611);
            zgaz3158 = CONVERT_OF(fbits, lbits)(zgsz3612, true);
            KILL(lbits)(&zgsz3612);
            KILL(lbits)(&zgsz3611);
            KILL(lbits)(&zgsz3610);
          }
        }
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3198;
        zgsz3198.ztup0 = zgaz3156;
        zgsz3198.ztup1 = zgaz3157;
        zgsz3198.ztup2 = zgaz3158;
        zgaz3159 = zgsz3198;
      }
      zBNE(&zgaz3160, zgaz3159);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3160);
    KILL(zast)(&zgaz3160);
    goto finish_match_202;
  }
case_208: ;
  {
    uint64_t zv__25;
    zv__25 = zmergez3var;
    bool zgaz3172;
    {
      uint64_t zgaz3168;
      {
        lbits zgsz3643;
        CREATE(lbits)(&zgsz3643);
        CONVERT_OF(lbits, fbits)(&zgsz3643, zv__25, UINT64_C(32) , true);
        sail_int zgsz3644;
        CREATE(sail_int)(&zgsz3644);
        CONVERT_OF(sail_int, mach_int)(&zgsz3644, INT64_C(31));
        sail_int zgsz3645;
        CREATE(sail_int)(&zgsz3645);
        CONVERT_OF(sail_int, mach_int)(&zgsz3645, INT64_C(20));
        lbits zgsz3646;
        CREATE(lbits)(&zgsz3646);
        vector_subrange_lbits(&zgsz3646, zgsz3643, zgsz3644, zgsz3645);
        zgaz3168 = CONVERT_OF(fbits, lbits)(zgsz3646, true);
        KILL(lbits)(&zgsz3646);
        KILL(sail_int)(&zgsz3645);
        KILL(sail_int)(&zgsz3644);
        KILL(lbits)(&zgsz3643);
      }
      {
        lbits zgsz3641;
        CREATE(lbits)(&zgsz3641);
        CONVERT_OF(lbits, fbits)(&zgsz3641, zgaz3168, UINT64_C(12) , true);
        lbits zgsz3642;
        CREATE(lbits)(&zgsz3642);
        CONVERT_OF(lbits, fbits)(&zgsz3642, UINT64_C(0xFEA), UINT64_C(12) , true);
        zgaz3172 = eq_bits(zgsz3641, zgsz3642);
        KILL(lbits)(&zgsz3642);
        KILL(lbits)(&zgsz3641);
      }
    }
    bool zgsz3201;
    if (zgaz3172) {
      bool zgaz3171;
      {
        uint64_t zgaz3169;
        {
          lbits zgsz3649;
          CREATE(lbits)(&zgsz3649);
          CONVERT_OF(lbits, fbits)(&zgsz3649, zv__25, UINT64_C(32) , true);
          sail_int zgsz3650;
          CREATE(sail_int)(&zgsz3650);
          CONVERT_OF(sail_int, mach_int)(&zgsz3650, INT64_C(14));
          sail_int zgsz3651;
          CREATE(sail_int)(&zgsz3651);
          CONVERT_OF(sail_int, mach_int)(&zgsz3651, INT64_C(12));
          lbits zgsz3652;
          CREATE(lbits)(&zgsz3652);
          vector_subrange_lbits(&zgsz3652, zgsz3649, zgsz3650, zgsz3651);
          zgaz3169 = CONVERT_OF(fbits, lbits)(zgsz3652, true);
          KILL(lbits)(&zgsz3652);
          KILL(sail_int)(&zgsz3651);
          KILL(sail_int)(&zgsz3650);
          KILL(lbits)(&zgsz3649);
        }
        {
          lbits zgsz3647;
          CREATE(lbits)(&zgsz3647);
          CONVERT_OF(lbits, fbits)(&zgsz3647, zgaz3169, UINT64_C(3) , true);
          lbits zgsz3648;
          CREATE(lbits)(&zgsz3648);
          CONVERT_OF(lbits, fbits)(&zgsz3648, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3171 = eq_bits(zgsz3647, zgsz3648);
          KILL(lbits)(&zgsz3648);
          KILL(lbits)(&zgsz3647);
        }
      }
      bool zgsz3200;
      if (zgaz3171) {
        uint64_t zgaz3170;
        {
          lbits zgsz3655;
          CREATE(lbits)(&zgsz3655);
          CONVERT_OF(lbits, fbits)(&zgsz3655, zv__25, UINT64_C(32) , true);
          sail_int zgsz3656;
          CREATE(sail_int)(&zgsz3656);
          CONVERT_OF(sail_int, mach_int)(&zgsz3656, INT64_C(6));
          sail_int zgsz3657;
          CREATE(sail_int)(&zgsz3657);
          CONVERT_OF(sail_int, mach_int)(&zgsz3657, INT64_C(0));
          lbits zgsz3658;
          CREATE(lbits)(&zgsz3658);
          vector_subrange_lbits(&zgsz3658, zgsz3655, zgsz3656, zgsz3657);
          zgaz3170 = CONVERT_OF(fbits, lbits)(zgsz3658, true);
          KILL(lbits)(&zgsz3658);
          KILL(sail_int)(&zgsz3657);
          KILL(sail_int)(&zgsz3656);
          KILL(lbits)(&zgsz3655);
        }
        {
          lbits zgsz3653;
          CREATE(lbits)(&zgsz3653);
          CONVERT_OF(lbits, fbits)(&zgsz3653, zgaz3170, UINT64_C(7) , true);
          lbits zgsz3654;
          CREATE(lbits)(&zgsz3654);
          CONVERT_OF(lbits, fbits)(&zgsz3654, UINT64_C(0b1011011), UINT64_C(7) , true);
          zgsz3200 = eq_bits(zgsz3653, zgsz3654);
          KILL(lbits)(&zgsz3654);
          KILL(lbits)(&zgsz3653);
        }
      } else {  zgsz3200 = false;  }
      zgsz3201 = zgsz3200;
    } else {  zgsz3201 = false;  }
    bool zgsz3203;
    zgsz3203 = zgsz3201;
    if (!(zgsz3203)) {

      goto case_209;
    }
    uint64_t zuz390;
    {
      lbits zgsz3663;
      CREATE(lbits)(&zgsz3663);
      CONVERT_OF(lbits, fbits)(&zgsz3663, zv__25, UINT64_C(32) , true);
      sail_int zgsz3664;
      CREATE(sail_int)(&zgsz3664);
      CONVERT_OF(sail_int, mach_int)(&zgsz3664, INT64_C(19));
      sail_int zgsz3665;
      CREATE(sail_int)(&zgsz3665);
      CONVERT_OF(sail_int, mach_int)(&zgsz3665, INT64_C(15));
      lbits zgsz3666;
      CREATE(lbits)(&zgsz3666);
      vector_subrange_lbits(&zgsz3666, zgsz3663, zgsz3664, zgsz3665);
      zuz390 = CONVERT_OF(fbits, lbits)(zgsz3666, true);
      KILL(lbits)(&zgsz3666);
      KILL(sail_int)(&zgsz3665);
      KILL(sail_int)(&zgsz3664);
      KILL(lbits)(&zgsz3663);
    }
    uint64_t zuz391;
    {
      lbits zgsz3659;
      CREATE(lbits)(&zgsz3659);
      CONVERT_OF(lbits, fbits)(&zgsz3659, zv__25, UINT64_C(32) , true);
      sail_int zgsz3660;
      CREATE(sail_int)(&zgsz3660);
      CONVERT_OF(sail_int, mach_int)(&zgsz3660, INT64_C(11));
      sail_int zgsz3661;
      CREATE(sail_int)(&zgsz3661);
      CONVERT_OF(sail_int, mach_int)(&zgsz3661, INT64_C(7));
      lbits zgsz3662;
      CREATE(lbits)(&zgsz3662);
      vector_subrange_lbits(&zgsz3662, zgsz3659, zgsz3660, zgsz3661);
      zuz391 = CONVERT_OF(fbits, lbits)(zgsz3662, true);
      KILL(lbits)(&zgsz3662);
      KILL(sail_int)(&zgsz3661);
      KILL(sail_int)(&zgsz3660);
      KILL(lbits)(&zgsz3659);
    }
    struct zast zgaz3167;
    CREATE(zast)(&zgaz3167);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3166;
      {
        uint64_t zgaz3164;
        zgaz3164 = ztREG(zuz391);
        uint64_t zgaz3165;
        zgaz3165 = ztREG(zuz390);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3202;
        zgsz3202.ztup0 = zgaz3164;
        zgsz3202.ztup1 = zgaz3165;
        zgaz3166 = zgsz3202;
      }
      zCMOVE(&zgaz3167, zgaz3166);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3167);
    KILL(zast)(&zgaz3167);
    goto finish_match_202;
  }
case_209: ;
  {
    uint64_t zv__30;
    zv__30 = zmergez3var;
    bool zgaz3179;
    {
      uint64_t zgaz3177;
      {
        lbits zgsz3669;
        CREATE(lbits)(&zgsz3669);
        CONVERT_OF(lbits, fbits)(&zgsz3669, zv__30, UINT64_C(32) , true);
        sail_int zgsz3670;
        CREATE(sail_int)(&zgsz3670);
        CONVERT_OF(sail_int, mach_int)(&zgsz3670, INT64_C(14));
        sail_int zgsz3671;
        CREATE(sail_int)(&zgsz3671);
        CONVERT_OF(sail_int, mach_int)(&zgsz3671, INT64_C(12));
        lbits zgsz3672;
        CREATE(lbits)(&zgsz3672);
        vector_subrange_lbits(&zgsz3672, zgsz3669, zgsz3670, zgsz3671);
        zgaz3177 = CONVERT_OF(fbits, lbits)(zgsz3672, true);
        KILL(lbits)(&zgsz3672);
        KILL(sail_int)(&zgsz3671);
        KILL(sail_int)(&zgsz3670);
        KILL(lbits)(&zgsz3669);
      }
      {
        lbits zgsz3667;
        CREATE(lbits)(&zgsz3667);
        CONVERT_OF(lbits, fbits)(&zgsz3667, zgaz3177, UINT64_C(3) , true);
        lbits zgsz3668;
        CREATE(lbits)(&zgsz3668);
        CONVERT_OF(lbits, fbits)(&zgsz3668, UINT64_C(0b011), UINT64_C(3) , true);
        zgaz3179 = eq_bits(zgsz3667, zgsz3668);
        KILL(lbits)(&zgsz3668);
        KILL(lbits)(&zgsz3667);
      }
    }
    bool zgsz3204;
    if (zgaz3179) {
      uint64_t zgaz3178;
      {
        lbits zgsz3675;
        CREATE(lbits)(&zgsz3675);
        CONVERT_OF(lbits, fbits)(&zgsz3675, zv__30, UINT64_C(32) , true);
        sail_int zgsz3676;
        CREATE(sail_int)(&zgsz3676);
        CONVERT_OF(sail_int, mach_int)(&zgsz3676, INT64_C(6));
        sail_int zgsz3677;
        CREATE(sail_int)(&zgsz3677);
        CONVERT_OF(sail_int, mach_int)(&zgsz3677, INT64_C(0));
        lbits zgsz3678;
        CREATE(lbits)(&zgsz3678);
        vector_subrange_lbits(&zgsz3678, zgsz3675, zgsz3676, zgsz3677);
        zgaz3178 = CONVERT_OF(fbits, lbits)(zgsz3678, true);
        KILL(lbits)(&zgsz3678);
        KILL(sail_int)(&zgsz3677);
        KILL(sail_int)(&zgsz3676);
        KILL(lbits)(&zgsz3675);
      }
      {
        lbits zgsz3673;
        CREATE(lbits)(&zgsz3673);
        CONVERT_OF(lbits, fbits)(&zgsz3673, zgaz3178, UINT64_C(7) , true);
        lbits zgsz3674;
        CREATE(lbits)(&zgsz3674);
        CONVERT_OF(lbits, fbits)(&zgsz3674, UINT64_C(0b0000011), UINT64_C(7) , true);
        zgsz3204 = eq_bits(zgsz3673, zgsz3674);
        KILL(lbits)(&zgsz3674);
        KILL(lbits)(&zgsz3673);
      }
    } else {  zgsz3204 = false;  }
    bool zgsz3206;
    zgsz3206 = zgsz3204;
    if (!(zgsz3206)) {

      goto case_210;
    }
    uint64_t zuz392;
    {
      lbits zgsz3691;
      CREATE(lbits)(&zgsz3691);
      CONVERT_OF(lbits, fbits)(&zgsz3691, zv__30, UINT64_C(32) , true);
      sail_int zgsz3692;
      CREATE(sail_int)(&zgsz3692);
      CONVERT_OF(sail_int, mach_int)(&zgsz3692, INT64_C(31));
      sail_int zgsz3693;
      CREATE(sail_int)(&zgsz3693);
      CONVERT_OF(sail_int, mach_int)(&zgsz3693, INT64_C(20));
      lbits zgsz3694;
      CREATE(lbits)(&zgsz3694);
      vector_subrange_lbits(&zgsz3694, zgsz3691, zgsz3692, zgsz3693);
      zuz392 = CONVERT_OF(fbits, lbits)(zgsz3694, true);
      KILL(lbits)(&zgsz3694);
      KILL(sail_int)(&zgsz3693);
      KILL(sail_int)(&zgsz3692);
      KILL(lbits)(&zgsz3691);
    }
    uint64_t zrs;
    {
      lbits zgsz3687;
      CREATE(lbits)(&zgsz3687);
      CONVERT_OF(lbits, fbits)(&zgsz3687, zv__30, UINT64_C(32) , true);
      sail_int zgsz3688;
      CREATE(sail_int)(&zgsz3688);
      CONVERT_OF(sail_int, mach_int)(&zgsz3688, INT64_C(19));
      sail_int zgsz3689;
      CREATE(sail_int)(&zgsz3689);
      CONVERT_OF(sail_int, mach_int)(&zgsz3689, INT64_C(15));
      lbits zgsz3690;
      CREATE(lbits)(&zgsz3690);
      vector_subrange_lbits(&zgsz3690, zgsz3687, zgsz3688, zgsz3689);
      zrs = CONVERT_OF(fbits, lbits)(zgsz3690, true);
      KILL(lbits)(&zgsz3690);
      KILL(sail_int)(&zgsz3689);
      KILL(sail_int)(&zgsz3688);
      KILL(lbits)(&zgsz3687);
    }
    uint64_t zrd;
    {
      lbits zgsz3683;
      CREATE(lbits)(&zgsz3683);
      CONVERT_OF(lbits, fbits)(&zgsz3683, zv__30, UINT64_C(32) , true);
      sail_int zgsz3684;
      CREATE(sail_int)(&zgsz3684);
      CONVERT_OF(sail_int, mach_int)(&zgsz3684, INT64_C(11));
      sail_int zgsz3685;
      CREATE(sail_int)(&zgsz3685);
      CONVERT_OF(sail_int, mach_int)(&zgsz3685, INT64_C(7));
      lbits zgsz3686;
      CREATE(lbits)(&zgsz3686);
      vector_subrange_lbits(&zgsz3686, zgsz3683, zgsz3684, zgsz3685);
      zrd = CONVERT_OF(fbits, lbits)(zgsz3686, true);
      KILL(lbits)(&zgsz3686);
      KILL(sail_int)(&zgsz3685);
      KILL(sail_int)(&zgsz3684);
      KILL(lbits)(&zgsz3683);
    }
    uint64_t zimmshadowz39;
    {
      lbits zgsz3679;
      CREATE(lbits)(&zgsz3679);
      CONVERT_OF(lbits, fbits)(&zgsz3679, zv__30, UINT64_C(32) , true);
      sail_int zgsz3680;
      CREATE(sail_int)(&zgsz3680);
      CONVERT_OF(sail_int, mach_int)(&zgsz3680, INT64_C(31));
      sail_int zgsz3681;
      CREATE(sail_int)(&zgsz3681);
      CONVERT_OF(sail_int, mach_int)(&zgsz3681, INT64_C(20));
      lbits zgsz3682;
      CREATE(lbits)(&zgsz3682);
      vector_subrange_lbits(&zgsz3682, zgsz3679, zgsz3680, zgsz3681);
      zimmshadowz39 = CONVERT_OF(fbits, lbits)(zgsz3682, true);
      KILL(lbits)(&zgsz3682);
      KILL(sail_int)(&zgsz3681);
      KILL(sail_int)(&zgsz3680);
      KILL(lbits)(&zgsz3679);
    }
    struct zast zgaz3176;
    CREATE(zast)(&zgaz3176);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3175;
      {
        uint64_t zgaz3173;
        zgaz3173 = ztREG(zrd);
        uint64_t zgaz3174;
        zgaz3174 = ztREG(zrs);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3205;
        zgsz3205.ztup0 = zgaz3173;
        zgsz3205.ztup1 = zgaz3174;
        zgsz3205.ztup2 = zimmshadowz39;
        zgaz3175 = zgsz3205;
      }
      zLD(&zgaz3176, zgaz3175);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3176);
    KILL(zast)(&zgaz3176);
    goto finish_match_202;
  }
case_210: ;
  {
    uint64_t zv__33;
    zv__33 = zmergez3var;
    bool zgaz3187;
    {
      uint64_t zgaz3185;
      {
        lbits zgsz3697;
        CREATE(lbits)(&zgsz3697);
        CONVERT_OF(lbits, fbits)(&zgsz3697, zv__33, UINT64_C(32) , true);
        sail_int zgsz3698;
        CREATE(sail_int)(&zgsz3698);
        CONVERT_OF(sail_int, mach_int)(&zgsz3698, INT64_C(14));
        sail_int zgsz3699;
        CREATE(sail_int)(&zgsz3699);
        CONVERT_OF(sail_int, mach_int)(&zgsz3699, INT64_C(12));
        lbits zgsz3700;
        CREATE(lbits)(&zgsz3700);
        vector_subrange_lbits(&zgsz3700, zgsz3697, zgsz3698, zgsz3699);
        zgaz3185 = CONVERT_OF(fbits, lbits)(zgsz3700, true);
        KILL(lbits)(&zgsz3700);
        KILL(sail_int)(&zgsz3699);
        KILL(sail_int)(&zgsz3698);
        KILL(lbits)(&zgsz3697);
      }
      {
        lbits zgsz3695;
        CREATE(lbits)(&zgsz3695);
        CONVERT_OF(lbits, fbits)(&zgsz3695, zgaz3185, UINT64_C(3) , true);
        lbits zgsz3696;
        CREATE(lbits)(&zgsz3696);
        CONVERT_OF(lbits, fbits)(&zgsz3696, UINT64_C(0b011), UINT64_C(3) , true);
        zgaz3187 = eq_bits(zgsz3695, zgsz3696);
        KILL(lbits)(&zgsz3696);
        KILL(lbits)(&zgsz3695);
      }
    }
    bool zgsz3207;
    if (zgaz3187) {
      uint64_t zgaz3186;
      {
        lbits zgsz3703;
        CREATE(lbits)(&zgsz3703);
        CONVERT_OF(lbits, fbits)(&zgsz3703, zv__33, UINT64_C(32) , true);
        sail_int zgsz3704;
        CREATE(sail_int)(&zgsz3704);
        CONVERT_OF(sail_int, mach_int)(&zgsz3704, INT64_C(6));
        sail_int zgsz3705;
        CREATE(sail_int)(&zgsz3705);
        CONVERT_OF(sail_int, mach_int)(&zgsz3705, INT64_C(0));
        lbits zgsz3706;
        CREATE(lbits)(&zgsz3706);
        vector_subrange_lbits(&zgsz3706, zgsz3703, zgsz3704, zgsz3705);
        zgaz3186 = CONVERT_OF(fbits, lbits)(zgsz3706, true);
        KILL(lbits)(&zgsz3706);
        KILL(sail_int)(&zgsz3705);
        KILL(sail_int)(&zgsz3704);
        KILL(lbits)(&zgsz3703);
      }
      {
        lbits zgsz3701;
        CREATE(lbits)(&zgsz3701);
        CONVERT_OF(lbits, fbits)(&zgsz3701, zgaz3186, UINT64_C(7) , true);
        lbits zgsz3702;
        CREATE(lbits)(&zgsz3702);
        CONVERT_OF(lbits, fbits)(&zgsz3702, UINT64_C(0b0100011), UINT64_C(7) , true);
        zgsz3207 = eq_bits(zgsz3701, zgsz3702);
        KILL(lbits)(&zgsz3702);
        KILL(lbits)(&zgsz3701);
      }
    } else {  zgsz3207 = false;  }
    bool zgsz3209;
    zgsz3209 = zgsz3207;
    if (!(zgsz3209)) {

      goto case_211;
    }
    uint64_t zimm1;
    {
      lbits zgsz3726;
      CREATE(lbits)(&zgsz3726);
      CONVERT_OF(lbits, fbits)(&zgsz3726, zv__33, UINT64_C(32) , true);
      sail_int zgsz3727;
      CREATE(sail_int)(&zgsz3727);
      CONVERT_OF(sail_int, mach_int)(&zgsz3727, INT64_C(31));
      sail_int zgsz3728;
      CREATE(sail_int)(&zgsz3728);
      CONVERT_OF(sail_int, mach_int)(&zgsz3728, INT64_C(25));
      lbits zgsz3729;
      CREATE(lbits)(&zgsz3729);
      vector_subrange_lbits(&zgsz3729, zgsz3726, zgsz3727, zgsz3728);
      zimm1 = CONVERT_OF(fbits, lbits)(zgsz3729, true);
      KILL(lbits)(&zgsz3729);
      KILL(sail_int)(&zgsz3728);
      KILL(sail_int)(&zgsz3727);
      KILL(lbits)(&zgsz3726);
    }
    uint64_t zuz393;
    {
      lbits zgsz3722;
      CREATE(lbits)(&zgsz3722);
      CONVERT_OF(lbits, fbits)(&zgsz3722, zv__33, UINT64_C(32) , true);
      sail_int zgsz3723;
      CREATE(sail_int)(&zgsz3723);
      CONVERT_OF(sail_int, mach_int)(&zgsz3723, INT64_C(24));
      sail_int zgsz3724;
      CREATE(sail_int)(&zgsz3724);
      CONVERT_OF(sail_int, mach_int)(&zgsz3724, INT64_C(20));
      lbits zgsz3725;
      CREATE(lbits)(&zgsz3725);
      vector_subrange_lbits(&zgsz3725, zgsz3722, zgsz3723, zgsz3724);
      zuz393 = CONVERT_OF(fbits, lbits)(zgsz3725, true);
      KILL(lbits)(&zgsz3725);
      KILL(sail_int)(&zgsz3724);
      KILL(sail_int)(&zgsz3723);
      KILL(lbits)(&zgsz3722);
    }
    uint64_t zrb;
    {
      lbits zgsz3718;
      CREATE(lbits)(&zgsz3718);
      CONVERT_OF(lbits, fbits)(&zgsz3718, zv__33, UINT64_C(32) , true);
      sail_int zgsz3719;
      CREATE(sail_int)(&zgsz3719);
      CONVERT_OF(sail_int, mach_int)(&zgsz3719, INT64_C(19));
      sail_int zgsz3720;
      CREATE(sail_int)(&zgsz3720);
      CONVERT_OF(sail_int, mach_int)(&zgsz3720, INT64_C(15));
      lbits zgsz3721;
      CREATE(lbits)(&zgsz3721);
      vector_subrange_lbits(&zgsz3721, zgsz3718, zgsz3719, zgsz3720);
      zrb = CONVERT_OF(fbits, lbits)(zgsz3721, true);
      KILL(lbits)(&zgsz3721);
      KILL(sail_int)(&zgsz3720);
      KILL(sail_int)(&zgsz3719);
      KILL(lbits)(&zgsz3718);
    }
    uint64_t zimm2;
    {
      lbits zgsz3714;
      CREATE(lbits)(&zgsz3714);
      CONVERT_OF(lbits, fbits)(&zgsz3714, zv__33, UINT64_C(32) , true);
      sail_int zgsz3715;
      CREATE(sail_int)(&zgsz3715);
      CONVERT_OF(sail_int, mach_int)(&zgsz3715, INT64_C(11));
      sail_int zgsz3716;
      CREATE(sail_int)(&zgsz3716);
      CONVERT_OF(sail_int, mach_int)(&zgsz3716, INT64_C(7));
      lbits zgsz3717;
      CREATE(lbits)(&zgsz3717);
      vector_subrange_lbits(&zgsz3717, zgsz3714, zgsz3715, zgsz3716);
      zimm2 = CONVERT_OF(fbits, lbits)(zgsz3717, true);
      KILL(lbits)(&zgsz3717);
      KILL(sail_int)(&zgsz3716);
      KILL(sail_int)(&zgsz3715);
      KILL(lbits)(&zgsz3714);
    }
    uint64_t zimm1shadowz310;
    {
      lbits zgsz3710;
      CREATE(lbits)(&zgsz3710);
      CONVERT_OF(lbits, fbits)(&zgsz3710, zv__33, UINT64_C(32) , true);
      sail_int zgsz3711;
      CREATE(sail_int)(&zgsz3711);
      CONVERT_OF(sail_int, mach_int)(&zgsz3711, INT64_C(31));
      sail_int zgsz3712;
      CREATE(sail_int)(&zgsz3712);
      CONVERT_OF(sail_int, mach_int)(&zgsz3712, INT64_C(25));
      lbits zgsz3713;
      CREATE(lbits)(&zgsz3713);
      vector_subrange_lbits(&zgsz3713, zgsz3710, zgsz3711, zgsz3712);
      zimm1shadowz310 = CONVERT_OF(fbits, lbits)(zgsz3713, true);
      KILL(lbits)(&zgsz3713);
      KILL(sail_int)(&zgsz3712);
      KILL(sail_int)(&zgsz3711);
      KILL(lbits)(&zgsz3710);
    }
    struct zast zgaz3184;
    CREATE(zast)(&zgaz3184);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3183;
      {
        uint64_t zgaz3180;
        zgaz3180 = ztREG(zuz393);
        uint64_t zgaz3181;
        zgaz3181 = ztREG(zrb);
        uint64_t zgaz3182;
        {
          lbits zgsz3707;
          CREATE(lbits)(&zgsz3707);
          CONVERT_OF(lbits, fbits)(&zgsz3707, zimm1shadowz310, UINT64_C(7) , true);
          lbits zgsz3708;
          CREATE(lbits)(&zgsz3708);
          CONVERT_OF(lbits, fbits)(&zgsz3708, zimm2, UINT64_C(5) , true);
          lbits zgsz3709;
          CREATE(lbits)(&zgsz3709);
          append(&zgsz3709, zgsz3707, zgsz3708);
          zgaz3182 = CONVERT_OF(fbits, lbits)(zgsz3709, true);
          KILL(lbits)(&zgsz3709);
          KILL(lbits)(&zgsz3708);
          KILL(lbits)(&zgsz3707);
        }
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3208;
        zgsz3208.ztup0 = zgaz3180;
        zgsz3208.ztup1 = zgaz3181;
        zgsz3208.ztup2 = zgaz3182;
        zgaz3183 = zgsz3208;
      }
      zSD(&zgaz3184, zgaz3183);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3184);
    KILL(zast)(&zgaz3184);
    goto finish_match_202;
  }
case_211: ;
  {
    uint64_t zv__36;
    zv__36 = zmergez3var;
    bool zgaz3194;
    {
      uint64_t zgaz3192;
      {
        lbits zgsz3732;
        CREATE(lbits)(&zgsz3732);
        CONVERT_OF(lbits, fbits)(&zgsz3732, zv__36, UINT64_C(32) , true);
        sail_int zgsz3733;
        CREATE(sail_int)(&zgsz3733);
        CONVERT_OF(sail_int, mach_int)(&zgsz3733, INT64_C(14));
        sail_int zgsz3734;
        CREATE(sail_int)(&zgsz3734);
        CONVERT_OF(sail_int, mach_int)(&zgsz3734, INT64_C(12));
        lbits zgsz3735;
        CREATE(lbits)(&zgsz3735);
        vector_subrange_lbits(&zgsz3735, zgsz3732, zgsz3733, zgsz3734);
        zgaz3192 = CONVERT_OF(fbits, lbits)(zgsz3735, true);
        KILL(lbits)(&zgsz3735);
        KILL(sail_int)(&zgsz3734);
        KILL(sail_int)(&zgsz3733);
        KILL(lbits)(&zgsz3732);
      }
      {
        lbits zgsz3730;
        CREATE(lbits)(&zgsz3730);
        CONVERT_OF(lbits, fbits)(&zgsz3730, zgaz3192, UINT64_C(3) , true);
        lbits zgsz3731;
        CREATE(lbits)(&zgsz3731);
        CONVERT_OF(lbits, fbits)(&zgsz3731, UINT64_C(0b000), UINT64_C(3) , true);
        zgaz3194 = eq_bits(zgsz3730, zgsz3731);
        KILL(lbits)(&zgsz3731);
        KILL(lbits)(&zgsz3730);
      }
    }
    bool zgsz3210;
    if (zgaz3194) {
      uint64_t zgaz3193;
      {
        lbits zgsz3738;
        CREATE(lbits)(&zgsz3738);
        CONVERT_OF(lbits, fbits)(&zgsz3738, zv__36, UINT64_C(32) , true);
        sail_int zgsz3739;
        CREATE(sail_int)(&zgsz3739);
        CONVERT_OF(sail_int, mach_int)(&zgsz3739, INT64_C(6));
        sail_int zgsz3740;
        CREATE(sail_int)(&zgsz3740);
        CONVERT_OF(sail_int, mach_int)(&zgsz3740, INT64_C(0));
        lbits zgsz3741;
        CREATE(lbits)(&zgsz3741);
        vector_subrange_lbits(&zgsz3741, zgsz3738, zgsz3739, zgsz3740);
        zgaz3193 = CONVERT_OF(fbits, lbits)(zgsz3741, true);
        KILL(lbits)(&zgsz3741);
        KILL(sail_int)(&zgsz3740);
        KILL(sail_int)(&zgsz3739);
        KILL(lbits)(&zgsz3738);
      }
      {
        lbits zgsz3736;
        CREATE(lbits)(&zgsz3736);
        CONVERT_OF(lbits, fbits)(&zgsz3736, zgaz3193, UINT64_C(7) , true);
        lbits zgsz3737;
        CREATE(lbits)(&zgsz3737);
        CONVERT_OF(lbits, fbits)(&zgsz3737, UINT64_C(0b0010011), UINT64_C(7) , true);
        zgsz3210 = eq_bits(zgsz3736, zgsz3737);
        KILL(lbits)(&zgsz3737);
        KILL(lbits)(&zgsz3736);
      }
    } else {  zgsz3210 = false;  }
    bool zgsz3212;
    zgsz3212 = zgsz3210;
    if (!(zgsz3212)) {

      goto case_212;
    }
    uint64_t zuz394;
    {
      lbits zgsz3754;
      CREATE(lbits)(&zgsz3754);
      CONVERT_OF(lbits, fbits)(&zgsz3754, zv__36, UINT64_C(32) , true);
      sail_int zgsz3755;
      CREATE(sail_int)(&zgsz3755);
      CONVERT_OF(sail_int, mach_int)(&zgsz3755, INT64_C(31));
      sail_int zgsz3756;
      CREATE(sail_int)(&zgsz3756);
      CONVERT_OF(sail_int, mach_int)(&zgsz3756, INT64_C(20));
      lbits zgsz3757;
      CREATE(lbits)(&zgsz3757);
      vector_subrange_lbits(&zgsz3757, zgsz3754, zgsz3755, zgsz3756);
      zuz394 = CONVERT_OF(fbits, lbits)(zgsz3757, true);
      KILL(lbits)(&zgsz3757);
      KILL(sail_int)(&zgsz3756);
      KILL(sail_int)(&zgsz3755);
      KILL(lbits)(&zgsz3754);
    }
    uint64_t zuz395;
    {
      lbits zgsz3750;
      CREATE(lbits)(&zgsz3750);
      CONVERT_OF(lbits, fbits)(&zgsz3750, zv__36, UINT64_C(32) , true);
      sail_int zgsz3751;
      CREATE(sail_int)(&zgsz3751);
      CONVERT_OF(sail_int, mach_int)(&zgsz3751, INT64_C(19));
      sail_int zgsz3752;
      CREATE(sail_int)(&zgsz3752);
      CONVERT_OF(sail_int, mach_int)(&zgsz3752, INT64_C(15));
      lbits zgsz3753;
      CREATE(lbits)(&zgsz3753);
      vector_subrange_lbits(&zgsz3753, zgsz3750, zgsz3751, zgsz3752);
      zuz395 = CONVERT_OF(fbits, lbits)(zgsz3753, true);
      KILL(lbits)(&zgsz3753);
      KILL(sail_int)(&zgsz3752);
      KILL(sail_int)(&zgsz3751);
      KILL(lbits)(&zgsz3750);
    }
    uint64_t zuz396;
    {
      lbits zgsz3746;
      CREATE(lbits)(&zgsz3746);
      CONVERT_OF(lbits, fbits)(&zgsz3746, zv__36, UINT64_C(32) , true);
      sail_int zgsz3747;
      CREATE(sail_int)(&zgsz3747);
      CONVERT_OF(sail_int, mach_int)(&zgsz3747, INT64_C(11));
      sail_int zgsz3748;
      CREATE(sail_int)(&zgsz3748);
      CONVERT_OF(sail_int, mach_int)(&zgsz3748, INT64_C(7));
      lbits zgsz3749;
      CREATE(lbits)(&zgsz3749);
      vector_subrange_lbits(&zgsz3749, zgsz3746, zgsz3747, zgsz3748);
      zuz396 = CONVERT_OF(fbits, lbits)(zgsz3749, true);
      KILL(lbits)(&zgsz3749);
      KILL(sail_int)(&zgsz3748);
      KILL(sail_int)(&zgsz3747);
      KILL(lbits)(&zgsz3746);
    }
    uint64_t zimmshadowz311;
    {
      lbits zgsz3742;
      CREATE(lbits)(&zgsz3742);
      CONVERT_OF(lbits, fbits)(&zgsz3742, zv__36, UINT64_C(32) , true);
      sail_int zgsz3743;
      CREATE(sail_int)(&zgsz3743);
      CONVERT_OF(sail_int, mach_int)(&zgsz3743, INT64_C(31));
      sail_int zgsz3744;
      CREATE(sail_int)(&zgsz3744);
      CONVERT_OF(sail_int, mach_int)(&zgsz3744, INT64_C(20));
      lbits zgsz3745;
      CREATE(lbits)(&zgsz3745);
      vector_subrange_lbits(&zgsz3745, zgsz3742, zgsz3743, zgsz3744);
      zimmshadowz311 = CONVERT_OF(fbits, lbits)(zgsz3745, true);
      KILL(lbits)(&zgsz3745);
      KILL(sail_int)(&zgsz3744);
      KILL(sail_int)(&zgsz3743);
      KILL(lbits)(&zgsz3742);
    }
    struct zast zgaz3191;
    CREATE(zast)(&zgaz3191);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3190;
      {
        uint64_t zgaz3188;
        zgaz3188 = ztREG(zuz396);
        uint64_t zgaz3189;
        zgaz3189 = ztREG(zuz395);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3211;
        zgsz3211.ztup0 = zgaz3188;
        zgsz3211.ztup1 = zgaz3189;
        zgsz3211.ztup2 = zimmshadowz311;
        zgaz3190 = zgsz3211;
      }
      zADDI(&zgaz3191, zgaz3190);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3191);
    KILL(zast)(&zgaz3191);
    goto finish_match_202;
  }
case_212: ;
  {
    uint64_t zv__39;
    zv__39 = zmergez3var;
    bool zgaz3204;
    {
      uint64_t zgaz3200;
      {
        lbits zgsz3760;
        CREATE(lbits)(&zgsz3760);
        CONVERT_OF(lbits, fbits)(&zgsz3760, zv__39, UINT64_C(32) , true);
        sail_int zgsz3761;
        CREATE(sail_int)(&zgsz3761);
        CONVERT_OF(sail_int, mach_int)(&zgsz3761, INT64_C(31));
        sail_int zgsz3762;
        CREATE(sail_int)(&zgsz3762);
        CONVERT_OF(sail_int, mach_int)(&zgsz3762, INT64_C(25));
        lbits zgsz3763;
        CREATE(lbits)(&zgsz3763);
        vector_subrange_lbits(&zgsz3763, zgsz3760, zgsz3761, zgsz3762);
        zgaz3200 = CONVERT_OF(fbits, lbits)(zgsz3763, true);
        KILL(lbits)(&zgsz3763);
        KILL(sail_int)(&zgsz3762);
        KILL(sail_int)(&zgsz3761);
        KILL(lbits)(&zgsz3760);
      }
      {
        lbits zgsz3758;
        CREATE(lbits)(&zgsz3758);
        CONVERT_OF(lbits, fbits)(&zgsz3758, zgaz3200, UINT64_C(7) , true);
        lbits zgsz3759;
        CREATE(lbits)(&zgsz3759);
        CONVERT_OF(lbits, fbits)(&zgsz3759, UINT64_C(0b0000000), UINT64_C(7) , true);
        zgaz3204 = eq_bits(zgsz3758, zgsz3759);
        KILL(lbits)(&zgsz3759);
        KILL(lbits)(&zgsz3758);
      }
    }
    bool zgsz3214;
    if (zgaz3204) {
      bool zgaz3203;
      {
        uint64_t zgaz3201;
        {
          lbits zgsz3766;
          CREATE(lbits)(&zgsz3766);
          CONVERT_OF(lbits, fbits)(&zgsz3766, zv__39, UINT64_C(32) , true);
          sail_int zgsz3767;
          CREATE(sail_int)(&zgsz3767);
          CONVERT_OF(sail_int, mach_int)(&zgsz3767, INT64_C(14));
          sail_int zgsz3768;
          CREATE(sail_int)(&zgsz3768);
          CONVERT_OF(sail_int, mach_int)(&zgsz3768, INT64_C(12));
          lbits zgsz3769;
          CREATE(lbits)(&zgsz3769);
          vector_subrange_lbits(&zgsz3769, zgsz3766, zgsz3767, zgsz3768);
          zgaz3201 = CONVERT_OF(fbits, lbits)(zgsz3769, true);
          KILL(lbits)(&zgsz3769);
          KILL(sail_int)(&zgsz3768);
          KILL(sail_int)(&zgsz3767);
          KILL(lbits)(&zgsz3766);
        }
        {
          lbits zgsz3764;
          CREATE(lbits)(&zgsz3764);
          CONVERT_OF(lbits, fbits)(&zgsz3764, zgaz3201, UINT64_C(3) , true);
          lbits zgsz3765;
          CREATE(lbits)(&zgsz3765);
          CONVERT_OF(lbits, fbits)(&zgsz3765, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3203 = eq_bits(zgsz3764, zgsz3765);
          KILL(lbits)(&zgsz3765);
          KILL(lbits)(&zgsz3764);
        }
      }
      bool zgsz3213;
      if (zgaz3203) {
        uint64_t zgaz3202;
        {
          lbits zgsz3772;
          CREATE(lbits)(&zgsz3772);
          CONVERT_OF(lbits, fbits)(&zgsz3772, zv__39, UINT64_C(32) , true);
          sail_int zgsz3773;
          CREATE(sail_int)(&zgsz3773);
          CONVERT_OF(sail_int, mach_int)(&zgsz3773, INT64_C(6));
          sail_int zgsz3774;
          CREATE(sail_int)(&zgsz3774);
          CONVERT_OF(sail_int, mach_int)(&zgsz3774, INT64_C(0));
          lbits zgsz3775;
          CREATE(lbits)(&zgsz3775);
          vector_subrange_lbits(&zgsz3775, zgsz3772, zgsz3773, zgsz3774);
          zgaz3202 = CONVERT_OF(fbits, lbits)(zgsz3775, true);
          KILL(lbits)(&zgsz3775);
          KILL(sail_int)(&zgsz3774);
          KILL(sail_int)(&zgsz3773);
          KILL(lbits)(&zgsz3772);
        }
        {
          lbits zgsz3770;
          CREATE(lbits)(&zgsz3770);
          CONVERT_OF(lbits, fbits)(&zgsz3770, zgaz3202, UINT64_C(7) , true);
          lbits zgsz3771;
          CREATE(lbits)(&zgsz3771);
          CONVERT_OF(lbits, fbits)(&zgsz3771, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3213 = eq_bits(zgsz3770, zgsz3771);
          KILL(lbits)(&zgsz3771);
          KILL(lbits)(&zgsz3770);
        }
      } else {  zgsz3213 = false;  }
      zgsz3214 = zgsz3213;
    } else {  zgsz3214 = false;  }
    bool zgsz3216;
    zgsz3216 = zgsz3214;
    if (!(zgsz3216)) {

      goto case_213;
    }
    uint64_t zuz397;
    {
      lbits zgsz3784;
      CREATE(lbits)(&zgsz3784);
      CONVERT_OF(lbits, fbits)(&zgsz3784, zv__39, UINT64_C(32) , true);
      sail_int zgsz3785;
      CREATE(sail_int)(&zgsz3785);
      CONVERT_OF(sail_int, mach_int)(&zgsz3785, INT64_C(19));
      sail_int zgsz3786;
      CREATE(sail_int)(&zgsz3786);
      CONVERT_OF(sail_int, mach_int)(&zgsz3786, INT64_C(15));
      lbits zgsz3787;
      CREATE(lbits)(&zgsz3787);
      vector_subrange_lbits(&zgsz3787, zgsz3784, zgsz3785, zgsz3786);
      zuz397 = CONVERT_OF(fbits, lbits)(zgsz3787, true);
      KILL(lbits)(&zgsz3787);
      KILL(sail_int)(&zgsz3786);
      KILL(sail_int)(&zgsz3785);
      KILL(lbits)(&zgsz3784);
    }
    uint64_t zuz398;
    {
      lbits zgsz3780;
      CREATE(lbits)(&zgsz3780);
      CONVERT_OF(lbits, fbits)(&zgsz3780, zv__39, UINT64_C(32) , true);
      sail_int zgsz3781;
      CREATE(sail_int)(&zgsz3781);
      CONVERT_OF(sail_int, mach_int)(&zgsz3781, INT64_C(11));
      sail_int zgsz3782;
      CREATE(sail_int)(&zgsz3782);
      CONVERT_OF(sail_int, mach_int)(&zgsz3782, INT64_C(7));
      lbits zgsz3783;
      CREATE(lbits)(&zgsz3783);
      vector_subrange_lbits(&zgsz3783, zgsz3780, zgsz3781, zgsz3782);
      zuz398 = CONVERT_OF(fbits, lbits)(zgsz3783, true);
      KILL(lbits)(&zgsz3783);
      KILL(sail_int)(&zgsz3782);
      KILL(sail_int)(&zgsz3781);
      KILL(lbits)(&zgsz3780);
    }
    uint64_t zuz399;
    {
      lbits zgsz3776;
      CREATE(lbits)(&zgsz3776);
      CONVERT_OF(lbits, fbits)(&zgsz3776, zv__39, UINT64_C(32) , true);
      sail_int zgsz3777;
      CREATE(sail_int)(&zgsz3777);
      CONVERT_OF(sail_int, mach_int)(&zgsz3777, INT64_C(24));
      sail_int zgsz3778;
      CREATE(sail_int)(&zgsz3778);
      CONVERT_OF(sail_int, mach_int)(&zgsz3778, INT64_C(20));
      lbits zgsz3779;
      CREATE(lbits)(&zgsz3779);
      vector_subrange_lbits(&zgsz3779, zgsz3776, zgsz3777, zgsz3778);
      zuz399 = CONVERT_OF(fbits, lbits)(zgsz3779, true);
      KILL(lbits)(&zgsz3779);
      KILL(sail_int)(&zgsz3778);
      KILL(sail_int)(&zgsz3777);
      KILL(lbits)(&zgsz3776);
    }
    struct zast zgaz3199;
    CREATE(zast)(&zgaz3199);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3198;
      {
        uint64_t zgaz3195;
        zgaz3195 = ztREG(zuz398);
        uint64_t zgaz3196;
        zgaz3196 = ztREG(zuz397);
        uint64_t zgaz3197;
        zgaz3197 = ztREG(zuz399);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3215;
        zgsz3215.ztup0 = zgaz3195;
        zgsz3215.ztup1 = zgaz3196;
        zgsz3215.ztup2 = zgaz3197;
        zgaz3198 = zgsz3215;
      }
      zADD(&zgaz3199, zgaz3198);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3199);
    KILL(zast)(&zgaz3199);
    goto finish_match_202;
  }
case_213: ;
  {
    uint64_t zv__43;
    zv__43 = zmergez3var;
    bool zgaz3214;
    {
      uint64_t zgaz3210;
      {
        lbits zgsz3790;
        CREATE(lbits)(&zgsz3790);
        CONVERT_OF(lbits, fbits)(&zgsz3790, zv__43, UINT64_C(32) , true);
        sail_int zgsz3791;
        CREATE(sail_int)(&zgsz3791);
        CONVERT_OF(sail_int, mach_int)(&zgsz3791, INT64_C(31));
        sail_int zgsz3792;
        CREATE(sail_int)(&zgsz3792);
        CONVERT_OF(sail_int, mach_int)(&zgsz3792, INT64_C(25));
        lbits zgsz3793;
        CREATE(lbits)(&zgsz3793);
        vector_subrange_lbits(&zgsz3793, zgsz3790, zgsz3791, zgsz3792);
        zgaz3210 = CONVERT_OF(fbits, lbits)(zgsz3793, true);
        KILL(lbits)(&zgsz3793);
        KILL(sail_int)(&zgsz3792);
        KILL(sail_int)(&zgsz3791);
        KILL(lbits)(&zgsz3790);
      }
      {
        lbits zgsz3788;
        CREATE(lbits)(&zgsz3788);
        CONVERT_OF(lbits, fbits)(&zgsz3788, zgaz3210, UINT64_C(7) , true);
        lbits zgsz3789;
        CREATE(lbits)(&zgsz3789);
        CONVERT_OF(lbits, fbits)(&zgsz3789, UINT64_C(0b0100000), UINT64_C(7) , true);
        zgaz3214 = eq_bits(zgsz3788, zgsz3789);
        KILL(lbits)(&zgsz3789);
        KILL(lbits)(&zgsz3788);
      }
    }
    bool zgsz3218;
    if (zgaz3214) {
      bool zgaz3213;
      {
        uint64_t zgaz3211;
        {
          lbits zgsz3796;
          CREATE(lbits)(&zgsz3796);
          CONVERT_OF(lbits, fbits)(&zgsz3796, zv__43, UINT64_C(32) , true);
          sail_int zgsz3797;
          CREATE(sail_int)(&zgsz3797);
          CONVERT_OF(sail_int, mach_int)(&zgsz3797, INT64_C(14));
          sail_int zgsz3798;
          CREATE(sail_int)(&zgsz3798);
          CONVERT_OF(sail_int, mach_int)(&zgsz3798, INT64_C(12));
          lbits zgsz3799;
          CREATE(lbits)(&zgsz3799);
          vector_subrange_lbits(&zgsz3799, zgsz3796, zgsz3797, zgsz3798);
          zgaz3211 = CONVERT_OF(fbits, lbits)(zgsz3799, true);
          KILL(lbits)(&zgsz3799);
          KILL(sail_int)(&zgsz3798);
          KILL(sail_int)(&zgsz3797);
          KILL(lbits)(&zgsz3796);
        }
        {
          lbits zgsz3794;
          CREATE(lbits)(&zgsz3794);
          CONVERT_OF(lbits, fbits)(&zgsz3794, zgaz3211, UINT64_C(3) , true);
          lbits zgsz3795;
          CREATE(lbits)(&zgsz3795);
          CONVERT_OF(lbits, fbits)(&zgsz3795, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3213 = eq_bits(zgsz3794, zgsz3795);
          KILL(lbits)(&zgsz3795);
          KILL(lbits)(&zgsz3794);
        }
      }
      bool zgsz3217;
      if (zgaz3213) {
        uint64_t zgaz3212;
        {
          lbits zgsz3802;
          CREATE(lbits)(&zgsz3802);
          CONVERT_OF(lbits, fbits)(&zgsz3802, zv__43, UINT64_C(32) , true);
          sail_int zgsz3803;
          CREATE(sail_int)(&zgsz3803);
          CONVERT_OF(sail_int, mach_int)(&zgsz3803, INT64_C(6));
          sail_int zgsz3804;
          CREATE(sail_int)(&zgsz3804);
          CONVERT_OF(sail_int, mach_int)(&zgsz3804, INT64_C(0));
          lbits zgsz3805;
          CREATE(lbits)(&zgsz3805);
          vector_subrange_lbits(&zgsz3805, zgsz3802, zgsz3803, zgsz3804);
          zgaz3212 = CONVERT_OF(fbits, lbits)(zgsz3805, true);
          KILL(lbits)(&zgsz3805);
          KILL(sail_int)(&zgsz3804);
          KILL(sail_int)(&zgsz3803);
          KILL(lbits)(&zgsz3802);
        }
        {
          lbits zgsz3800;
          CREATE(lbits)(&zgsz3800);
          CONVERT_OF(lbits, fbits)(&zgsz3800, zgaz3212, UINT64_C(7) , true);
          lbits zgsz3801;
          CREATE(lbits)(&zgsz3801);
          CONVERT_OF(lbits, fbits)(&zgsz3801, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3217 = eq_bits(zgsz3800, zgsz3801);
          KILL(lbits)(&zgsz3801);
          KILL(lbits)(&zgsz3800);
        }
      } else {  zgsz3217 = false;  }
      zgsz3218 = zgsz3217;
    } else {  zgsz3218 = false;  }
    bool zgsz3220;
    zgsz3220 = zgsz3218;
    if (!(zgsz3220)) {

      goto case_214;
    }
    uint64_t zuz3100;
    {
      lbits zgsz3814;
      CREATE(lbits)(&zgsz3814);
      CONVERT_OF(lbits, fbits)(&zgsz3814, zv__43, UINT64_C(32) , true);
      sail_int zgsz3815;
      CREATE(sail_int)(&zgsz3815);
      CONVERT_OF(sail_int, mach_int)(&zgsz3815, INT64_C(19));
      sail_int zgsz3816;
      CREATE(sail_int)(&zgsz3816);
      CONVERT_OF(sail_int, mach_int)(&zgsz3816, INT64_C(15));
      lbits zgsz3817;
      CREATE(lbits)(&zgsz3817);
      vector_subrange_lbits(&zgsz3817, zgsz3814, zgsz3815, zgsz3816);
      zuz3100 = CONVERT_OF(fbits, lbits)(zgsz3817, true);
      KILL(lbits)(&zgsz3817);
      KILL(sail_int)(&zgsz3816);
      KILL(sail_int)(&zgsz3815);
      KILL(lbits)(&zgsz3814);
    }
    uint64_t zuz3101;
    {
      lbits zgsz3810;
      CREATE(lbits)(&zgsz3810);
      CONVERT_OF(lbits, fbits)(&zgsz3810, zv__43, UINT64_C(32) , true);
      sail_int zgsz3811;
      CREATE(sail_int)(&zgsz3811);
      CONVERT_OF(sail_int, mach_int)(&zgsz3811, INT64_C(11));
      sail_int zgsz3812;
      CREATE(sail_int)(&zgsz3812);
      CONVERT_OF(sail_int, mach_int)(&zgsz3812, INT64_C(7));
      lbits zgsz3813;
      CREATE(lbits)(&zgsz3813);
      vector_subrange_lbits(&zgsz3813, zgsz3810, zgsz3811, zgsz3812);
      zuz3101 = CONVERT_OF(fbits, lbits)(zgsz3813, true);
      KILL(lbits)(&zgsz3813);
      KILL(sail_int)(&zgsz3812);
      KILL(sail_int)(&zgsz3811);
      KILL(lbits)(&zgsz3810);
    }
    uint64_t zuz3102;
    {
      lbits zgsz3806;
      CREATE(lbits)(&zgsz3806);
      CONVERT_OF(lbits, fbits)(&zgsz3806, zv__43, UINT64_C(32) , true);
      sail_int zgsz3807;
      CREATE(sail_int)(&zgsz3807);
      CONVERT_OF(sail_int, mach_int)(&zgsz3807, INT64_C(24));
      sail_int zgsz3808;
      CREATE(sail_int)(&zgsz3808);
      CONVERT_OF(sail_int, mach_int)(&zgsz3808, INT64_C(20));
      lbits zgsz3809;
      CREATE(lbits)(&zgsz3809);
      vector_subrange_lbits(&zgsz3809, zgsz3806, zgsz3807, zgsz3808);
      zuz3102 = CONVERT_OF(fbits, lbits)(zgsz3809, true);
      KILL(lbits)(&zgsz3809);
      KILL(sail_int)(&zgsz3808);
      KILL(sail_int)(&zgsz3807);
      KILL(lbits)(&zgsz3806);
    }
    struct zast zgaz3209;
    CREATE(zast)(&zgaz3209);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3208;
      {
        uint64_t zgaz3205;
        zgaz3205 = ztREG(zuz3101);
        uint64_t zgaz3206;
        zgaz3206 = ztREG(zuz3100);
        uint64_t zgaz3207;
        zgaz3207 = ztREG(zuz3102);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3219;
        zgsz3219.ztup0 = zgaz3205;
        zgsz3219.ztup1 = zgaz3206;
        zgsz3219.ztup2 = zgaz3207;
        zgaz3208 = zgsz3219;
      }
      zSUB(&zgaz3209, zgaz3208);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3209);
    KILL(zast)(&zgaz3209);
    goto finish_match_202;
  }
case_214: ;
  {
    uint64_t zv__47;
    zv__47 = zmergez3var;
    bool zgaz3223;
    {
      uint64_t zgaz3219;
      {
        lbits zgsz3820;
        CREATE(lbits)(&zgsz3820);
        CONVERT_OF(lbits, fbits)(&zgsz3820, zv__47, UINT64_C(32) , true);
        sail_int zgsz3821;
        CREATE(sail_int)(&zgsz3821);
        CONVERT_OF(sail_int, mach_int)(&zgsz3821, INT64_C(31));
        sail_int zgsz3822;
        CREATE(sail_int)(&zgsz3822);
        CONVERT_OF(sail_int, mach_int)(&zgsz3822, INT64_C(20));
        lbits zgsz3823;
        CREATE(lbits)(&zgsz3823);
        vector_subrange_lbits(&zgsz3823, zgsz3820, zgsz3821, zgsz3822);
        zgaz3219 = CONVERT_OF(fbits, lbits)(zgsz3823, true);
        KILL(lbits)(&zgsz3823);
        KILL(sail_int)(&zgsz3822);
        KILL(sail_int)(&zgsz3821);
        KILL(lbits)(&zgsz3820);
      }
      {
        lbits zgsz3818;
        CREATE(lbits)(&zgsz3818);
        CONVERT_OF(lbits, fbits)(&zgsz3818, zgaz3219, UINT64_C(12) , true);
        lbits zgsz3819;
        CREATE(lbits)(&zgsz3819);
        CONVERT_OF(lbits, fbits)(&zgsz3819, UINT64_C(0x400), UINT64_C(12) , true);
        zgaz3223 = eq_bits(zgsz3818, zgsz3819);
        KILL(lbits)(&zgsz3819);
        KILL(lbits)(&zgsz3818);
      }
    }
    bool zgsz3222;
    if (zgaz3223) {
      bool zgaz3222;
      {
        uint64_t zgaz3220;
        {
          lbits zgsz3826;
          CREATE(lbits)(&zgsz3826);
          CONVERT_OF(lbits, fbits)(&zgsz3826, zv__47, UINT64_C(32) , true);
          sail_int zgsz3827;
          CREATE(sail_int)(&zgsz3827);
          CONVERT_OF(sail_int, mach_int)(&zgsz3827, INT64_C(14));
          sail_int zgsz3828;
          CREATE(sail_int)(&zgsz3828);
          CONVERT_OF(sail_int, mach_int)(&zgsz3828, INT64_C(12));
          lbits zgsz3829;
          CREATE(lbits)(&zgsz3829);
          vector_subrange_lbits(&zgsz3829, zgsz3826, zgsz3827, zgsz3828);
          zgaz3220 = CONVERT_OF(fbits, lbits)(zgsz3829, true);
          KILL(lbits)(&zgsz3829);
          KILL(sail_int)(&zgsz3828);
          KILL(sail_int)(&zgsz3827);
          KILL(lbits)(&zgsz3826);
        }
        {
          lbits zgsz3824;
          CREATE(lbits)(&zgsz3824);
          CONVERT_OF(lbits, fbits)(&zgsz3824, zgaz3220, UINT64_C(3) , true);
          lbits zgsz3825;
          CREATE(lbits)(&zgsz3825);
          CONVERT_OF(lbits, fbits)(&zgsz3825, UINT64_C(0b101), UINT64_C(3) , true);
          zgaz3222 = eq_bits(zgsz3824, zgsz3825);
          KILL(lbits)(&zgsz3825);
          KILL(lbits)(&zgsz3824);
        }
      }
      bool zgsz3221;
      if (zgaz3222) {
        uint64_t zgaz3221;
        {
          lbits zgsz3832;
          CREATE(lbits)(&zgsz3832);
          CONVERT_OF(lbits, fbits)(&zgsz3832, zv__47, UINT64_C(32) , true);
          sail_int zgsz3833;
          CREATE(sail_int)(&zgsz3833);
          CONVERT_OF(sail_int, mach_int)(&zgsz3833, INT64_C(6));
          sail_int zgsz3834;
          CREATE(sail_int)(&zgsz3834);
          CONVERT_OF(sail_int, mach_int)(&zgsz3834, INT64_C(0));
          lbits zgsz3835;
          CREATE(lbits)(&zgsz3835);
          vector_subrange_lbits(&zgsz3835, zgsz3832, zgsz3833, zgsz3834);
          zgaz3221 = CONVERT_OF(fbits, lbits)(zgsz3835, true);
          KILL(lbits)(&zgsz3835);
          KILL(sail_int)(&zgsz3834);
          KILL(sail_int)(&zgsz3833);
          KILL(lbits)(&zgsz3832);
        }
        {
          lbits zgsz3830;
          CREATE(lbits)(&zgsz3830);
          CONVERT_OF(lbits, fbits)(&zgsz3830, zgaz3221, UINT64_C(7) , true);
          lbits zgsz3831;
          CREATE(lbits)(&zgsz3831);
          CONVERT_OF(lbits, fbits)(&zgsz3831, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3221 = eq_bits(zgsz3830, zgsz3831);
          KILL(lbits)(&zgsz3831);
          KILL(lbits)(&zgsz3830);
        }
      } else {  zgsz3221 = false;  }
      zgsz3222 = zgsz3221;
    } else {  zgsz3222 = false;  }
    bool zgsz3224;
    zgsz3224 = zgsz3222;
    if (!(zgsz3224)) {

      goto case_215;
    }
    uint64_t zuz3103;
    {
      lbits zgsz3840;
      CREATE(lbits)(&zgsz3840);
      CONVERT_OF(lbits, fbits)(&zgsz3840, zv__47, UINT64_C(32) , true);
      sail_int zgsz3841;
      CREATE(sail_int)(&zgsz3841);
      CONVERT_OF(sail_int, mach_int)(&zgsz3841, INT64_C(19));
      sail_int zgsz3842;
      CREATE(sail_int)(&zgsz3842);
      CONVERT_OF(sail_int, mach_int)(&zgsz3842, INT64_C(15));
      lbits zgsz3843;
      CREATE(lbits)(&zgsz3843);
      vector_subrange_lbits(&zgsz3843, zgsz3840, zgsz3841, zgsz3842);
      zuz3103 = CONVERT_OF(fbits, lbits)(zgsz3843, true);
      KILL(lbits)(&zgsz3843);
      KILL(sail_int)(&zgsz3842);
      KILL(sail_int)(&zgsz3841);
      KILL(lbits)(&zgsz3840);
    }
    uint64_t zuz3104;
    {
      lbits zgsz3836;
      CREATE(lbits)(&zgsz3836);
      CONVERT_OF(lbits, fbits)(&zgsz3836, zv__47, UINT64_C(32) , true);
      sail_int zgsz3837;
      CREATE(sail_int)(&zgsz3837);
      CONVERT_OF(sail_int, mach_int)(&zgsz3837, INT64_C(11));
      sail_int zgsz3838;
      CREATE(sail_int)(&zgsz3838);
      CONVERT_OF(sail_int, mach_int)(&zgsz3838, INT64_C(7));
      lbits zgsz3839;
      CREATE(lbits)(&zgsz3839);
      vector_subrange_lbits(&zgsz3839, zgsz3836, zgsz3837, zgsz3838);
      zuz3104 = CONVERT_OF(fbits, lbits)(zgsz3839, true);
      KILL(lbits)(&zgsz3839);
      KILL(sail_int)(&zgsz3838);
      KILL(sail_int)(&zgsz3837);
      KILL(lbits)(&zgsz3836);
    }
    struct zast zgaz3218;
    CREATE(zast)(&zgaz3218);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3217;
      {
        uint64_t zgaz3215;
        zgaz3215 = ztREG(zuz3104);
        uint64_t zgaz3216;
        zgaz3216 = ztREG(zuz3103);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3223;
        zgsz3223.ztup0 = zgaz3215;
        zgsz3223.ztup1 = zgaz3216;
        zgaz3217 = zgsz3223;
      }
      zCGETPERM(&zgaz3218, zgaz3217);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3218);
    KILL(zast)(&zgaz3218);
    goto finish_match_202;
  }
case_215: ;
  {
    uint64_t zv__52;
    zv__52 = zmergez3var;
    bool zgaz3232;
    {
      uint64_t zgaz3228;
      {
        lbits zgsz3846;
        CREATE(lbits)(&zgsz3846);
        CONVERT_OF(lbits, fbits)(&zgsz3846, zv__52, UINT64_C(32) , true);
        sail_int zgsz3847;
        CREATE(sail_int)(&zgsz3847);
        CONVERT_OF(sail_int, mach_int)(&zgsz3847, INT64_C(31));
        sail_int zgsz3848;
        CREATE(sail_int)(&zgsz3848);
        CONVERT_OF(sail_int, mach_int)(&zgsz3848, INT64_C(20));
        lbits zgsz3849;
        CREATE(lbits)(&zgsz3849);
        vector_subrange_lbits(&zgsz3849, zgsz3846, zgsz3847, zgsz3848);
        zgaz3228 = CONVERT_OF(fbits, lbits)(zgsz3849, true);
        KILL(lbits)(&zgsz3849);
        KILL(sail_int)(&zgsz3848);
        KILL(sail_int)(&zgsz3847);
        KILL(lbits)(&zgsz3846);
      }
      {
        lbits zgsz3844;
        CREATE(lbits)(&zgsz3844);
        CONVERT_OF(lbits, fbits)(&zgsz3844, zgaz3228, UINT64_C(12) , true);
        lbits zgsz3845;
        CREATE(lbits)(&zgsz3845);
        CONVERT_OF(lbits, fbits)(&zgsz3845, UINT64_C(0x401), UINT64_C(12) , true);
        zgaz3232 = eq_bits(zgsz3844, zgsz3845);
        KILL(lbits)(&zgsz3845);
        KILL(lbits)(&zgsz3844);
      }
    }
    bool zgsz3226;
    if (zgaz3232) {
      bool zgaz3231;
      {
        uint64_t zgaz3229;
        {
          lbits zgsz3852;
          CREATE(lbits)(&zgsz3852);
          CONVERT_OF(lbits, fbits)(&zgsz3852, zv__52, UINT64_C(32) , true);
          sail_int zgsz3853;
          CREATE(sail_int)(&zgsz3853);
          CONVERT_OF(sail_int, mach_int)(&zgsz3853, INT64_C(14));
          sail_int zgsz3854;
          CREATE(sail_int)(&zgsz3854);
          CONVERT_OF(sail_int, mach_int)(&zgsz3854, INT64_C(12));
          lbits zgsz3855;
          CREATE(lbits)(&zgsz3855);
          vector_subrange_lbits(&zgsz3855, zgsz3852, zgsz3853, zgsz3854);
          zgaz3229 = CONVERT_OF(fbits, lbits)(zgsz3855, true);
          KILL(lbits)(&zgsz3855);
          KILL(sail_int)(&zgsz3854);
          KILL(sail_int)(&zgsz3853);
          KILL(lbits)(&zgsz3852);
        }
        {
          lbits zgsz3850;
          CREATE(lbits)(&zgsz3850);
          CONVERT_OF(lbits, fbits)(&zgsz3850, zgaz3229, UINT64_C(3) , true);
          lbits zgsz3851;
          CREATE(lbits)(&zgsz3851);
          CONVERT_OF(lbits, fbits)(&zgsz3851, UINT64_C(0b101), UINT64_C(3) , true);
          zgaz3231 = eq_bits(zgsz3850, zgsz3851);
          KILL(lbits)(&zgsz3851);
          KILL(lbits)(&zgsz3850);
        }
      }
      bool zgsz3225;
      if (zgaz3231) {
        uint64_t zgaz3230;
        {
          lbits zgsz3858;
          CREATE(lbits)(&zgsz3858);
          CONVERT_OF(lbits, fbits)(&zgsz3858, zv__52, UINT64_C(32) , true);
          sail_int zgsz3859;
          CREATE(sail_int)(&zgsz3859);
          CONVERT_OF(sail_int, mach_int)(&zgsz3859, INT64_C(6));
          sail_int zgsz3860;
          CREATE(sail_int)(&zgsz3860);
          CONVERT_OF(sail_int, mach_int)(&zgsz3860, INT64_C(0));
          lbits zgsz3861;
          CREATE(lbits)(&zgsz3861);
          vector_subrange_lbits(&zgsz3861, zgsz3858, zgsz3859, zgsz3860);
          zgaz3230 = CONVERT_OF(fbits, lbits)(zgsz3861, true);
          KILL(lbits)(&zgsz3861);
          KILL(sail_int)(&zgsz3860);
          KILL(sail_int)(&zgsz3859);
          KILL(lbits)(&zgsz3858);
        }
        {
          lbits zgsz3856;
          CREATE(lbits)(&zgsz3856);
          CONVERT_OF(lbits, fbits)(&zgsz3856, zgaz3230, UINT64_C(7) , true);
          lbits zgsz3857;
          CREATE(lbits)(&zgsz3857);
          CONVERT_OF(lbits, fbits)(&zgsz3857, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3225 = eq_bits(zgsz3856, zgsz3857);
          KILL(lbits)(&zgsz3857);
          KILL(lbits)(&zgsz3856);
        }
      } else {  zgsz3225 = false;  }
      zgsz3226 = zgsz3225;
    } else {  zgsz3226 = false;  }
    bool zgsz3228;
    zgsz3228 = zgsz3226;
    if (!(zgsz3228)) {

      goto case_216;
    }
    uint64_t zuz3105;
    {
      lbits zgsz3866;
      CREATE(lbits)(&zgsz3866);
      CONVERT_OF(lbits, fbits)(&zgsz3866, zv__52, UINT64_C(32) , true);
      sail_int zgsz3867;
      CREATE(sail_int)(&zgsz3867);
      CONVERT_OF(sail_int, mach_int)(&zgsz3867, INT64_C(19));
      sail_int zgsz3868;
      CREATE(sail_int)(&zgsz3868);
      CONVERT_OF(sail_int, mach_int)(&zgsz3868, INT64_C(15));
      lbits zgsz3869;
      CREATE(lbits)(&zgsz3869);
      vector_subrange_lbits(&zgsz3869, zgsz3866, zgsz3867, zgsz3868);
      zuz3105 = CONVERT_OF(fbits, lbits)(zgsz3869, true);
      KILL(lbits)(&zgsz3869);
      KILL(sail_int)(&zgsz3868);
      KILL(sail_int)(&zgsz3867);
      KILL(lbits)(&zgsz3866);
    }
    uint64_t zuz3106;
    {
      lbits zgsz3862;
      CREATE(lbits)(&zgsz3862);
      CONVERT_OF(lbits, fbits)(&zgsz3862, zv__52, UINT64_C(32) , true);
      sail_int zgsz3863;
      CREATE(sail_int)(&zgsz3863);
      CONVERT_OF(sail_int, mach_int)(&zgsz3863, INT64_C(11));
      sail_int zgsz3864;
      CREATE(sail_int)(&zgsz3864);
      CONVERT_OF(sail_int, mach_int)(&zgsz3864, INT64_C(7));
      lbits zgsz3865;
      CREATE(lbits)(&zgsz3865);
      vector_subrange_lbits(&zgsz3865, zgsz3862, zgsz3863, zgsz3864);
      zuz3106 = CONVERT_OF(fbits, lbits)(zgsz3865, true);
      KILL(lbits)(&zgsz3865);
      KILL(sail_int)(&zgsz3864);
      KILL(sail_int)(&zgsz3863);
      KILL(lbits)(&zgsz3862);
    }
    struct zast zgaz3227;
    CREATE(zast)(&zgaz3227);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3226;
      {
        uint64_t zgaz3224;
        zgaz3224 = ztREG(zuz3106);
        uint64_t zgaz3225;
        zgaz3225 = ztREG(zuz3105);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3227;
        zgsz3227.ztup0 = zgaz3224;
        zgsz3227.ztup1 = zgaz3225;
        zgaz3226 = zgsz3227;
      }
      zCGETBASE(&zgaz3227, zgaz3226);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3227);
    KILL(zast)(&zgaz3227);
    goto finish_match_202;
  }
case_216: ;
  {
    uint64_t zv__57;
    zv__57 = zmergez3var;
    bool zgaz3241;
    {
      uint64_t zgaz3237;
      {
        lbits zgsz3872;
        CREATE(lbits)(&zgsz3872);
        CONVERT_OF(lbits, fbits)(&zgsz3872, zv__57, UINT64_C(32) , true);
        sail_int zgsz3873;
        CREATE(sail_int)(&zgsz3873);
        CONVERT_OF(sail_int, mach_int)(&zgsz3873, INT64_C(31));
        sail_int zgsz3874;
        CREATE(sail_int)(&zgsz3874);
        CONVERT_OF(sail_int, mach_int)(&zgsz3874, INT64_C(20));
        lbits zgsz3875;
        CREATE(lbits)(&zgsz3875);
        vector_subrange_lbits(&zgsz3875, zgsz3872, zgsz3873, zgsz3874);
        zgaz3237 = CONVERT_OF(fbits, lbits)(zgsz3875, true);
        KILL(lbits)(&zgsz3875);
        KILL(sail_int)(&zgsz3874);
        KILL(sail_int)(&zgsz3873);
        KILL(lbits)(&zgsz3872);
      }
      {
        lbits zgsz3870;
        CREATE(lbits)(&zgsz3870);
        CONVERT_OF(lbits, fbits)(&zgsz3870, zgaz3237, UINT64_C(12) , true);
        lbits zgsz3871;
        CREATE(lbits)(&zgsz3871);
        CONVERT_OF(lbits, fbits)(&zgsz3871, UINT64_C(0x402), UINT64_C(12) , true);
        zgaz3241 = eq_bits(zgsz3870, zgsz3871);
        KILL(lbits)(&zgsz3871);
        KILL(lbits)(&zgsz3870);
      }
    }
    bool zgsz3230;
    if (zgaz3241) {
      bool zgaz3240;
      {
        uint64_t zgaz3238;
        {
          lbits zgsz3878;
          CREATE(lbits)(&zgsz3878);
          CONVERT_OF(lbits, fbits)(&zgsz3878, zv__57, UINT64_C(32) , true);
          sail_int zgsz3879;
          CREATE(sail_int)(&zgsz3879);
          CONVERT_OF(sail_int, mach_int)(&zgsz3879, INT64_C(14));
          sail_int zgsz3880;
          CREATE(sail_int)(&zgsz3880);
          CONVERT_OF(sail_int, mach_int)(&zgsz3880, INT64_C(12));
          lbits zgsz3881;
          CREATE(lbits)(&zgsz3881);
          vector_subrange_lbits(&zgsz3881, zgsz3878, zgsz3879, zgsz3880);
          zgaz3238 = CONVERT_OF(fbits, lbits)(zgsz3881, true);
          KILL(lbits)(&zgsz3881);
          KILL(sail_int)(&zgsz3880);
          KILL(sail_int)(&zgsz3879);
          KILL(lbits)(&zgsz3878);
        }
        {
          lbits zgsz3876;
          CREATE(lbits)(&zgsz3876);
          CONVERT_OF(lbits, fbits)(&zgsz3876, zgaz3238, UINT64_C(3) , true);
          lbits zgsz3877;
          CREATE(lbits)(&zgsz3877);
          CONVERT_OF(lbits, fbits)(&zgsz3877, UINT64_C(0b101), UINT64_C(3) , true);
          zgaz3240 = eq_bits(zgsz3876, zgsz3877);
          KILL(lbits)(&zgsz3877);
          KILL(lbits)(&zgsz3876);
        }
      }
      bool zgsz3229;
      if (zgaz3240) {
        uint64_t zgaz3239;
        {
          lbits zgsz3884;
          CREATE(lbits)(&zgsz3884);
          CONVERT_OF(lbits, fbits)(&zgsz3884, zv__57, UINT64_C(32) , true);
          sail_int zgsz3885;
          CREATE(sail_int)(&zgsz3885);
          CONVERT_OF(sail_int, mach_int)(&zgsz3885, INT64_C(6));
          sail_int zgsz3886;
          CREATE(sail_int)(&zgsz3886);
          CONVERT_OF(sail_int, mach_int)(&zgsz3886, INT64_C(0));
          lbits zgsz3887;
          CREATE(lbits)(&zgsz3887);
          vector_subrange_lbits(&zgsz3887, zgsz3884, zgsz3885, zgsz3886);
          zgaz3239 = CONVERT_OF(fbits, lbits)(zgsz3887, true);
          KILL(lbits)(&zgsz3887);
          KILL(sail_int)(&zgsz3886);
          KILL(sail_int)(&zgsz3885);
          KILL(lbits)(&zgsz3884);
        }
        {
          lbits zgsz3882;
          CREATE(lbits)(&zgsz3882);
          CONVERT_OF(lbits, fbits)(&zgsz3882, zgaz3239, UINT64_C(7) , true);
          lbits zgsz3883;
          CREATE(lbits)(&zgsz3883);
          CONVERT_OF(lbits, fbits)(&zgsz3883, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3229 = eq_bits(zgsz3882, zgsz3883);
          KILL(lbits)(&zgsz3883);
          KILL(lbits)(&zgsz3882);
        }
      } else {  zgsz3229 = false;  }
      zgsz3230 = zgsz3229;
    } else {  zgsz3230 = false;  }
    bool zgsz3232;
    zgsz3232 = zgsz3230;
    if (!(zgsz3232)) {

      goto case_217;
    }
    uint64_t zuz3107;
    {
      lbits zgsz3892;
      CREATE(lbits)(&zgsz3892);
      CONVERT_OF(lbits, fbits)(&zgsz3892, zv__57, UINT64_C(32) , true);
      sail_int zgsz3893;
      CREATE(sail_int)(&zgsz3893);
      CONVERT_OF(sail_int, mach_int)(&zgsz3893, INT64_C(19));
      sail_int zgsz3894;
      CREATE(sail_int)(&zgsz3894);
      CONVERT_OF(sail_int, mach_int)(&zgsz3894, INT64_C(15));
      lbits zgsz3895;
      CREATE(lbits)(&zgsz3895);
      vector_subrange_lbits(&zgsz3895, zgsz3892, zgsz3893, zgsz3894);
      zuz3107 = CONVERT_OF(fbits, lbits)(zgsz3895, true);
      KILL(lbits)(&zgsz3895);
      KILL(sail_int)(&zgsz3894);
      KILL(sail_int)(&zgsz3893);
      KILL(lbits)(&zgsz3892);
    }
    uint64_t zuz3108;
    {
      lbits zgsz3888;
      CREATE(lbits)(&zgsz3888);
      CONVERT_OF(lbits, fbits)(&zgsz3888, zv__57, UINT64_C(32) , true);
      sail_int zgsz3889;
      CREATE(sail_int)(&zgsz3889);
      CONVERT_OF(sail_int, mach_int)(&zgsz3889, INT64_C(11));
      sail_int zgsz3890;
      CREATE(sail_int)(&zgsz3890);
      CONVERT_OF(sail_int, mach_int)(&zgsz3890, INT64_C(7));
      lbits zgsz3891;
      CREATE(lbits)(&zgsz3891);
      vector_subrange_lbits(&zgsz3891, zgsz3888, zgsz3889, zgsz3890);
      zuz3108 = CONVERT_OF(fbits, lbits)(zgsz3891, true);
      KILL(lbits)(&zgsz3891);
      KILL(sail_int)(&zgsz3890);
      KILL(sail_int)(&zgsz3889);
      KILL(lbits)(&zgsz3888);
    }
    struct zast zgaz3236;
    CREATE(zast)(&zgaz3236);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3235;
      {
        uint64_t zgaz3233;
        zgaz3233 = ztREG(zuz3108);
        uint64_t zgaz3234;
        zgaz3234 = ztREG(zuz3107);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3231;
        zgsz3231.ztup0 = zgaz3233;
        zgsz3231.ztup1 = zgaz3234;
        zgaz3235 = zgsz3231;
      }
      zCGETLEN(&zgaz3236, zgaz3235);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3236);
    KILL(zast)(&zgaz3236);
    goto finish_match_202;
  }
case_217: ;
  {
    uint64_t zv__62;
    zv__62 = zmergez3var;
    bool zgaz3250;
    {
      uint64_t zgaz3246;
      {
        lbits zgsz3898;
        CREATE(lbits)(&zgsz3898);
        CONVERT_OF(lbits, fbits)(&zgsz3898, zv__62, UINT64_C(32) , true);
        sail_int zgsz3899;
        CREATE(sail_int)(&zgsz3899);
        CONVERT_OF(sail_int, mach_int)(&zgsz3899, INT64_C(31));
        sail_int zgsz3900;
        CREATE(sail_int)(&zgsz3900);
        CONVERT_OF(sail_int, mach_int)(&zgsz3900, INT64_C(20));
        lbits zgsz3901;
        CREATE(lbits)(&zgsz3901);
        vector_subrange_lbits(&zgsz3901, zgsz3898, zgsz3899, zgsz3900);
        zgaz3246 = CONVERT_OF(fbits, lbits)(zgsz3901, true);
        KILL(lbits)(&zgsz3901);
        KILL(sail_int)(&zgsz3900);
        KILL(sail_int)(&zgsz3899);
        KILL(lbits)(&zgsz3898);
      }
      {
        lbits zgsz3896;
        CREATE(lbits)(&zgsz3896);
        CONVERT_OF(lbits, fbits)(&zgsz3896, zgaz3246, UINT64_C(12) , true);
        lbits zgsz3897;
        CREATE(lbits)(&zgsz3897);
        CONVERT_OF(lbits, fbits)(&zgsz3897, UINT64_C(0x403), UINT64_C(12) , true);
        zgaz3250 = eq_bits(zgsz3896, zgsz3897);
        KILL(lbits)(&zgsz3897);
        KILL(lbits)(&zgsz3896);
      }
    }
    bool zgsz3234;
    if (zgaz3250) {
      bool zgaz3249;
      {
        uint64_t zgaz3247;
        {
          lbits zgsz3904;
          CREATE(lbits)(&zgsz3904);
          CONVERT_OF(lbits, fbits)(&zgsz3904, zv__62, UINT64_C(32) , true);
          sail_int zgsz3905;
          CREATE(sail_int)(&zgsz3905);
          CONVERT_OF(sail_int, mach_int)(&zgsz3905, INT64_C(14));
          sail_int zgsz3906;
          CREATE(sail_int)(&zgsz3906);
          CONVERT_OF(sail_int, mach_int)(&zgsz3906, INT64_C(12));
          lbits zgsz3907;
          CREATE(lbits)(&zgsz3907);
          vector_subrange_lbits(&zgsz3907, zgsz3904, zgsz3905, zgsz3906);
          zgaz3247 = CONVERT_OF(fbits, lbits)(zgsz3907, true);
          KILL(lbits)(&zgsz3907);
          KILL(sail_int)(&zgsz3906);
          KILL(sail_int)(&zgsz3905);
          KILL(lbits)(&zgsz3904);
        }
        {
          lbits zgsz3902;
          CREATE(lbits)(&zgsz3902);
          CONVERT_OF(lbits, fbits)(&zgsz3902, zgaz3247, UINT64_C(3) , true);
          lbits zgsz3903;
          CREATE(lbits)(&zgsz3903);
          CONVERT_OF(lbits, fbits)(&zgsz3903, UINT64_C(0b101), UINT64_C(3) , true);
          zgaz3249 = eq_bits(zgsz3902, zgsz3903);
          KILL(lbits)(&zgsz3903);
          KILL(lbits)(&zgsz3902);
        }
      }
      bool zgsz3233;
      if (zgaz3249) {
        uint64_t zgaz3248;
        {
          lbits zgsz3910;
          CREATE(lbits)(&zgsz3910);
          CONVERT_OF(lbits, fbits)(&zgsz3910, zv__62, UINT64_C(32) , true);
          sail_int zgsz3911;
          CREATE(sail_int)(&zgsz3911);
          CONVERT_OF(sail_int, mach_int)(&zgsz3911, INT64_C(6));
          sail_int zgsz3912;
          CREATE(sail_int)(&zgsz3912);
          CONVERT_OF(sail_int, mach_int)(&zgsz3912, INT64_C(0));
          lbits zgsz3913;
          CREATE(lbits)(&zgsz3913);
          vector_subrange_lbits(&zgsz3913, zgsz3910, zgsz3911, zgsz3912);
          zgaz3248 = CONVERT_OF(fbits, lbits)(zgsz3913, true);
          KILL(lbits)(&zgsz3913);
          KILL(sail_int)(&zgsz3912);
          KILL(sail_int)(&zgsz3911);
          KILL(lbits)(&zgsz3910);
        }
        {
          lbits zgsz3908;
          CREATE(lbits)(&zgsz3908);
          CONVERT_OF(lbits, fbits)(&zgsz3908, zgaz3248, UINT64_C(7) , true);
          lbits zgsz3909;
          CREATE(lbits)(&zgsz3909);
          CONVERT_OF(lbits, fbits)(&zgsz3909, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3233 = eq_bits(zgsz3908, zgsz3909);
          KILL(lbits)(&zgsz3909);
          KILL(lbits)(&zgsz3908);
        }
      } else {  zgsz3233 = false;  }
      zgsz3234 = zgsz3233;
    } else {  zgsz3234 = false;  }
    bool zgsz3236;
    zgsz3236 = zgsz3234;
    if (!(zgsz3236)) {

      goto case_218;
    }
    uint64_t zuz3109;
    {
      lbits zgsz3918;
      CREATE(lbits)(&zgsz3918);
      CONVERT_OF(lbits, fbits)(&zgsz3918, zv__62, UINT64_C(32) , true);
      sail_int zgsz3919;
      CREATE(sail_int)(&zgsz3919);
      CONVERT_OF(sail_int, mach_int)(&zgsz3919, INT64_C(19));
      sail_int zgsz3920;
      CREATE(sail_int)(&zgsz3920);
      CONVERT_OF(sail_int, mach_int)(&zgsz3920, INT64_C(15));
      lbits zgsz3921;
      CREATE(lbits)(&zgsz3921);
      vector_subrange_lbits(&zgsz3921, zgsz3918, zgsz3919, zgsz3920);
      zuz3109 = CONVERT_OF(fbits, lbits)(zgsz3921, true);
      KILL(lbits)(&zgsz3921);
      KILL(sail_int)(&zgsz3920);
      KILL(sail_int)(&zgsz3919);
      KILL(lbits)(&zgsz3918);
    }
    uint64_t zuz3110;
    {
      lbits zgsz3914;
      CREATE(lbits)(&zgsz3914);
      CONVERT_OF(lbits, fbits)(&zgsz3914, zv__62, UINT64_C(32) , true);
      sail_int zgsz3915;
      CREATE(sail_int)(&zgsz3915);
      CONVERT_OF(sail_int, mach_int)(&zgsz3915, INT64_C(11));
      sail_int zgsz3916;
      CREATE(sail_int)(&zgsz3916);
      CONVERT_OF(sail_int, mach_int)(&zgsz3916, INT64_C(7));
      lbits zgsz3917;
      CREATE(lbits)(&zgsz3917);
      vector_subrange_lbits(&zgsz3917, zgsz3914, zgsz3915, zgsz3916);
      zuz3110 = CONVERT_OF(fbits, lbits)(zgsz3917, true);
      KILL(lbits)(&zgsz3917);
      KILL(sail_int)(&zgsz3916);
      KILL(sail_int)(&zgsz3915);
      KILL(lbits)(&zgsz3914);
    }
    struct zast zgaz3245;
    CREATE(zast)(&zgaz3245);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3244;
      {
        uint64_t zgaz3242;
        zgaz3242 = ztREG(zuz3110);
        uint64_t zgaz3243;
        zgaz3243 = ztREG(zuz3109);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3235;
        zgsz3235.ztup0 = zgaz3242;
        zgsz3235.ztup1 = zgaz3243;
        zgaz3244 = zgsz3235;
      }
      zCGETADDR(&zgaz3245, zgaz3244);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3245);
    KILL(zast)(&zgaz3245);
    goto finish_match_202;
  }
case_218: ;
  {
    uint64_t zv__67;
    zv__67 = zmergez3var;
    bool zgaz3260;
    {
      uint64_t zgaz3256;
      {
        lbits zgsz3924;
        CREATE(lbits)(&zgsz3924);
        CONVERT_OF(lbits, fbits)(&zgsz3924, zv__67, UINT64_C(32) , true);
        sail_int zgsz3925;
        CREATE(sail_int)(&zgsz3925);
        CONVERT_OF(sail_int, mach_int)(&zgsz3925, INT64_C(31));
        sail_int zgsz3926;
        CREATE(sail_int)(&zgsz3926);
        CONVERT_OF(sail_int, mach_int)(&zgsz3926, INT64_C(25));
        lbits zgsz3927;
        CREATE(lbits)(&zgsz3927);
        vector_subrange_lbits(&zgsz3927, zgsz3924, zgsz3925, zgsz3926);
        zgaz3256 = CONVERT_OF(fbits, lbits)(zgsz3927, true);
        KILL(lbits)(&zgsz3927);
        KILL(sail_int)(&zgsz3926);
        KILL(sail_int)(&zgsz3925);
        KILL(lbits)(&zgsz3924);
      }
      {
        lbits zgsz3922;
        CREATE(lbits)(&zgsz3922);
        CONVERT_OF(lbits, fbits)(&zgsz3922, zgaz3256, UINT64_C(7) , true);
        lbits zgsz3923;
        CREATE(lbits)(&zgsz3923);
        CONVERT_OF(lbits, fbits)(&zgsz3923, UINT64_C(0b0010001), UINT64_C(7) , true);
        zgaz3260 = eq_bits(zgsz3922, zgsz3923);
        KILL(lbits)(&zgsz3923);
        KILL(lbits)(&zgsz3922);
      }
    }
    bool zgsz3238;
    if (zgaz3260) {
      bool zgaz3259;
      {
        uint64_t zgaz3257;
        {
          lbits zgsz3930;
          CREATE(lbits)(&zgsz3930);
          CONVERT_OF(lbits, fbits)(&zgsz3930, zv__67, UINT64_C(32) , true);
          sail_int zgsz3931;
          CREATE(sail_int)(&zgsz3931);
          CONVERT_OF(sail_int, mach_int)(&zgsz3931, INT64_C(14));
          sail_int zgsz3932;
          CREATE(sail_int)(&zgsz3932);
          CONVERT_OF(sail_int, mach_int)(&zgsz3932, INT64_C(12));
          lbits zgsz3933;
          CREATE(lbits)(&zgsz3933);
          vector_subrange_lbits(&zgsz3933, zgsz3930, zgsz3931, zgsz3932);
          zgaz3257 = CONVERT_OF(fbits, lbits)(zgsz3933, true);
          KILL(lbits)(&zgsz3933);
          KILL(sail_int)(&zgsz3932);
          KILL(sail_int)(&zgsz3931);
          KILL(lbits)(&zgsz3930);
        }
        {
          lbits zgsz3928;
          CREATE(lbits)(&zgsz3928);
          CONVERT_OF(lbits, fbits)(&zgsz3928, zgaz3257, UINT64_C(3) , true);
          lbits zgsz3929;
          CREATE(lbits)(&zgsz3929);
          CONVERT_OF(lbits, fbits)(&zgsz3929, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3259 = eq_bits(zgsz3928, zgsz3929);
          KILL(lbits)(&zgsz3929);
          KILL(lbits)(&zgsz3928);
        }
      }
      bool zgsz3237;
      if (zgaz3259) {
        uint64_t zgaz3258;
        {
          lbits zgsz3936;
          CREATE(lbits)(&zgsz3936);
          CONVERT_OF(lbits, fbits)(&zgsz3936, zv__67, UINT64_C(32) , true);
          sail_int zgsz3937;
          CREATE(sail_int)(&zgsz3937);
          CONVERT_OF(sail_int, mach_int)(&zgsz3937, INT64_C(6));
          sail_int zgsz3938;
          CREATE(sail_int)(&zgsz3938);
          CONVERT_OF(sail_int, mach_int)(&zgsz3938, INT64_C(0));
          lbits zgsz3939;
          CREATE(lbits)(&zgsz3939);
          vector_subrange_lbits(&zgsz3939, zgsz3936, zgsz3937, zgsz3938);
          zgaz3258 = CONVERT_OF(fbits, lbits)(zgsz3939, true);
          KILL(lbits)(&zgsz3939);
          KILL(sail_int)(&zgsz3938);
          KILL(sail_int)(&zgsz3937);
          KILL(lbits)(&zgsz3936);
        }
        {
          lbits zgsz3934;
          CREATE(lbits)(&zgsz3934);
          CONVERT_OF(lbits, fbits)(&zgsz3934, zgaz3258, UINT64_C(7) , true);
          lbits zgsz3935;
          CREATE(lbits)(&zgsz3935);
          CONVERT_OF(lbits, fbits)(&zgsz3935, UINT64_C(0b1011011), UINT64_C(7) , true);
          zgsz3237 = eq_bits(zgsz3934, zgsz3935);
          KILL(lbits)(&zgsz3935);
          KILL(lbits)(&zgsz3934);
        }
      } else {  zgsz3237 = false;  }
      zgsz3238 = zgsz3237;
    } else {  zgsz3238 = false;  }
    bool zgsz3240;
    zgsz3240 = zgsz3238;
    if (!(zgsz3240)) {

      goto case_219;
    }
    uint64_t zuz3111;
    {
      lbits zgsz3948;
      CREATE(lbits)(&zgsz3948);
      CONVERT_OF(lbits, fbits)(&zgsz3948, zv__67, UINT64_C(32) , true);
      sail_int zgsz3949;
      CREATE(sail_int)(&zgsz3949);
      CONVERT_OF(sail_int, mach_int)(&zgsz3949, INT64_C(24));
      sail_int zgsz3950;
      CREATE(sail_int)(&zgsz3950);
      CONVERT_OF(sail_int, mach_int)(&zgsz3950, INT64_C(20));
      lbits zgsz3951;
      CREATE(lbits)(&zgsz3951);
      vector_subrange_lbits(&zgsz3951, zgsz3948, zgsz3949, zgsz3950);
      zuz3111 = CONVERT_OF(fbits, lbits)(zgsz3951, true);
      KILL(lbits)(&zgsz3951);
      KILL(sail_int)(&zgsz3950);
      KILL(sail_int)(&zgsz3949);
      KILL(lbits)(&zgsz3948);
    }
    uint64_t zuz3112;
    {
      lbits zgsz3944;
      CREATE(lbits)(&zgsz3944);
      CONVERT_OF(lbits, fbits)(&zgsz3944, zv__67, UINT64_C(32) , true);
      sail_int zgsz3945;
      CREATE(sail_int)(&zgsz3945);
      CONVERT_OF(sail_int, mach_int)(&zgsz3945, INT64_C(19));
      sail_int zgsz3946;
      CREATE(sail_int)(&zgsz3946);
      CONVERT_OF(sail_int, mach_int)(&zgsz3946, INT64_C(15));
      lbits zgsz3947;
      CREATE(lbits)(&zgsz3947);
      vector_subrange_lbits(&zgsz3947, zgsz3944, zgsz3945, zgsz3946);
      zuz3112 = CONVERT_OF(fbits, lbits)(zgsz3947, true);
      KILL(lbits)(&zgsz3947);
      KILL(sail_int)(&zgsz3946);
      KILL(sail_int)(&zgsz3945);
      KILL(lbits)(&zgsz3944);
    }
    uint64_t zuz3113;
    {
      lbits zgsz3940;
      CREATE(lbits)(&zgsz3940);
      CONVERT_OF(lbits, fbits)(&zgsz3940, zv__67, UINT64_C(32) , true);
      sail_int zgsz3941;
      CREATE(sail_int)(&zgsz3941);
      CONVERT_OF(sail_int, mach_int)(&zgsz3941, INT64_C(11));
      sail_int zgsz3942;
      CREATE(sail_int)(&zgsz3942);
      CONVERT_OF(sail_int, mach_int)(&zgsz3942, INT64_C(7));
      lbits zgsz3943;
      CREATE(lbits)(&zgsz3943);
      vector_subrange_lbits(&zgsz3943, zgsz3940, zgsz3941, zgsz3942);
      zuz3113 = CONVERT_OF(fbits, lbits)(zgsz3943, true);
      KILL(lbits)(&zgsz3943);
      KILL(sail_int)(&zgsz3942);
      KILL(sail_int)(&zgsz3941);
      KILL(lbits)(&zgsz3940);
    }
    struct zast zgaz3255;
    CREATE(zast)(&zgaz3255);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3254;
      {
        uint64_t zgaz3251;
        zgaz3251 = ztREG(zuz3113);
        uint64_t zgaz3252;
        zgaz3252 = ztREG(zuz3112);
        uint64_t zgaz3253;
        zgaz3253 = ztREG(zuz3111);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3239;
        zgsz3239.ztup0 = zgaz3251;
        zgsz3239.ztup1 = zgaz3252;
        zgsz3239.ztup2 = zgaz3253;
        zgaz3254 = zgsz3239;
      }
      zCINCOFFSET(&zgaz3255, zgaz3254);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3255);
    KILL(zast)(&zgaz3255);
    goto finish_match_202;
  }
case_219: ;
  {
    uint64_t zv__71;
    zv__71 = zmergez3var;
    bool zgaz3269;
    {
      uint64_t zgaz3265;
      {
        lbits zgsz3954;
        CREATE(lbits)(&zgsz3954);
        CONVERT_OF(lbits, fbits)(&zgsz3954, zv__71, UINT64_C(32) , true);
        sail_int zgsz3955;
        CREATE(sail_int)(&zgsz3955);
        CONVERT_OF(sail_int, mach_int)(&zgsz3955, INT64_C(31));
        sail_int zgsz3956;
        CREATE(sail_int)(&zgsz3956);
        CONVERT_OF(sail_int, mach_int)(&zgsz3956, INT64_C(20));
        lbits zgsz3957;
        CREATE(lbits)(&zgsz3957);
        vector_subrange_lbits(&zgsz3957, zgsz3954, zgsz3955, zgsz3956);
        zgaz3265 = CONVERT_OF(fbits, lbits)(zgsz3957, true);
        KILL(lbits)(&zgsz3957);
        KILL(sail_int)(&zgsz3956);
        KILL(sail_int)(&zgsz3955);
        KILL(lbits)(&zgsz3954);
      }
      {
        lbits zgsz3952;
        CREATE(lbits)(&zgsz3952);
        CONVERT_OF(lbits, fbits)(&zgsz3952, zgaz3265, UINT64_C(12) , true);
        lbits zgsz3953;
        CREATE(lbits)(&zgsz3953);
        CONVERT_OF(lbits, fbits)(&zgsz3953, UINT64_C(0x405), UINT64_C(12) , true);
        zgaz3269 = eq_bits(zgsz3952, zgsz3953);
        KILL(lbits)(&zgsz3953);
        KILL(lbits)(&zgsz3952);
      }
    }
    bool zgsz3242;
    if (zgaz3269) {
      bool zgaz3268;
      {
        uint64_t zgaz3266;
        {
          lbits zgsz3960;
          CREATE(lbits)(&zgsz3960);
          CONVERT_OF(lbits, fbits)(&zgsz3960, zv__71, UINT64_C(32) , true);
          sail_int zgsz3961;
          CREATE(sail_int)(&zgsz3961);
          CONVERT_OF(sail_int, mach_int)(&zgsz3961, INT64_C(14));
          sail_int zgsz3962;
          CREATE(sail_int)(&zgsz3962);
          CONVERT_OF(sail_int, mach_int)(&zgsz3962, INT64_C(12));
          lbits zgsz3963;
          CREATE(lbits)(&zgsz3963);
          vector_subrange_lbits(&zgsz3963, zgsz3960, zgsz3961, zgsz3962);
          zgaz3266 = CONVERT_OF(fbits, lbits)(zgsz3963, true);
          KILL(lbits)(&zgsz3963);
          KILL(sail_int)(&zgsz3962);
          KILL(sail_int)(&zgsz3961);
          KILL(lbits)(&zgsz3960);
        }
        {
          lbits zgsz3958;
          CREATE(lbits)(&zgsz3958);
          CONVERT_OF(lbits, fbits)(&zgsz3958, zgaz3266, UINT64_C(3) , true);
          lbits zgsz3959;
          CREATE(lbits)(&zgsz3959);
          CONVERT_OF(lbits, fbits)(&zgsz3959, UINT64_C(0b101), UINT64_C(3) , true);
          zgaz3268 = eq_bits(zgsz3958, zgsz3959);
          KILL(lbits)(&zgsz3959);
          KILL(lbits)(&zgsz3958);
        }
      }
      bool zgsz3241;
      if (zgaz3268) {
        uint64_t zgaz3267;
        {
          lbits zgsz3966;
          CREATE(lbits)(&zgsz3966);
          CONVERT_OF(lbits, fbits)(&zgsz3966, zv__71, UINT64_C(32) , true);
          sail_int zgsz3967;
          CREATE(sail_int)(&zgsz3967);
          CONVERT_OF(sail_int, mach_int)(&zgsz3967, INT64_C(6));
          sail_int zgsz3968;
          CREATE(sail_int)(&zgsz3968);
          CONVERT_OF(sail_int, mach_int)(&zgsz3968, INT64_C(0));
          lbits zgsz3969;
          CREATE(lbits)(&zgsz3969);
          vector_subrange_lbits(&zgsz3969, zgsz3966, zgsz3967, zgsz3968);
          zgaz3267 = CONVERT_OF(fbits, lbits)(zgsz3969, true);
          KILL(lbits)(&zgsz3969);
          KILL(sail_int)(&zgsz3968);
          KILL(sail_int)(&zgsz3967);
          KILL(lbits)(&zgsz3966);
        }
        {
          lbits zgsz3964;
          CREATE(lbits)(&zgsz3964);
          CONVERT_OF(lbits, fbits)(&zgsz3964, zgaz3267, UINT64_C(7) , true);
          lbits zgsz3965;
          CREATE(lbits)(&zgsz3965);
          CONVERT_OF(lbits, fbits)(&zgsz3965, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3241 = eq_bits(zgsz3964, zgsz3965);
          KILL(lbits)(&zgsz3965);
          KILL(lbits)(&zgsz3964);
        }
      } else {  zgsz3241 = false;  }
      zgsz3242 = zgsz3241;
    } else {  zgsz3242 = false;  }
    bool zgsz3244;
    zgsz3244 = zgsz3242;
    if (!(zgsz3244)) {

      goto case_220;
    }
    uint64_t zuz3114;
    {
      lbits zgsz3974;
      CREATE(lbits)(&zgsz3974);
      CONVERT_OF(lbits, fbits)(&zgsz3974, zv__71, UINT64_C(32) , true);
      sail_int zgsz3975;
      CREATE(sail_int)(&zgsz3975);
      CONVERT_OF(sail_int, mach_int)(&zgsz3975, INT64_C(19));
      sail_int zgsz3976;
      CREATE(sail_int)(&zgsz3976);
      CONVERT_OF(sail_int, mach_int)(&zgsz3976, INT64_C(15));
      lbits zgsz3977;
      CREATE(lbits)(&zgsz3977);
      vector_subrange_lbits(&zgsz3977, zgsz3974, zgsz3975, zgsz3976);
      zuz3114 = CONVERT_OF(fbits, lbits)(zgsz3977, true);
      KILL(lbits)(&zgsz3977);
      KILL(sail_int)(&zgsz3976);
      KILL(sail_int)(&zgsz3975);
      KILL(lbits)(&zgsz3974);
    }
    uint64_t zuz3115;
    {
      lbits zgsz3970;
      CREATE(lbits)(&zgsz3970);
      CONVERT_OF(lbits, fbits)(&zgsz3970, zv__71, UINT64_C(32) , true);
      sail_int zgsz3971;
      CREATE(sail_int)(&zgsz3971);
      CONVERT_OF(sail_int, mach_int)(&zgsz3971, INT64_C(11));
      sail_int zgsz3972;
      CREATE(sail_int)(&zgsz3972);
      CONVERT_OF(sail_int, mach_int)(&zgsz3972, INT64_C(7));
      lbits zgsz3973;
      CREATE(lbits)(&zgsz3973);
      vector_subrange_lbits(&zgsz3973, zgsz3970, zgsz3971, zgsz3972);
      zuz3115 = CONVERT_OF(fbits, lbits)(zgsz3973, true);
      KILL(lbits)(&zgsz3973);
      KILL(sail_int)(&zgsz3972);
      KILL(sail_int)(&zgsz3971);
      KILL(lbits)(&zgsz3970);
    }
    struct zast zgaz3264;
    CREATE(zast)(&zgaz3264);
    {
      struct ztuple_z8z5bv2zCz0z5bv2z9 zgaz3263;
      {
        uint64_t zgaz3261;
        zgaz3261 = ztREG(zuz3115);
        uint64_t zgaz3262;
        zgaz3262 = ztREG(zuz3114);
        struct ztuple_z8z5bv2zCz0z5bv2z9 zgsz3243;
        zgsz3243.ztup0 = zgaz3261;
        zgsz3243.ztup1 = zgaz3262;
        zgaz3263 = zgsz3243;
      }
      zCGETTAG(&zgaz3264, zgaz3263);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3264);
    KILL(zast)(&zgaz3264);
    goto finish_match_202;
  }
case_220: ;
  {
    uint64_t zv__76;
    zv__76 = zmergez3var;
    bool zgaz3279;
    {
      uint64_t zgaz3275;
      {
        lbits zgsz3980;
        CREATE(lbits)(&zgsz3980);
        CONVERT_OF(lbits, fbits)(&zgsz3980, zv__76, UINT64_C(32) , true);
        sail_int zgsz3981;
        CREATE(sail_int)(&zgsz3981);
        CONVERT_OF(sail_int, mach_int)(&zgsz3981, INT64_C(31));
        sail_int zgsz3982;
        CREATE(sail_int)(&zgsz3982);
        CONVERT_OF(sail_int, mach_int)(&zgsz3982, INT64_C(25));
        lbits zgsz3983;
        CREATE(lbits)(&zgsz3983);
        vector_subrange_lbits(&zgsz3983, zgsz3980, zgsz3981, zgsz3982);
        zgaz3275 = CONVERT_OF(fbits, lbits)(zgsz3983, true);
        KILL(lbits)(&zgsz3983);
        KILL(sail_int)(&zgsz3982);
        KILL(sail_int)(&zgsz3981);
        KILL(lbits)(&zgsz3980);
      }
      {
        lbits zgsz3978;
        CREATE(lbits)(&zgsz3978);
        CONVERT_OF(lbits, fbits)(&zgsz3978, zgaz3275, UINT64_C(7) , true);
        lbits zgsz3979;
        CREATE(lbits)(&zgsz3979);
        CONVERT_OF(lbits, fbits)(&zgsz3979, UINT64_C(0b0001101), UINT64_C(7) , true);
        zgaz3279 = eq_bits(zgsz3978, zgsz3979);
        KILL(lbits)(&zgsz3979);
        KILL(lbits)(&zgsz3978);
      }
    }
    bool zgsz3246;
    if (zgaz3279) {
      bool zgaz3278;
      {
        uint64_t zgaz3276;
        {
          lbits zgsz3986;
          CREATE(lbits)(&zgsz3986);
          CONVERT_OF(lbits, fbits)(&zgsz3986, zv__76, UINT64_C(32) , true);
          sail_int zgsz3987;
          CREATE(sail_int)(&zgsz3987);
          CONVERT_OF(sail_int, mach_int)(&zgsz3987, INT64_C(14));
          sail_int zgsz3988;
          CREATE(sail_int)(&zgsz3988);
          CONVERT_OF(sail_int, mach_int)(&zgsz3988, INT64_C(12));
          lbits zgsz3989;
          CREATE(lbits)(&zgsz3989);
          vector_subrange_lbits(&zgsz3989, zgsz3986, zgsz3987, zgsz3988);
          zgaz3276 = CONVERT_OF(fbits, lbits)(zgsz3989, true);
          KILL(lbits)(&zgsz3989);
          KILL(sail_int)(&zgsz3988);
          KILL(sail_int)(&zgsz3987);
          KILL(lbits)(&zgsz3986);
        }
        {
          lbits zgsz3984;
          CREATE(lbits)(&zgsz3984);
          CONVERT_OF(lbits, fbits)(&zgsz3984, zgaz3276, UINT64_C(3) , true);
          lbits zgsz3985;
          CREATE(lbits)(&zgsz3985);
          CONVERT_OF(lbits, fbits)(&zgsz3985, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3278 = eq_bits(zgsz3984, zgsz3985);
          KILL(lbits)(&zgsz3985);
          KILL(lbits)(&zgsz3984);
        }
      }
      bool zgsz3245;
      if (zgaz3278) {
        uint64_t zgaz3277;
        {
          lbits zgsz3992;
          CREATE(lbits)(&zgsz3992);
          CONVERT_OF(lbits, fbits)(&zgsz3992, zv__76, UINT64_C(32) , true);
          sail_int zgsz3993;
          CREATE(sail_int)(&zgsz3993);
          CONVERT_OF(sail_int, mach_int)(&zgsz3993, INT64_C(6));
          sail_int zgsz3994;
          CREATE(sail_int)(&zgsz3994);
          CONVERT_OF(sail_int, mach_int)(&zgsz3994, INT64_C(0));
          lbits zgsz3995;
          CREATE(lbits)(&zgsz3995);
          vector_subrange_lbits(&zgsz3995, zgsz3992, zgsz3993, zgsz3994);
          zgaz3277 = CONVERT_OF(fbits, lbits)(zgsz3995, true);
          KILL(lbits)(&zgsz3995);
          KILL(sail_int)(&zgsz3994);
          KILL(sail_int)(&zgsz3993);
          KILL(lbits)(&zgsz3992);
        }
        {
          lbits zgsz3990;
          CREATE(lbits)(&zgsz3990);
          CONVERT_OF(lbits, fbits)(&zgsz3990, zgaz3277, UINT64_C(7) , true);
          lbits zgsz3991;
          CREATE(lbits)(&zgsz3991);
          CONVERT_OF(lbits, fbits)(&zgsz3991, UINT64_C(0b1011011), UINT64_C(7) , true);
          zgsz3245 = eq_bits(zgsz3990, zgsz3991);
          KILL(lbits)(&zgsz3991);
          KILL(lbits)(&zgsz3990);
        }
      } else {  zgsz3245 = false;  }
      zgsz3246 = zgsz3245;
    } else {  zgsz3246 = false;  }
    bool zgsz3248;
    zgsz3248 = zgsz3246;
    if (!(zgsz3248)) {

      goto case_221;
    }
    uint64_t zuz3116;
    {
      lbits zgsz31004;
      CREATE(lbits)(&zgsz31004);
      CONVERT_OF(lbits, fbits)(&zgsz31004, zv__76, UINT64_C(32) , true);
      sail_int zgsz31005;
      CREATE(sail_int)(&zgsz31005);
      CONVERT_OF(sail_int, mach_int)(&zgsz31005, INT64_C(24));
      sail_int zgsz31006;
      CREATE(sail_int)(&zgsz31006);
      CONVERT_OF(sail_int, mach_int)(&zgsz31006, INT64_C(20));
      lbits zgsz31007;
      CREATE(lbits)(&zgsz31007);
      vector_subrange_lbits(&zgsz31007, zgsz31004, zgsz31005, zgsz31006);
      zuz3116 = CONVERT_OF(fbits, lbits)(zgsz31007, true);
      KILL(lbits)(&zgsz31007);
      KILL(sail_int)(&zgsz31006);
      KILL(sail_int)(&zgsz31005);
      KILL(lbits)(&zgsz31004);
    }
    uint64_t zuz3117;
    {
      lbits zgsz31000;
      CREATE(lbits)(&zgsz31000);
      CONVERT_OF(lbits, fbits)(&zgsz31000, zv__76, UINT64_C(32) , true);
      sail_int zgsz31001;
      CREATE(sail_int)(&zgsz31001);
      CONVERT_OF(sail_int, mach_int)(&zgsz31001, INT64_C(19));
      sail_int zgsz31002;
      CREATE(sail_int)(&zgsz31002);
      CONVERT_OF(sail_int, mach_int)(&zgsz31002, INT64_C(15));
      lbits zgsz31003;
      CREATE(lbits)(&zgsz31003);
      vector_subrange_lbits(&zgsz31003, zgsz31000, zgsz31001, zgsz31002);
      zuz3117 = CONVERT_OF(fbits, lbits)(zgsz31003, true);
      KILL(lbits)(&zgsz31003);
      KILL(sail_int)(&zgsz31002);
      KILL(sail_int)(&zgsz31001);
      KILL(lbits)(&zgsz31000);
    }
    uint64_t zuz3118;
    {
      lbits zgsz3996;
      CREATE(lbits)(&zgsz3996);
      CONVERT_OF(lbits, fbits)(&zgsz3996, zv__76, UINT64_C(32) , true);
      sail_int zgsz3997;
      CREATE(sail_int)(&zgsz3997);
      CONVERT_OF(sail_int, mach_int)(&zgsz3997, INT64_C(11));
      sail_int zgsz3998;
      CREATE(sail_int)(&zgsz3998);
      CONVERT_OF(sail_int, mach_int)(&zgsz3998, INT64_C(7));
      lbits zgsz3999;
      CREATE(lbits)(&zgsz3999);
      vector_subrange_lbits(&zgsz3999, zgsz3996, zgsz3997, zgsz3998);
      zuz3118 = CONVERT_OF(fbits, lbits)(zgsz3999, true);
      KILL(lbits)(&zgsz3999);
      KILL(sail_int)(&zgsz3998);
      KILL(sail_int)(&zgsz3997);
      KILL(lbits)(&zgsz3996);
    }
    struct zast zgaz3274;
    CREATE(zast)(&zgaz3274);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3273;
      {
        uint64_t zgaz3270;
        zgaz3270 = ztREG(zuz3118);
        uint64_t zgaz3271;
        zgaz3271 = ztREG(zuz3117);
        uint64_t zgaz3272;
        zgaz3272 = ztREG(zuz3116);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3247;
        zgsz3247.ztup0 = zgaz3270;
        zgsz3247.ztup1 = zgaz3271;
        zgsz3247.ztup2 = zgaz3272;
        zgaz3273 = zgsz3247;
      }
      zCANDPERM(&zgaz3274, zgaz3273);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3274);
    KILL(zast)(&zgaz3274);
    goto finish_match_202;
  }
case_221: ;
  {
    uint64_t zv__80;
    zv__80 = zmergez3var;
    bool zgaz3289;
    {
      uint64_t zgaz3285;
      {
        lbits zgsz31010;
        CREATE(lbits)(&zgsz31010);
        CONVERT_OF(lbits, fbits)(&zgsz31010, zv__80, UINT64_C(32) , true);
        sail_int zgsz31011;
        CREATE(sail_int)(&zgsz31011);
        CONVERT_OF(sail_int, mach_int)(&zgsz31011, INT64_C(31));
        sail_int zgsz31012;
        CREATE(sail_int)(&zgsz31012);
        CONVERT_OF(sail_int, mach_int)(&zgsz31012, INT64_C(25));
        lbits zgsz31013;
        CREATE(lbits)(&zgsz31013);
        vector_subrange_lbits(&zgsz31013, zgsz31010, zgsz31011, zgsz31012);
        zgaz3285 = CONVERT_OF(fbits, lbits)(zgsz31013, true);
        KILL(lbits)(&zgsz31013);
        KILL(sail_int)(&zgsz31012);
        KILL(sail_int)(&zgsz31011);
        KILL(lbits)(&zgsz31010);
      }
      {
        lbits zgsz31008;
        CREATE(lbits)(&zgsz31008);
        CONVERT_OF(lbits, fbits)(&zgsz31008, zgaz3285, UINT64_C(7) , true);
        lbits zgsz31009;
        CREATE(lbits)(&zgsz31009);
        CONVERT_OF(lbits, fbits)(&zgsz31009, UINT64_C(0b0000000), UINT64_C(7) , true);
        zgaz3289 = eq_bits(zgsz31008, zgsz31009);
        KILL(lbits)(&zgsz31009);
        KILL(lbits)(&zgsz31008);
      }
    }
    bool zgsz3250;
    if (zgaz3289) {
      bool zgaz3288;
      {
        uint64_t zgaz3286;
        {
          lbits zgsz31016;
          CREATE(lbits)(&zgsz31016);
          CONVERT_OF(lbits, fbits)(&zgsz31016, zv__80, UINT64_C(32) , true);
          sail_int zgsz31017;
          CREATE(sail_int)(&zgsz31017);
          CONVERT_OF(sail_int, mach_int)(&zgsz31017, INT64_C(14));
          sail_int zgsz31018;
          CREATE(sail_int)(&zgsz31018);
          CONVERT_OF(sail_int, mach_int)(&zgsz31018, INT64_C(12));
          lbits zgsz31019;
          CREATE(lbits)(&zgsz31019);
          vector_subrange_lbits(&zgsz31019, zgsz31016, zgsz31017, zgsz31018);
          zgaz3286 = CONVERT_OF(fbits, lbits)(zgsz31019, true);
          KILL(lbits)(&zgsz31019);
          KILL(sail_int)(&zgsz31018);
          KILL(sail_int)(&zgsz31017);
          KILL(lbits)(&zgsz31016);
        }
        {
          lbits zgsz31014;
          CREATE(lbits)(&zgsz31014);
          CONVERT_OF(lbits, fbits)(&zgsz31014, zgaz3286, UINT64_C(3) , true);
          lbits zgsz31015;
          CREATE(lbits)(&zgsz31015);
          CONVERT_OF(lbits, fbits)(&zgsz31015, UINT64_C(0b010), UINT64_C(3) , true);
          zgaz3288 = eq_bits(zgsz31014, zgsz31015);
          KILL(lbits)(&zgsz31015);
          KILL(lbits)(&zgsz31014);
        }
      }
      bool zgsz3249;
      if (zgaz3288) {
        uint64_t zgaz3287;
        {
          lbits zgsz31022;
          CREATE(lbits)(&zgsz31022);
          CONVERT_OF(lbits, fbits)(&zgsz31022, zv__80, UINT64_C(32) , true);
          sail_int zgsz31023;
          CREATE(sail_int)(&zgsz31023);
          CONVERT_OF(sail_int, mach_int)(&zgsz31023, INT64_C(6));
          sail_int zgsz31024;
          CREATE(sail_int)(&zgsz31024);
          CONVERT_OF(sail_int, mach_int)(&zgsz31024, INT64_C(0));
          lbits zgsz31025;
          CREATE(lbits)(&zgsz31025);
          vector_subrange_lbits(&zgsz31025, zgsz31022, zgsz31023, zgsz31024);
          zgaz3287 = CONVERT_OF(fbits, lbits)(zgsz31025, true);
          KILL(lbits)(&zgsz31025);
          KILL(sail_int)(&zgsz31024);
          KILL(sail_int)(&zgsz31023);
          KILL(lbits)(&zgsz31022);
        }
        {
          lbits zgsz31020;
          CREATE(lbits)(&zgsz31020);
          CONVERT_OF(lbits, fbits)(&zgsz31020, zgaz3287, UINT64_C(7) , true);
          lbits zgsz31021;
          CREATE(lbits)(&zgsz31021);
          CONVERT_OF(lbits, fbits)(&zgsz31021, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3249 = eq_bits(zgsz31020, zgsz31021);
          KILL(lbits)(&zgsz31021);
          KILL(lbits)(&zgsz31020);
        }
      } else {  zgsz3249 = false;  }
      zgsz3250 = zgsz3249;
    } else {  zgsz3250 = false;  }
    bool zgsz3252;
    zgsz3252 = zgsz3250;
    if (!(zgsz3252)) {

      goto case_222;
    }
    uint64_t zuz3119;
    {
      lbits zgsz31034;
      CREATE(lbits)(&zgsz31034);
      CONVERT_OF(lbits, fbits)(&zgsz31034, zv__80, UINT64_C(32) , true);
      sail_int zgsz31035;
      CREATE(sail_int)(&zgsz31035);
      CONVERT_OF(sail_int, mach_int)(&zgsz31035, INT64_C(19));
      sail_int zgsz31036;
      CREATE(sail_int)(&zgsz31036);
      CONVERT_OF(sail_int, mach_int)(&zgsz31036, INT64_C(15));
      lbits zgsz31037;
      CREATE(lbits)(&zgsz31037);
      vector_subrange_lbits(&zgsz31037, zgsz31034, zgsz31035, zgsz31036);
      zuz3119 = CONVERT_OF(fbits, lbits)(zgsz31037, true);
      KILL(lbits)(&zgsz31037);
      KILL(sail_int)(&zgsz31036);
      KILL(sail_int)(&zgsz31035);
      KILL(lbits)(&zgsz31034);
    }
    uint64_t zuz3120;
    {
      lbits zgsz31030;
      CREATE(lbits)(&zgsz31030);
      CONVERT_OF(lbits, fbits)(&zgsz31030, zv__80, UINT64_C(32) , true);
      sail_int zgsz31031;
      CREATE(sail_int)(&zgsz31031);
      CONVERT_OF(sail_int, mach_int)(&zgsz31031, INT64_C(11));
      sail_int zgsz31032;
      CREATE(sail_int)(&zgsz31032);
      CONVERT_OF(sail_int, mach_int)(&zgsz31032, INT64_C(7));
      lbits zgsz31033;
      CREATE(lbits)(&zgsz31033);
      vector_subrange_lbits(&zgsz31033, zgsz31030, zgsz31031, zgsz31032);
      zuz3120 = CONVERT_OF(fbits, lbits)(zgsz31033, true);
      KILL(lbits)(&zgsz31033);
      KILL(sail_int)(&zgsz31032);
      KILL(sail_int)(&zgsz31031);
      KILL(lbits)(&zgsz31030);
    }
    uint64_t zuz3121;
    {
      lbits zgsz31026;
      CREATE(lbits)(&zgsz31026);
      CONVERT_OF(lbits, fbits)(&zgsz31026, zv__80, UINT64_C(32) , true);
      sail_int zgsz31027;
      CREATE(sail_int)(&zgsz31027);
      CONVERT_OF(sail_int, mach_int)(&zgsz31027, INT64_C(24));
      sail_int zgsz31028;
      CREATE(sail_int)(&zgsz31028);
      CONVERT_OF(sail_int, mach_int)(&zgsz31028, INT64_C(20));
      lbits zgsz31029;
      CREATE(lbits)(&zgsz31029);
      vector_subrange_lbits(&zgsz31029, zgsz31026, zgsz31027, zgsz31028);
      zuz3121 = CONVERT_OF(fbits, lbits)(zgsz31029, true);
      KILL(lbits)(&zgsz31029);
      KILL(sail_int)(&zgsz31028);
      KILL(sail_int)(&zgsz31027);
      KILL(lbits)(&zgsz31026);
    }
    struct zast zgaz3284;
    CREATE(zast)(&zgaz3284);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3283;
      {
        uint64_t zgaz3280;
        zgaz3280 = ztREG(zuz3120);
        uint64_t zgaz3281;
        zgaz3281 = ztREG(zuz3119);
        uint64_t zgaz3282;
        zgaz3282 = ztREG(zuz3121);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3251;
        zgsz3251.ztup0 = zgaz3280;
        zgsz3251.ztup1 = zgaz3281;
        zgsz3251.ztup2 = zgaz3282;
        zgaz3283 = zgsz3251;
      }
      zSLT(&zgaz3284, zgaz3283);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3284);
    KILL(zast)(&zgaz3284);
    goto finish_match_202;
  }
case_222: ;
  {
    uint64_t zv__84;
    zv__84 = zmergez3var;
    bool zgaz3296;
    {
      uint64_t zgaz3294;
      {
        lbits zgsz31040;
        CREATE(lbits)(&zgsz31040);
        CONVERT_OF(lbits, fbits)(&zgsz31040, zv__84, UINT64_C(32) , true);
        sail_int zgsz31041;
        CREATE(sail_int)(&zgsz31041);
        CONVERT_OF(sail_int, mach_int)(&zgsz31041, INT64_C(14));
        sail_int zgsz31042;
        CREATE(sail_int)(&zgsz31042);
        CONVERT_OF(sail_int, mach_int)(&zgsz31042, INT64_C(12));
        lbits zgsz31043;
        CREATE(lbits)(&zgsz31043);
        vector_subrange_lbits(&zgsz31043, zgsz31040, zgsz31041, zgsz31042);
        zgaz3294 = CONVERT_OF(fbits, lbits)(zgsz31043, true);
        KILL(lbits)(&zgsz31043);
        KILL(sail_int)(&zgsz31042);
        KILL(sail_int)(&zgsz31041);
        KILL(lbits)(&zgsz31040);
      }
      {
        lbits zgsz31038;
        CREATE(lbits)(&zgsz31038);
        CONVERT_OF(lbits, fbits)(&zgsz31038, zgaz3294, UINT64_C(3) , true);
        lbits zgsz31039;
        CREATE(lbits)(&zgsz31039);
        CONVERT_OF(lbits, fbits)(&zgsz31039, UINT64_C(0b010), UINT64_C(3) , true);
        zgaz3296 = eq_bits(zgsz31038, zgsz31039);
        KILL(lbits)(&zgsz31039);
        KILL(lbits)(&zgsz31038);
      }
    }
    bool zgsz3253;
    if (zgaz3296) {
      uint64_t zgaz3295;
      {
        lbits zgsz31046;
        CREATE(lbits)(&zgsz31046);
        CONVERT_OF(lbits, fbits)(&zgsz31046, zv__84, UINT64_C(32) , true);
        sail_int zgsz31047;
        CREATE(sail_int)(&zgsz31047);
        CONVERT_OF(sail_int, mach_int)(&zgsz31047, INT64_C(6));
        sail_int zgsz31048;
        CREATE(sail_int)(&zgsz31048);
        CONVERT_OF(sail_int, mach_int)(&zgsz31048, INT64_C(0));
        lbits zgsz31049;
        CREATE(lbits)(&zgsz31049);
        vector_subrange_lbits(&zgsz31049, zgsz31046, zgsz31047, zgsz31048);
        zgaz3295 = CONVERT_OF(fbits, lbits)(zgsz31049, true);
        KILL(lbits)(&zgsz31049);
        KILL(sail_int)(&zgsz31048);
        KILL(sail_int)(&zgsz31047);
        KILL(lbits)(&zgsz31046);
      }
      {
        lbits zgsz31044;
        CREATE(lbits)(&zgsz31044);
        CONVERT_OF(lbits, fbits)(&zgsz31044, zgaz3295, UINT64_C(7) , true);
        lbits zgsz31045;
        CREATE(lbits)(&zgsz31045);
        CONVERT_OF(lbits, fbits)(&zgsz31045, UINT64_C(0b0010011), UINT64_C(7) , true);
        zgsz3253 = eq_bits(zgsz31044, zgsz31045);
        KILL(lbits)(&zgsz31045);
        KILL(lbits)(&zgsz31044);
      }
    } else {  zgsz3253 = false;  }
    bool zgsz3255;
    zgsz3255 = zgsz3253;
    if (!(zgsz3255)) {

      goto case_223;
    }
    uint64_t zuz3122;
    {
      lbits zgsz31062;
      CREATE(lbits)(&zgsz31062);
      CONVERT_OF(lbits, fbits)(&zgsz31062, zv__84, UINT64_C(32) , true);
      sail_int zgsz31063;
      CREATE(sail_int)(&zgsz31063);
      CONVERT_OF(sail_int, mach_int)(&zgsz31063, INT64_C(31));
      sail_int zgsz31064;
      CREATE(sail_int)(&zgsz31064);
      CONVERT_OF(sail_int, mach_int)(&zgsz31064, INT64_C(20));
      lbits zgsz31065;
      CREATE(lbits)(&zgsz31065);
      vector_subrange_lbits(&zgsz31065, zgsz31062, zgsz31063, zgsz31064);
      zuz3122 = CONVERT_OF(fbits, lbits)(zgsz31065, true);
      KILL(lbits)(&zgsz31065);
      KILL(sail_int)(&zgsz31064);
      KILL(sail_int)(&zgsz31063);
      KILL(lbits)(&zgsz31062);
    }
    uint64_t zuz3123;
    {
      lbits zgsz31058;
      CREATE(lbits)(&zgsz31058);
      CONVERT_OF(lbits, fbits)(&zgsz31058, zv__84, UINT64_C(32) , true);
      sail_int zgsz31059;
      CREATE(sail_int)(&zgsz31059);
      CONVERT_OF(sail_int, mach_int)(&zgsz31059, INT64_C(19));
      sail_int zgsz31060;
      CREATE(sail_int)(&zgsz31060);
      CONVERT_OF(sail_int, mach_int)(&zgsz31060, INT64_C(15));
      lbits zgsz31061;
      CREATE(lbits)(&zgsz31061);
      vector_subrange_lbits(&zgsz31061, zgsz31058, zgsz31059, zgsz31060);
      zuz3123 = CONVERT_OF(fbits, lbits)(zgsz31061, true);
      KILL(lbits)(&zgsz31061);
      KILL(sail_int)(&zgsz31060);
      KILL(sail_int)(&zgsz31059);
      KILL(lbits)(&zgsz31058);
    }
    uint64_t zuz3124;
    {
      lbits zgsz31054;
      CREATE(lbits)(&zgsz31054);
      CONVERT_OF(lbits, fbits)(&zgsz31054, zv__84, UINT64_C(32) , true);
      sail_int zgsz31055;
      CREATE(sail_int)(&zgsz31055);
      CONVERT_OF(sail_int, mach_int)(&zgsz31055, INT64_C(11));
      sail_int zgsz31056;
      CREATE(sail_int)(&zgsz31056);
      CONVERT_OF(sail_int, mach_int)(&zgsz31056, INT64_C(7));
      lbits zgsz31057;
      CREATE(lbits)(&zgsz31057);
      vector_subrange_lbits(&zgsz31057, zgsz31054, zgsz31055, zgsz31056);
      zuz3124 = CONVERT_OF(fbits, lbits)(zgsz31057, true);
      KILL(lbits)(&zgsz31057);
      KILL(sail_int)(&zgsz31056);
      KILL(sail_int)(&zgsz31055);
      KILL(lbits)(&zgsz31054);
    }
    uint64_t zimmshadowz312;
    {
      lbits zgsz31050;
      CREATE(lbits)(&zgsz31050);
      CONVERT_OF(lbits, fbits)(&zgsz31050, zv__84, UINT64_C(32) , true);
      sail_int zgsz31051;
      CREATE(sail_int)(&zgsz31051);
      CONVERT_OF(sail_int, mach_int)(&zgsz31051, INT64_C(31));
      sail_int zgsz31052;
      CREATE(sail_int)(&zgsz31052);
      CONVERT_OF(sail_int, mach_int)(&zgsz31052, INT64_C(20));
      lbits zgsz31053;
      CREATE(lbits)(&zgsz31053);
      vector_subrange_lbits(&zgsz31053, zgsz31050, zgsz31051, zgsz31052);
      zimmshadowz312 = CONVERT_OF(fbits, lbits)(zgsz31053, true);
      KILL(lbits)(&zgsz31053);
      KILL(sail_int)(&zgsz31052);
      KILL(sail_int)(&zgsz31051);
      KILL(lbits)(&zgsz31050);
    }
    struct zast zgaz3293;
    CREATE(zast)(&zgaz3293);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3292;
      {
        uint64_t zgaz3290;
        zgaz3290 = ztREG(zuz3124);
        uint64_t zgaz3291;
        zgaz3291 = ztREG(zuz3123);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3254;
        zgsz3254.ztup0 = zgaz3290;
        zgsz3254.ztup1 = zgaz3291;
        zgsz3254.ztup2 = zimmshadowz312;
        zgaz3292 = zgsz3254;
      }
      zSLTI(&zgaz3293, zgaz3292);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3293);
    KILL(zast)(&zgaz3293);
    goto finish_match_202;
  }
case_223: ;
  {
    uint64_t zv__87;
    zv__87 = zmergez3var;
    bool zgaz3306;
    {
      uint64_t zgaz3302;
      {
        lbits zgsz31068;
        CREATE(lbits)(&zgsz31068);
        CONVERT_OF(lbits, fbits)(&zgsz31068, zv__87, UINT64_C(32) , true);
        sail_int zgsz31069;
        CREATE(sail_int)(&zgsz31069);
        CONVERT_OF(sail_int, mach_int)(&zgsz31069, INT64_C(31));
        sail_int zgsz31070;
        CREATE(sail_int)(&zgsz31070);
        CONVERT_OF(sail_int, mach_int)(&zgsz31070, INT64_C(25));
        lbits zgsz31071;
        CREATE(lbits)(&zgsz31071);
        vector_subrange_lbits(&zgsz31071, zgsz31068, zgsz31069, zgsz31070);
        zgaz3302 = CONVERT_OF(fbits, lbits)(zgsz31071, true);
        KILL(lbits)(&zgsz31071);
        KILL(sail_int)(&zgsz31070);
        KILL(sail_int)(&zgsz31069);
        KILL(lbits)(&zgsz31068);
      }
      {
        lbits zgsz31066;
        CREATE(lbits)(&zgsz31066);
        CONVERT_OF(lbits, fbits)(&zgsz31066, zgaz3302, UINT64_C(7) , true);
        lbits zgsz31067;
        CREATE(lbits)(&zgsz31067);
        CONVERT_OF(lbits, fbits)(&zgsz31067, UINT64_C(0b0000000), UINT64_C(7) , true);
        zgaz3306 = eq_bits(zgsz31066, zgsz31067);
        KILL(lbits)(&zgsz31067);
        KILL(lbits)(&zgsz31066);
      }
    }
    bool zgsz3257;
    if (zgaz3306) {
      bool zgaz3305;
      {
        uint64_t zgaz3303;
        {
          lbits zgsz31074;
          CREATE(lbits)(&zgsz31074);
          CONVERT_OF(lbits, fbits)(&zgsz31074, zv__87, UINT64_C(32) , true);
          sail_int zgsz31075;
          CREATE(sail_int)(&zgsz31075);
          CONVERT_OF(sail_int, mach_int)(&zgsz31075, INT64_C(14));
          sail_int zgsz31076;
          CREATE(sail_int)(&zgsz31076);
          CONVERT_OF(sail_int, mach_int)(&zgsz31076, INT64_C(12));
          lbits zgsz31077;
          CREATE(lbits)(&zgsz31077);
          vector_subrange_lbits(&zgsz31077, zgsz31074, zgsz31075, zgsz31076);
          zgaz3303 = CONVERT_OF(fbits, lbits)(zgsz31077, true);
          KILL(lbits)(&zgsz31077);
          KILL(sail_int)(&zgsz31076);
          KILL(sail_int)(&zgsz31075);
          KILL(lbits)(&zgsz31074);
        }
        {
          lbits zgsz31072;
          CREATE(lbits)(&zgsz31072);
          CONVERT_OF(lbits, fbits)(&zgsz31072, zgaz3303, UINT64_C(3) , true);
          lbits zgsz31073;
          CREATE(lbits)(&zgsz31073);
          CONVERT_OF(lbits, fbits)(&zgsz31073, UINT64_C(0b011), UINT64_C(3) , true);
          zgaz3305 = eq_bits(zgsz31072, zgsz31073);
          KILL(lbits)(&zgsz31073);
          KILL(lbits)(&zgsz31072);
        }
      }
      bool zgsz3256;
      if (zgaz3305) {
        uint64_t zgaz3304;
        {
          lbits zgsz31080;
          CREATE(lbits)(&zgsz31080);
          CONVERT_OF(lbits, fbits)(&zgsz31080, zv__87, UINT64_C(32) , true);
          sail_int zgsz31081;
          CREATE(sail_int)(&zgsz31081);
          CONVERT_OF(sail_int, mach_int)(&zgsz31081, INT64_C(6));
          sail_int zgsz31082;
          CREATE(sail_int)(&zgsz31082);
          CONVERT_OF(sail_int, mach_int)(&zgsz31082, INT64_C(0));
          lbits zgsz31083;
          CREATE(lbits)(&zgsz31083);
          vector_subrange_lbits(&zgsz31083, zgsz31080, zgsz31081, zgsz31082);
          zgaz3304 = CONVERT_OF(fbits, lbits)(zgsz31083, true);
          KILL(lbits)(&zgsz31083);
          KILL(sail_int)(&zgsz31082);
          KILL(sail_int)(&zgsz31081);
          KILL(lbits)(&zgsz31080);
        }
        {
          lbits zgsz31078;
          CREATE(lbits)(&zgsz31078);
          CONVERT_OF(lbits, fbits)(&zgsz31078, zgaz3304, UINT64_C(7) , true);
          lbits zgsz31079;
          CREATE(lbits)(&zgsz31079);
          CONVERT_OF(lbits, fbits)(&zgsz31079, UINT64_C(0b0110011), UINT64_C(7) , true);
          zgsz3256 = eq_bits(zgsz31078, zgsz31079);
          KILL(lbits)(&zgsz31079);
          KILL(lbits)(&zgsz31078);
        }
      } else {  zgsz3256 = false;  }
      zgsz3257 = zgsz3256;
    } else {  zgsz3257 = false;  }
    bool zgsz3259;
    zgsz3259 = zgsz3257;
    if (!(zgsz3259)) {

      goto case_224;
    }
    uint64_t zuz3125;
    {
      lbits zgsz31092;
      CREATE(lbits)(&zgsz31092);
      CONVERT_OF(lbits, fbits)(&zgsz31092, zv__87, UINT64_C(32) , true);
      sail_int zgsz31093;
      CREATE(sail_int)(&zgsz31093);
      CONVERT_OF(sail_int, mach_int)(&zgsz31093, INT64_C(19));
      sail_int zgsz31094;
      CREATE(sail_int)(&zgsz31094);
      CONVERT_OF(sail_int, mach_int)(&zgsz31094, INT64_C(15));
      lbits zgsz31095;
      CREATE(lbits)(&zgsz31095);
      vector_subrange_lbits(&zgsz31095, zgsz31092, zgsz31093, zgsz31094);
      zuz3125 = CONVERT_OF(fbits, lbits)(zgsz31095, true);
      KILL(lbits)(&zgsz31095);
      KILL(sail_int)(&zgsz31094);
      KILL(sail_int)(&zgsz31093);
      KILL(lbits)(&zgsz31092);
    }
    uint64_t zuz3126;
    {
      lbits zgsz31088;
      CREATE(lbits)(&zgsz31088);
      CONVERT_OF(lbits, fbits)(&zgsz31088, zv__87, UINT64_C(32) , true);
      sail_int zgsz31089;
      CREATE(sail_int)(&zgsz31089);
      CONVERT_OF(sail_int, mach_int)(&zgsz31089, INT64_C(11));
      sail_int zgsz31090;
      CREATE(sail_int)(&zgsz31090);
      CONVERT_OF(sail_int, mach_int)(&zgsz31090, INT64_C(7));
      lbits zgsz31091;
      CREATE(lbits)(&zgsz31091);
      vector_subrange_lbits(&zgsz31091, zgsz31088, zgsz31089, zgsz31090);
      zuz3126 = CONVERT_OF(fbits, lbits)(zgsz31091, true);
      KILL(lbits)(&zgsz31091);
      KILL(sail_int)(&zgsz31090);
      KILL(sail_int)(&zgsz31089);
      KILL(lbits)(&zgsz31088);
    }
    uint64_t zuz3127;
    {
      lbits zgsz31084;
      CREATE(lbits)(&zgsz31084);
      CONVERT_OF(lbits, fbits)(&zgsz31084, zv__87, UINT64_C(32) , true);
      sail_int zgsz31085;
      CREATE(sail_int)(&zgsz31085);
      CONVERT_OF(sail_int, mach_int)(&zgsz31085, INT64_C(24));
      sail_int zgsz31086;
      CREATE(sail_int)(&zgsz31086);
      CONVERT_OF(sail_int, mach_int)(&zgsz31086, INT64_C(20));
      lbits zgsz31087;
      CREATE(lbits)(&zgsz31087);
      vector_subrange_lbits(&zgsz31087, zgsz31084, zgsz31085, zgsz31086);
      zuz3127 = CONVERT_OF(fbits, lbits)(zgsz31087, true);
      KILL(lbits)(&zgsz31087);
      KILL(sail_int)(&zgsz31086);
      KILL(sail_int)(&zgsz31085);
      KILL(lbits)(&zgsz31084);
    }
    struct zast zgaz3301;
    CREATE(zast)(&zgaz3301);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3300;
      {
        uint64_t zgaz3297;
        zgaz3297 = ztREG(zuz3126);
        uint64_t zgaz3298;
        zgaz3298 = ztREG(zuz3125);
        uint64_t zgaz3299;
        zgaz3299 = ztREG(zuz3127);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3258;
        zgsz3258.ztup0 = zgaz3297;
        zgsz3258.ztup1 = zgaz3298;
        zgsz3258.ztup2 = zgaz3299;
        zgaz3300 = zgsz3258;
      }
      zSLTU(&zgaz3301, zgaz3300);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3301);
    KILL(zast)(&zgaz3301);
    goto finish_match_202;
  }
case_224: ;
  {
    uint64_t zv__91;
    zv__91 = zmergez3var;
    bool zgaz3313;
    {
      uint64_t zgaz3311;
      {
        lbits zgsz31098;
        CREATE(lbits)(&zgsz31098);
        CONVERT_OF(lbits, fbits)(&zgsz31098, zv__91, UINT64_C(32) , true);
        sail_int zgsz31099;
        CREATE(sail_int)(&zgsz31099);
        CONVERT_OF(sail_int, mach_int)(&zgsz31099, INT64_C(14));
        sail_int zgsz31100;
        CREATE(sail_int)(&zgsz31100);
        CONVERT_OF(sail_int, mach_int)(&zgsz31100, INT64_C(12));
        lbits zgsz31101;
        CREATE(lbits)(&zgsz31101);
        vector_subrange_lbits(&zgsz31101, zgsz31098, zgsz31099, zgsz31100);
        zgaz3311 = CONVERT_OF(fbits, lbits)(zgsz31101, true);
        KILL(lbits)(&zgsz31101);
        KILL(sail_int)(&zgsz31100);
        KILL(sail_int)(&zgsz31099);
        KILL(lbits)(&zgsz31098);
      }
      {
        lbits zgsz31096;
        CREATE(lbits)(&zgsz31096);
        CONVERT_OF(lbits, fbits)(&zgsz31096, zgaz3311, UINT64_C(3) , true);
        lbits zgsz31097;
        CREATE(lbits)(&zgsz31097);
        CONVERT_OF(lbits, fbits)(&zgsz31097, UINT64_C(0b011), UINT64_C(3) , true);
        zgaz3313 = eq_bits(zgsz31096, zgsz31097);
        KILL(lbits)(&zgsz31097);
        KILL(lbits)(&zgsz31096);
      }
    }
    bool zgsz3260;
    if (zgaz3313) {
      uint64_t zgaz3312;
      {
        lbits zgsz31104;
        CREATE(lbits)(&zgsz31104);
        CONVERT_OF(lbits, fbits)(&zgsz31104, zv__91, UINT64_C(32) , true);
        sail_int zgsz31105;
        CREATE(sail_int)(&zgsz31105);
        CONVERT_OF(sail_int, mach_int)(&zgsz31105, INT64_C(6));
        sail_int zgsz31106;
        CREATE(sail_int)(&zgsz31106);
        CONVERT_OF(sail_int, mach_int)(&zgsz31106, INT64_C(0));
        lbits zgsz31107;
        CREATE(lbits)(&zgsz31107);
        vector_subrange_lbits(&zgsz31107, zgsz31104, zgsz31105, zgsz31106);
        zgaz3312 = CONVERT_OF(fbits, lbits)(zgsz31107, true);
        KILL(lbits)(&zgsz31107);
        KILL(sail_int)(&zgsz31106);
        KILL(sail_int)(&zgsz31105);
        KILL(lbits)(&zgsz31104);
      }
      {
        lbits zgsz31102;
        CREATE(lbits)(&zgsz31102);
        CONVERT_OF(lbits, fbits)(&zgsz31102, zgaz3312, UINT64_C(7) , true);
        lbits zgsz31103;
        CREATE(lbits)(&zgsz31103);
        CONVERT_OF(lbits, fbits)(&zgsz31103, UINT64_C(0b0010011), UINT64_C(7) , true);
        zgsz3260 = eq_bits(zgsz31102, zgsz31103);
        KILL(lbits)(&zgsz31103);
        KILL(lbits)(&zgsz31102);
      }
    } else {  zgsz3260 = false;  }
    bool zgsz3262;
    zgsz3262 = zgsz3260;
    if (!(zgsz3262)) {

      goto case_225;
    }
    uint64_t zuz3128;
    {
      lbits zgsz31120;
      CREATE(lbits)(&zgsz31120);
      CONVERT_OF(lbits, fbits)(&zgsz31120, zv__91, UINT64_C(32) , true);
      sail_int zgsz31121;
      CREATE(sail_int)(&zgsz31121);
      CONVERT_OF(sail_int, mach_int)(&zgsz31121, INT64_C(31));
      sail_int zgsz31122;
      CREATE(sail_int)(&zgsz31122);
      CONVERT_OF(sail_int, mach_int)(&zgsz31122, INT64_C(20));
      lbits zgsz31123;
      CREATE(lbits)(&zgsz31123);
      vector_subrange_lbits(&zgsz31123, zgsz31120, zgsz31121, zgsz31122);
      zuz3128 = CONVERT_OF(fbits, lbits)(zgsz31123, true);
      KILL(lbits)(&zgsz31123);
      KILL(sail_int)(&zgsz31122);
      KILL(sail_int)(&zgsz31121);
      KILL(lbits)(&zgsz31120);
    }
    uint64_t zuz3129;
    {
      lbits zgsz31116;
      CREATE(lbits)(&zgsz31116);
      CONVERT_OF(lbits, fbits)(&zgsz31116, zv__91, UINT64_C(32) , true);
      sail_int zgsz31117;
      CREATE(sail_int)(&zgsz31117);
      CONVERT_OF(sail_int, mach_int)(&zgsz31117, INT64_C(19));
      sail_int zgsz31118;
      CREATE(sail_int)(&zgsz31118);
      CONVERT_OF(sail_int, mach_int)(&zgsz31118, INT64_C(15));
      lbits zgsz31119;
      CREATE(lbits)(&zgsz31119);
      vector_subrange_lbits(&zgsz31119, zgsz31116, zgsz31117, zgsz31118);
      zuz3129 = CONVERT_OF(fbits, lbits)(zgsz31119, true);
      KILL(lbits)(&zgsz31119);
      KILL(sail_int)(&zgsz31118);
      KILL(sail_int)(&zgsz31117);
      KILL(lbits)(&zgsz31116);
    }
    uint64_t zuz3130;
    {
      lbits zgsz31112;
      CREATE(lbits)(&zgsz31112);
      CONVERT_OF(lbits, fbits)(&zgsz31112, zv__91, UINT64_C(32) , true);
      sail_int zgsz31113;
      CREATE(sail_int)(&zgsz31113);
      CONVERT_OF(sail_int, mach_int)(&zgsz31113, INT64_C(11));
      sail_int zgsz31114;
      CREATE(sail_int)(&zgsz31114);
      CONVERT_OF(sail_int, mach_int)(&zgsz31114, INT64_C(7));
      lbits zgsz31115;
      CREATE(lbits)(&zgsz31115);
      vector_subrange_lbits(&zgsz31115, zgsz31112, zgsz31113, zgsz31114);
      zuz3130 = CONVERT_OF(fbits, lbits)(zgsz31115, true);
      KILL(lbits)(&zgsz31115);
      KILL(sail_int)(&zgsz31114);
      KILL(sail_int)(&zgsz31113);
      KILL(lbits)(&zgsz31112);
    }
    uint64_t zimmshadowz313;
    {
      lbits zgsz31108;
      CREATE(lbits)(&zgsz31108);
      CONVERT_OF(lbits, fbits)(&zgsz31108, zv__91, UINT64_C(32) , true);
      sail_int zgsz31109;
      CREATE(sail_int)(&zgsz31109);
      CONVERT_OF(sail_int, mach_int)(&zgsz31109, INT64_C(31));
      sail_int zgsz31110;
      CREATE(sail_int)(&zgsz31110);
      CONVERT_OF(sail_int, mach_int)(&zgsz31110, INT64_C(20));
      lbits zgsz31111;
      CREATE(lbits)(&zgsz31111);
      vector_subrange_lbits(&zgsz31111, zgsz31108, zgsz31109, zgsz31110);
      zimmshadowz313 = CONVERT_OF(fbits, lbits)(zgsz31111, true);
      KILL(lbits)(&zgsz31111);
      KILL(sail_int)(&zgsz31110);
      KILL(sail_int)(&zgsz31109);
      KILL(lbits)(&zgsz31108);
    }
    struct zast zgaz3310;
    CREATE(zast)(&zgaz3310);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3309;
      {
        uint64_t zgaz3307;
        zgaz3307 = ztREG(zuz3130);
        uint64_t zgaz3308;
        zgaz3308 = ztREG(zuz3129);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3261;
        zgsz3261.ztup0 = zgaz3307;
        zgsz3261.ztup1 = zgaz3308;
        zgsz3261.ztup2 = zimmshadowz313;
        zgaz3309 = zgsz3261;
      }
      zSLTIU(&zgaz3310, zgaz3309);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3310);
    KILL(zast)(&zgaz3310);
    goto finish_match_202;
  }
case_225: ;
  {
    uint64_t zv__94;
    zv__94 = zmergez3var;
    bool zgaz3323;
    {
      uint64_t zgaz3319;
      {
        lbits zgsz31126;
        CREATE(lbits)(&zgsz31126);
        CONVERT_OF(lbits, fbits)(&zgsz31126, zv__94, UINT64_C(32) , true);
        sail_int zgsz31127;
        CREATE(sail_int)(&zgsz31127);
        CONVERT_OF(sail_int, mach_int)(&zgsz31127, INT64_C(31));
        sail_int zgsz31128;
        CREATE(sail_int)(&zgsz31128);
        CONVERT_OF(sail_int, mach_int)(&zgsz31128, INT64_C(25));
        lbits zgsz31129;
        CREATE(lbits)(&zgsz31129);
        vector_subrange_lbits(&zgsz31129, zgsz31126, zgsz31127, zgsz31128);
        zgaz3319 = CONVERT_OF(fbits, lbits)(zgsz31129, true);
        KILL(lbits)(&zgsz31129);
        KILL(sail_int)(&zgsz31128);
        KILL(sail_int)(&zgsz31127);
        KILL(lbits)(&zgsz31126);
      }
      {
        lbits zgsz31124;
        CREATE(lbits)(&zgsz31124);
        CONVERT_OF(lbits, fbits)(&zgsz31124, zgaz3319, UINT64_C(7) , true);
        lbits zgsz31125;
        CREATE(lbits)(&zgsz31125);
        CONVERT_OF(lbits, fbits)(&zgsz31125, UINT64_C(0b0001000), UINT64_C(7) , true);
        zgaz3323 = eq_bits(zgsz31124, zgsz31125);
        KILL(lbits)(&zgsz31125);
        KILL(lbits)(&zgsz31124);
      }
    }
    bool zgsz3264;
    if (zgaz3323) {
      bool zgaz3322;
      {
        uint64_t zgaz3320;
        {
          lbits zgsz31132;
          CREATE(lbits)(&zgsz31132);
          CONVERT_OF(lbits, fbits)(&zgsz31132, zv__94, UINT64_C(32) , true);
          sail_int zgsz31133;
          CREATE(sail_int)(&zgsz31133);
          CONVERT_OF(sail_int, mach_int)(&zgsz31133, INT64_C(14));
          sail_int zgsz31134;
          CREATE(sail_int)(&zgsz31134);
          CONVERT_OF(sail_int, mach_int)(&zgsz31134, INT64_C(12));
          lbits zgsz31135;
          CREATE(lbits)(&zgsz31135);
          vector_subrange_lbits(&zgsz31135, zgsz31132, zgsz31133, zgsz31134);
          zgaz3320 = CONVERT_OF(fbits, lbits)(zgsz31135, true);
          KILL(lbits)(&zgsz31135);
          KILL(sail_int)(&zgsz31134);
          KILL(sail_int)(&zgsz31133);
          KILL(lbits)(&zgsz31132);
        }
        {
          lbits zgsz31130;
          CREATE(lbits)(&zgsz31130);
          CONVERT_OF(lbits, fbits)(&zgsz31130, zgaz3320, UINT64_C(3) , true);
          lbits zgsz31131;
          CREATE(lbits)(&zgsz31131);
          CONVERT_OF(lbits, fbits)(&zgsz31131, UINT64_C(0b000), UINT64_C(3) , true);
          zgaz3322 = eq_bits(zgsz31130, zgsz31131);
          KILL(lbits)(&zgsz31131);
          KILL(lbits)(&zgsz31130);
        }
      }
      bool zgsz3263;
      if (zgaz3322) {
        uint64_t zgaz3321;
        {
          lbits zgsz31138;
          CREATE(lbits)(&zgsz31138);
          CONVERT_OF(lbits, fbits)(&zgsz31138, zv__94, UINT64_C(32) , true);
          sail_int zgsz31139;
          CREATE(sail_int)(&zgsz31139);
          CONVERT_OF(sail_int, mach_int)(&zgsz31139, INT64_C(6));
          sail_int zgsz31140;
          CREATE(sail_int)(&zgsz31140);
          CONVERT_OF(sail_int, mach_int)(&zgsz31140, INT64_C(0));
          lbits zgsz31141;
          CREATE(lbits)(&zgsz31141);
          vector_subrange_lbits(&zgsz31141, zgsz31138, zgsz31139, zgsz31140);
          zgaz3321 = CONVERT_OF(fbits, lbits)(zgsz31141, true);
          KILL(lbits)(&zgsz31141);
          KILL(sail_int)(&zgsz31140);
          KILL(sail_int)(&zgsz31139);
          KILL(lbits)(&zgsz31138);
        }
        {
          lbits zgsz31136;
          CREATE(lbits)(&zgsz31136);
          CONVERT_OF(lbits, fbits)(&zgsz31136, zgaz3321, UINT64_C(7) , true);
          lbits zgsz31137;
          CREATE(lbits)(&zgsz31137);
          CONVERT_OF(lbits, fbits)(&zgsz31137, UINT64_C(0b1011011), UINT64_C(7) , true);
          zgsz3263 = eq_bits(zgsz31136, zgsz31137);
          KILL(lbits)(&zgsz31137);
          KILL(lbits)(&zgsz31136);
        }
      } else {  zgsz3263 = false;  }
      zgsz3264 = zgsz3263;
    } else {  zgsz3264 = false;  }
    bool zgsz3266;
    zgsz3266 = zgsz3264;
    if (!(zgsz3266)) {

      goto case_226;
    }
    uint64_t zuz3131;
    {
      lbits zgsz31150;
      CREATE(lbits)(&zgsz31150);
      CONVERT_OF(lbits, fbits)(&zgsz31150, zv__94, UINT64_C(32) , true);
      sail_int zgsz31151;
      CREATE(sail_int)(&zgsz31151);
      CONVERT_OF(sail_int, mach_int)(&zgsz31151, INT64_C(24));
      sail_int zgsz31152;
      CREATE(sail_int)(&zgsz31152);
      CONVERT_OF(sail_int, mach_int)(&zgsz31152, INT64_C(20));
      lbits zgsz31153;
      CREATE(lbits)(&zgsz31153);
      vector_subrange_lbits(&zgsz31153, zgsz31150, zgsz31151, zgsz31152);
      zuz3131 = CONVERT_OF(fbits, lbits)(zgsz31153, true);
      KILL(lbits)(&zgsz31153);
      KILL(sail_int)(&zgsz31152);
      KILL(sail_int)(&zgsz31151);
      KILL(lbits)(&zgsz31150);
    }
    uint64_t zuz3132;
    {
      lbits zgsz31146;
      CREATE(lbits)(&zgsz31146);
      CONVERT_OF(lbits, fbits)(&zgsz31146, zv__94, UINT64_C(32) , true);
      sail_int zgsz31147;
      CREATE(sail_int)(&zgsz31147);
      CONVERT_OF(sail_int, mach_int)(&zgsz31147, INT64_C(19));
      sail_int zgsz31148;
      CREATE(sail_int)(&zgsz31148);
      CONVERT_OF(sail_int, mach_int)(&zgsz31148, INT64_C(15));
      lbits zgsz31149;
      CREATE(lbits)(&zgsz31149);
      vector_subrange_lbits(&zgsz31149, zgsz31146, zgsz31147, zgsz31148);
      zuz3132 = CONVERT_OF(fbits, lbits)(zgsz31149, true);
      KILL(lbits)(&zgsz31149);
      KILL(sail_int)(&zgsz31148);
      KILL(sail_int)(&zgsz31147);
      KILL(lbits)(&zgsz31146);
    }
    uint64_t zuz3133;
    {
      lbits zgsz31142;
      CREATE(lbits)(&zgsz31142);
      CONVERT_OF(lbits, fbits)(&zgsz31142, zv__94, UINT64_C(32) , true);
      sail_int zgsz31143;
      CREATE(sail_int)(&zgsz31143);
      CONVERT_OF(sail_int, mach_int)(&zgsz31143, INT64_C(11));
      sail_int zgsz31144;
      CREATE(sail_int)(&zgsz31144);
      CONVERT_OF(sail_int, mach_int)(&zgsz31144, INT64_C(7));
      lbits zgsz31145;
      CREATE(lbits)(&zgsz31145);
      vector_subrange_lbits(&zgsz31145, zgsz31142, zgsz31143, zgsz31144);
      zuz3133 = CONVERT_OF(fbits, lbits)(zgsz31145, true);
      KILL(lbits)(&zgsz31145);
      KILL(sail_int)(&zgsz31144);
      KILL(sail_int)(&zgsz31143);
      KILL(lbits)(&zgsz31142);
    }
    struct zast zgaz3318;
    CREATE(zast)(&zgaz3318);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgaz3317;
      {
        uint64_t zgaz3314;
        zgaz3314 = ztREG(zuz3133);
        uint64_t zgaz3315;
        zgaz3315 = ztREG(zuz3132);
        uint64_t zgaz3316;
        zgaz3316 = ztREG(zuz3131);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv2z9 zgsz3265;
        zgsz3265.ztup0 = zgaz3314;
        zgsz3265.ztup1 = zgaz3315;
        zgsz3265.ztup2 = zgaz3316;
        zgaz3317 = zgsz3265;
      }
      zCSETBOUNDS(&zgaz3318, zgaz3317);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3318);
    KILL(zast)(&zgaz3318);
    goto finish_match_202;
  }
case_226: ;
  {
    uint64_t zv__98;
    zv__98 = zmergez3var;
    bool zgaz3330;
    {
      uint64_t zgaz3328;
      {
        lbits zgsz31156;
        CREATE(lbits)(&zgsz31156);
        CONVERT_OF(lbits, fbits)(&zgsz31156, zv__98, UINT64_C(32) , true);
        sail_int zgsz31157;
        CREATE(sail_int)(&zgsz31157);
        CONVERT_OF(sail_int, mach_int)(&zgsz31157, INT64_C(14));
        sail_int zgsz31158;
        CREATE(sail_int)(&zgsz31158);
        CONVERT_OF(sail_int, mach_int)(&zgsz31158, INT64_C(12));
        lbits zgsz31159;
        CREATE(lbits)(&zgsz31159);
        vector_subrange_lbits(&zgsz31159, zgsz31156, zgsz31157, zgsz31158);
        zgaz3328 = CONVERT_OF(fbits, lbits)(zgsz31159, true);
        KILL(lbits)(&zgsz31159);
        KILL(sail_int)(&zgsz31158);
        KILL(sail_int)(&zgsz31157);
        KILL(lbits)(&zgsz31156);
      }
      {
        lbits zgsz31154;
        CREATE(lbits)(&zgsz31154);
        CONVERT_OF(lbits, fbits)(&zgsz31154, zgaz3328, UINT64_C(3) , true);
        lbits zgsz31155;
        CREATE(lbits)(&zgsz31155);
        CONVERT_OF(lbits, fbits)(&zgsz31155, UINT64_C(0b010), UINT64_C(3) , true);
        zgaz3330 = eq_bits(zgsz31154, zgsz31155);
        KILL(lbits)(&zgsz31155);
        KILL(lbits)(&zgsz31154);
      }
    }
    bool zgsz3267;
    if (zgaz3330) {
      uint64_t zgaz3329;
      {
        lbits zgsz31162;
        CREATE(lbits)(&zgsz31162);
        CONVERT_OF(lbits, fbits)(&zgsz31162, zv__98, UINT64_C(32) , true);
        sail_int zgsz31163;
        CREATE(sail_int)(&zgsz31163);
        CONVERT_OF(sail_int, mach_int)(&zgsz31163, INT64_C(6));
        sail_int zgsz31164;
        CREATE(sail_int)(&zgsz31164);
        CONVERT_OF(sail_int, mach_int)(&zgsz31164, INT64_C(0));
        lbits zgsz31165;
        CREATE(lbits)(&zgsz31165);
        vector_subrange_lbits(&zgsz31165, zgsz31162, zgsz31163, zgsz31164);
        zgaz3329 = CONVERT_OF(fbits, lbits)(zgsz31165, true);
        KILL(lbits)(&zgsz31165);
        KILL(sail_int)(&zgsz31164);
        KILL(sail_int)(&zgsz31163);
        KILL(lbits)(&zgsz31162);
      }
      {
        lbits zgsz31160;
        CREATE(lbits)(&zgsz31160);
        CONVERT_OF(lbits, fbits)(&zgsz31160, zgaz3329, UINT64_C(7) , true);
        lbits zgsz31161;
        CREATE(lbits)(&zgsz31161);
        CONVERT_OF(lbits, fbits)(&zgsz31161, UINT64_C(0b1011011), UINT64_C(7) , true);
        zgsz3267 = eq_bits(zgsz31160, zgsz31161);
        KILL(lbits)(&zgsz31161);
        KILL(lbits)(&zgsz31160);
      }
    } else {  zgsz3267 = false;  }
    bool zgsz3269;
    zgsz3269 = zgsz3267;
    if (!(zgsz3269)) {

      goto case_227;
    }
    uint64_t zuz3134;
    {
      lbits zgsz31178;
      CREATE(lbits)(&zgsz31178);
      CONVERT_OF(lbits, fbits)(&zgsz31178, zv__98, UINT64_C(32) , true);
      sail_int zgsz31179;
      CREATE(sail_int)(&zgsz31179);
      CONVERT_OF(sail_int, mach_int)(&zgsz31179, INT64_C(31));
      sail_int zgsz31180;
      CREATE(sail_int)(&zgsz31180);
      CONVERT_OF(sail_int, mach_int)(&zgsz31180, INT64_C(20));
      lbits zgsz31181;
      CREATE(lbits)(&zgsz31181);
      vector_subrange_lbits(&zgsz31181, zgsz31178, zgsz31179, zgsz31180);
      zuz3134 = CONVERT_OF(fbits, lbits)(zgsz31181, true);
      KILL(lbits)(&zgsz31181);
      KILL(sail_int)(&zgsz31180);
      KILL(sail_int)(&zgsz31179);
      KILL(lbits)(&zgsz31178);
    }
    uint64_t zimmshadowz314;
    {
      lbits zgsz31174;
      CREATE(lbits)(&zgsz31174);
      CONVERT_OF(lbits, fbits)(&zgsz31174, zv__98, UINT64_C(32) , true);
      sail_int zgsz31175;
      CREATE(sail_int)(&zgsz31175);
      CONVERT_OF(sail_int, mach_int)(&zgsz31175, INT64_C(31));
      sail_int zgsz31176;
      CREATE(sail_int)(&zgsz31176);
      CONVERT_OF(sail_int, mach_int)(&zgsz31176, INT64_C(20));
      lbits zgsz31177;
      CREATE(lbits)(&zgsz31177);
      vector_subrange_lbits(&zgsz31177, zgsz31174, zgsz31175, zgsz31176);
      zimmshadowz314 = CONVERT_OF(fbits, lbits)(zgsz31177, true);
      KILL(lbits)(&zgsz31177);
      KILL(sail_int)(&zgsz31176);
      KILL(sail_int)(&zgsz31175);
      KILL(lbits)(&zgsz31174);
    }
    uint64_t zuz3135;
    {
      lbits zgsz31170;
      CREATE(lbits)(&zgsz31170);
      CONVERT_OF(lbits, fbits)(&zgsz31170, zv__98, UINT64_C(32) , true);
      sail_int zgsz31171;
      CREATE(sail_int)(&zgsz31171);
      CONVERT_OF(sail_int, mach_int)(&zgsz31171, INT64_C(19));
      sail_int zgsz31172;
      CREATE(sail_int)(&zgsz31172);
      CONVERT_OF(sail_int, mach_int)(&zgsz31172, INT64_C(15));
      lbits zgsz31173;
      CREATE(lbits)(&zgsz31173);
      vector_subrange_lbits(&zgsz31173, zgsz31170, zgsz31171, zgsz31172);
      zuz3135 = CONVERT_OF(fbits, lbits)(zgsz31173, true);
      KILL(lbits)(&zgsz31173);
      KILL(sail_int)(&zgsz31172);
      KILL(sail_int)(&zgsz31171);
      KILL(lbits)(&zgsz31170);
    }
    uint64_t zuz3136;
    {
      lbits zgsz31166;
      CREATE(lbits)(&zgsz31166);
      CONVERT_OF(lbits, fbits)(&zgsz31166, zv__98, UINT64_C(32) , true);
      sail_int zgsz31167;
      CREATE(sail_int)(&zgsz31167);
      CONVERT_OF(sail_int, mach_int)(&zgsz31167, INT64_C(11));
      sail_int zgsz31168;
      CREATE(sail_int)(&zgsz31168);
      CONVERT_OF(sail_int, mach_int)(&zgsz31168, INT64_C(7));
      lbits zgsz31169;
      CREATE(lbits)(&zgsz31169);
      vector_subrange_lbits(&zgsz31169, zgsz31166, zgsz31167, zgsz31168);
      zuz3136 = CONVERT_OF(fbits, lbits)(zgsz31169, true);
      KILL(lbits)(&zgsz31169);
      KILL(sail_int)(&zgsz31168);
      KILL(sail_int)(&zgsz31167);
      KILL(lbits)(&zgsz31166);
    }
    struct zast zgaz3327;
    CREATE(zast)(&zgaz3327);
    {
      struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgaz3326;
      {
        uint64_t zgaz3324;
        zgaz3324 = ztREG(zuz3136);
        uint64_t zgaz3325;
        zgaz3325 = ztREG(zuz3135);
        struct ztuple_z8z5bv2zCz0z5bv2zCz0z5bv12z9 zgsz3268;
        zgsz3268.ztup0 = zgaz3324;
        zgsz3268.ztup1 = zgaz3325;
        zgsz3268.ztup2 = zimmshadowz314;
        zgaz3326 = zgsz3268;
      }
      zCSETBOUNDSIMM(&zgaz3327, zgaz3326);
    }
    zSomezIUastzIzKzK(&zgsz3185, zgaz3327);
    KILL(zast)(&zgaz3327);
    goto finish_match_202;
  }
case_227: ;
  {
    uint64_t znot_supported;
    znot_supported = zmergez3var;
    zNonezIUastzIzKzK(&zgsz3185, UNIT);
    goto finish_match_202;
  }
case_228: ;
  sail_match_failure("decode");
finish_match_202: ;
  COPY(zoptionzIUastzIzKzK)((*(&zcbz330)), zgsz3185);
  KILL(zoptionzIUastzIzKzK)(&zgsz3185);
end_function_229: ;
  goto end_function_254;
end_block_exception_230: ;
  goto end_function_254;
end_function_254: ;
}

// register instr_count
sail_int zinstr_count;

unit zprint_register_dump(unit);

unit zprint_register_dump(unit zgsz3271)
{
  __label__ end_function_232, end_block_exception_233;

  unit zcbz331;
  zcbz331 = UNIT;
end_function_232: ;
  return zcbz331;
end_block_exception_233: ;

  return UNIT;
}



bool zcycle_limit_reached(unit zgsz3272)
{
  __label__ end_function_235, end_block_exception_236;

  bool zcbz332;
  zcbz332 = false;
end_function_235: ;
  return zcbz332;
end_block_exception_236: ;

  return false;
}



unit zinit_registers(uint64_t);

unit zinit_registers(uint64_t zinitialPC)
{
  __label__ end_function_238, end_block_exception_239;

  unit zcbz333;
  {
    zPC = zdefault_capability;
    unit zgsz3274;
    zgsz3274 = UNIT;
  }
  {
    zPC.zcap_cursor = zinitialPC;
    unit zgsz3273;
    zgsz3273 = UNIT;
  }
  zCap(&zR0, zdefault_capability);
  zcbz333 = UNIT;
end_function_238: ;
  return zcbz333;
end_block_exception_239: ;

  return UNIT;
}

bool zfetch_and_execute(unit);














































bool zfetch_and_execute(unit zgsz3275)
{
  __label__ end_function_244, end_block_exception_245;

  bool zcbz334;
  bool zgaz3333;
  {
    bool zgaz3332;
    {
      enum zPermission zgaz3331;
      zgaz3331 = zPC.zcap_permission;
      zgaz3332 = zreadAllowed(zgaz3331);
    }
    bool zgsz3276;
    if (zgaz3332) {  zgsz3276 = zwithinBounds(zPC);  } else {  zgsz3276 = false;  }
    zgaz3333 = zgsz3276;
  }
  if (zgaz3333) {
    uint64_t zinstr;
    {
      uint64_t zgaz3337;
      zgaz3337 = zPC.zcap_cursor;
      zinstr = zMEMri(zgaz3337);
    }
    {
      {
        sail_int zgsz31182;
        CREATE(sail_int)(&zgsz31182);
        CONVERT_OF(sail_int, mach_int)(&zgsz31182, INT64_C(1));
        add_int(&zinstr_count, zinstr_count, zgsz31182);
        KILL(sail_int)(&zgsz31182);
      }
      unit zgsz3279;
      zgsz3279 = UNIT;
    }
    {
      uint64_t zgaz3334;
      zgaz3334 = zPC.zcap_cursor;
      unit zgsz3278;
      {
        lbits zgsz31183;
        CREATE(lbits)(&zgsz31183);
        CONVERT_OF(lbits, fbits)(&zgsz31183, zgaz3334, UINT64_C(64) , true);
        zgsz3278 = print_bits("pc: ", zgsz31183);
        KILL(lbits)(&zgsz31183);
      }
    }
    {
      unit zgsz3277;
      {
        lbits zgsz31184;
        CREATE(lbits)(&zgsz31184);
        CONVERT_OF(lbits, fbits)(&zgsz31184, zinstr, UINT64_C(32) , true);
        zgsz3277 = print_bits("instr: ", zgsz31184);
        KILL(lbits)(&zgsz31184);
      }
    }
    bool zloop_again;
    {
      __label__ case_242, case_243, finish_match_241;

      struct zoptionzIUastzIzKzK zgaz3336;
      CREATE(zoptionzIUastzIzKzK)(&zgaz3336);
      zdecode(&zgaz3336, zinstr);
      /* Case with num_cases: 2 */
      bool zgsz3280;
      {
        if (zgaz3336.kind != Kind_zSomezIUastzIzKzK) goto case_242;
        struct zast zast;
        CREATE(zast)(&zast);
        COPY(zast)(&zast, zgaz3336.zSomezIUastzIzKzK);
        {
          zgsz3280 = zexecute(zast);
          if (have_exception) {



            KILL(zoptionzIUastzIzKzK)(&zgaz3336);

            KILL(zast)(&zast);
            goto end_block_exception_245;
          }
        }
        KILL(zast)(&zast);
        goto finish_match_241;
      }
    case_242: ;
      {
        if (zgaz3336.kind != Kind_zNonezIUastzIzKzK) goto case_243;
        {
          unit zgsz3282;
          zgsz3282 = print_endline("invalid instruction");
        }
        zgsz3280 = false;
        goto finish_match_241;
      }
    case_243: ;
      sail_match_failure("fetch_and_execute");
    finish_match_241: ;
      zloop_again = zgsz3280;
      KILL(zoptionzIUastzIzKzK)(&zgaz3336);
    }
    bool zgsz3284;
    if (zloop_again) {
      bool zgaz3335;
      zgaz3335 = cycle_limit_reached(UNIT);
      zgsz3284 = not(zgaz3335);
    } else {  zgsz3284 = false;  }
    zcbz334 = zgsz3284;
  } else {  zcbz334 = false;  }

end_function_244: ;
  return zcbz334;
end_block_exception_245: ;

  return false;
}

unit zmain(unit);

unit zmain(unit zgsz3285)
{
  __label__ end_function_249, end_block_exception_250;

  unit zcbz335;
  {
    CONVERT_OF(sail_int, mach_int)(&zinstr_count, INT64_C(0));
    unit zgsz3292;
    zgsz3292 = UNIT;
  }
  {
    uint64_t zgaz3339;
    {
      sail_int zgaz3338;
      CREATE(sail_int)(&zgaz3338);
      elf_entry(&zgaz3338, UNIT);
      {
        sail_int zgsz31185;
        CREATE(sail_int)(&zgsz31185);
        CONVERT_OF(sail_int, mach_int)(&zgsz31185, INT64_C(64));
        sail_int zgsz31186;
        CREATE(sail_int)(&zgsz31186);
        CONVERT_OF(sail_int, mach_int)(&zgsz31186, INT64_C(0));
        lbits zgsz31187;
        CREATE(lbits)(&zgsz31187);
        get_slice_int(&zgsz31187, zgsz31185, zgaz3338, zgsz31186);
        zgaz3339 = CONVERT_OF(fbits, lbits)(zgsz31187, true);
        KILL(lbits)(&zgsz31187);
        KILL(sail_int)(&zgsz31186);
        KILL(sail_int)(&zgsz31185);
      }
      KILL(sail_int)(&zgaz3338);
    }
    unit zgsz3291;
    zgsz3291 = zinit_registers(zgaz3339);
  }
  {
    __label__ while_247, wend_248;

    bool zgsz3286;
    unit zgsz3287;
  while_247: ;
    {
      {
        zgsz3286 = zfetch_and_execute(UNIT);
        if (have_exception) {


          goto end_block_exception_250;
        }
      }
      if (!(zgsz3286)) goto wend_248;
      zgsz3287 = UNIT;
      goto while_247;
    }
  wend_248: ;
    unit zgsz3290;
    zgsz3290 = UNIT;
  }
  {
    unit zgsz3289;
    zgsz3289 = print_endline("Machine Output");
  }
  {
    unit zgsz3288;
    zgsz3288 = zprint_register_dump(UNIT);
  }
  sail_string zgaz3340;
  CREATE(sail_string)(&zgaz3340);
  zconcat_str_dec(&zgaz3340, "Instruction Count: ", zinstr_count);
  zcbz335 = print_endline(zgaz3340);
  KILL(sail_string)(&zgaz3340);
end_function_249: ;
  return zcbz335;
end_block_exception_250: ;

  return UNIT;
}

unit zinitializze_registers(unit);

unit zinitializze_registers(unit zgsz3293)
{
  __label__ end_function_252, end_block_exception_253;

  unit zcbz336;
  {
    zPC = zundefined_Capability(UNIT);
    unit zgsz3298;
    zgsz3298 = UNIT;
  }
  {
    zundefined_word(&zR0, UNIT);
    unit zgsz3297;
    zgsz3297 = UNIT;
  }
  {
    zundefined_word(&zR1, UNIT);
    unit zgsz3296;
    zgsz3296 = UNIT;
  }
  {
    zundefined_word(&zR2, UNIT);
    unit zgsz3295;
    zgsz3295 = UNIT;
  }
  {
    zundefined_word(&zR3, UNIT);
    unit zgsz3294;
    zgsz3294 = UNIT;
  }
  undefined_int(&zinstr_count, UNIT);
  zcbz336 = UNIT;
end_function_252: ;
  return zcbz336;
end_block_exception_253: ;

  return UNIT;
}

void model_init(void)
{
  setup_rts();
  current_exception = sail_new(struct zexception);
  CREATE(zexception)(current_exception);
  throw_location = sail_new(sail_string);
  CREATE(sail_string)(throw_location);
  create_letbind_0();
  create_letbind_1();
  create_letbind_2();
  create_letbind_3();
  create_letbind_4();
  create_letbind_5();
  CREATE(zword)(&zR0);
  CREATE(zword)(&zR1);
  CREATE(zword)(&zR2);
  CREATE(zword)(&zR3);
  CREATE(sail_int)(&zinstr_count);
  zinitializze_registers(UNIT);
}

void model_fini(void)
{
  kill_letbind_5();
  kill_letbind_4();
  kill_letbind_3();
  kill_letbind_2();
  kill_letbind_1();
  kill_letbind_0();
  KILL(zword)(&zR0);
  KILL(zword)(&zR1);
  KILL(zword)(&zR2);
  KILL(zword)(&zR3);
  KILL(sail_int)(&zinstr_count);
  cleanup_rts();
  if (have_exception) {fprintf(stderr, "Exiting due to uncaught exception: %s\n", *throw_location);}
  KILL(zexception)(current_exception);
  sail_free(current_exception);
  KILL(sail_string)(throw_location);
  sail_free(throw_location);
  if (have_exception) {exit(EXIT_FAILURE);}
}

void model_pre_exit()
{
}

int model_main(int argc, char *argv[])
{
  model_init();
  if (process_arguments(argc, argv)) exit(EXIT_FAILURE);
  zmain(UNIT);
  model_fini();
  model_pre_exit();
  return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
  return model_main(argc, argv);
}

#ifdef __cplusplus
}
#endif
