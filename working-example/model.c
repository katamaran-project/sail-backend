#include "sail.h"
#include "rts.h"
#include "elf.h"
void (*sail_rts_set_coverage_file)(const char *) = NULL;
#ifdef __cplusplus
extern "C" {
#endif

// union exception
enum kind_zexception { Kind_z__dummy_exnz3 };

struct zexception {
  enum kind_zexception kind;
  union {struct { unit z__dummy_exnz3; };};
};

static void CREATE(zexception)(struct zexception *op) {
  op->kind = Kind_z__dummy_exnz3;
}

static void RECREATE(zexception)(struct zexception *op) {

}

static void KILL(zexception)(struct zexception *op) {
  {}
}

static void COPY(zexception)(struct zexception *rop, struct zexception op) {
  {};
  rop->kind = op.kind;
  if (op.kind == Kind_z__dummy_exnz3) {
    rop->z__dummy_exnz3 = op.z__dummy_exnz3;
  }
}

static bool EQUAL(zexception)(struct zexception op1, struct zexception op2) {
  if (op1.kind == Kind_z__dummy_exnz3 && op2.kind == Kind_z__dummy_exnz3) {
    return EQUAL(unit)(op1.z__dummy_exnz3, op2.z__dummy_exnz3);
  } else return false;
}

static void z__dummy_exnz3(struct zexception *rop, unit op) {
  {}
  rop->kind = Kind_z__dummy_exnz3;
  rop->z__dummy_exnz3 = op;
}

struct zexception *current_exception = NULL;
bool have_exception = false;
sail_string *throw_location = NULL;





// register reg
sail_int zreg;

unit zincrement(unit);

unit zincrement(unit zgsz30)
{
  __label__ end_function_1, end_block_exception_2;

  unit zcbz30;
  {
    sail_int zgsz33;
    CREATE(sail_int)(&zgsz33);
    CONVERT_OF(sail_int, mach_int)(&zgsz33, INT64_C(1));
    add_int(&zreg, zreg, zgsz33);
    KILL(sail_int)(&zgsz33);
  }
  zcbz30 = UNIT;
end_function_1: ;
  return zcbz30;
end_block_exception_2: ;

  return UNIT;
}

unit zmain(unit);

unit zmain(unit zgsz31)
{
  __label__ end_function_4, end_block_exception_5;

  unit zcbz31;
  zcbz31 = zincrement(UNIT);
end_function_4: ;
  return zcbz31;
end_block_exception_5: ;

  return UNIT;
}

unit zinitializze_registers(unit);

unit zinitializze_registers(unit zgsz32)
{
  __label__ end_function_7, end_block_exception_8;

  unit zcbz32;
  zcbz32 = UNIT;
end_function_7: ;
  return zcbz32;
end_block_exception_8: ;

  return UNIT;
}

void model_init(void)
{
  setup_rts();
  current_exception = sail_new(struct zexception);
  CREATE(zexception)(current_exception);
  throw_location = sail_new(sail_string);
  CREATE(sail_string)(throw_location);
  CREATE(sail_int)(&zreg);
  CONVERT_OF(sail_int, mach_int)(&zreg, INT64_C(0));
  zinitializze_registers(UNIT);
}

void model_fini(void)
{
  KILL(sail_int)(&zreg);
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
