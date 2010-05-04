/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Description: C source file of EuLisp module stream1
 **  Copyright: See file stream1.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_telos();
extern LispRef telos_bindings[];
extern LispRef boot1_bindings[];

/* Module bindings with size 41 */
LispRef stream1_bindings[41];

/* Foreign functions */
static LispRef ff_stub_eul_posix_codes4305 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT6(res,eul_posix_codes());
  return res;
}

static LispRef ff_stub_open4306 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004337, G004338, G004339, res;

  POPVAL1(G004339);
  POPVAL1(G004338);
  POPVAL1(G004337);
  FF_RES_CONVERT0(res,open(FF_ARG_CONVERT3(G004337), FF_ARG_CONVERT0(G004338), FF_ARG_CONVERT0(G004339)));
  return res;
}

static LispRef ff_stub_close4307 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004340, res;

  POPVAL1(G004340);
  FF_RES_CONVERT0(res,close(FF_ARG_CONVERT0(G004340)));
  return res;
}

static LispRef ff_stub_read4308 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004341, G004342, G004343, res;

  POPVAL1(G004343);
  POPVAL1(G004342);
  POPVAL1(G004341);
  FF_RES_CONVERT0(res,read(FF_ARG_CONVERT0(G004341), FF_ARG_CONVERT3(G004342), FF_ARG_CONVERT0(G004343)));
  return res;
}

static LispRef ff_stub_write4309 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004344, G004345, G004346, res;

  POPVAL1(G004346);
  POPVAL1(G004345);
  POPVAL1(G004344);
  FF_RES_CONVERT0(res,write(FF_ARG_CONVERT0(G004344), FF_ARG_CONVERT3(G004345), FF_ARG_CONVERT0(G004346)));
  return res;
}

static LispRef ff_stub_eul_sprintf4310 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004347, G004348, G004349, G004350, res;

  POPVAL1(G004350);
  POPVAL1(G004349);
  POPVAL1(G004348);
  POPVAL1(G004347);
  FF_RES_CONVERT0(res,eul_sprintf(FF_ARG_CONVERT3(G004347), FF_ARG_CONVERT0(G004348), FF_ARG_CONVERT3(G004349), FF_ARG_CONVERT8(G004350)));
  return res;
}

static LispRef ff_stub_eul_sprintf_string4311 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004351, G004352, G004353, G004354, G004355, G004356, res;

  POPVAL1(G004356);
  POPVAL1(G004355);
  POPVAL1(G004354);
  POPVAL1(G004353);
  POPVAL1(G004352);
  POPVAL1(G004351);
  FF_RES_CONVERT0(res,eul_sprintf_string(FF_ARG_CONVERT3(G004351), FF_ARG_CONVERT0(G004352), FF_ARG_CONVERT0(G004353), FF_ARG_CONVERT0(G004354), FF_ARG_CONVERT3(G004355), FF_ARG_CONVERT3(G004356)));
  return res;
}

static LispRef ff_stub_eul_make_socket4312 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004357, G004358, G004359, res;

  POPVAL1(G004359);
  POPVAL1(G004358);
  POPVAL1(G004357);
  FF_RES_CONVERT0(res,eul_make_socket(FF_ARG_CONVERT3(G004357), FF_ARG_CONVERT3(G004358), FF_ARG_CONVERT0(G004359)));
  return res;
}

static LispRef ff_stub_eul_socket_accept4313 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004360, res;

  POPVAL1(G004360);
  FF_RES_CONVERT0(res,eul_socket_accept(FF_ARG_CONVERT0(G004360)));
  return res;
}

static LispRef ff_stub_eul_make_connection4314 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004361, G004362, G004363, res;

  POPVAL1(G004363);
  POPVAL1(G004362);
  POPVAL1(G004361);
  FF_RES_CONVERT0(res,eul_make_connection(FF_ARG_CONVERT3(G004361), FF_ARG_CONVERT3(G004362), FF_ARG_CONVERT3(G004363)));
  return res;
}

static LispRef ff_stub_eul_socket_strerror4315 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004364, res;

  POPVAL1(G004364);
  FF_RES_CONVERT3(res,eul_socket_strerror(FF_ARG_CONVERT0(G004364)));
  return res;
}

static LispRef ff_stub_eul_strerror4316 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT3(res,eul_strerror());
  return res;
}

static LispRef ff_stub_ntok4317 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004365, G004366, res;

  POPVAL1(G004366);
  POPVAL1(G004365);
  FF_RES_CONVERT6(res,ntok(FF_ARG_CONVERT8(G004365), FF_ARG_CONVERT8(G004366)));
  return res;
}

static LispRef ff_stub_read_into_buffer4318 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef G004367, G004368, G004369, res;

  POPVAL1(G004369);
  POPVAL1(G004368);
  POPVAL1(G004367);
  FF_RES_CONVERT0(res,read_into_buffer(FF_ARG_CONVERT0(G004367), FF_ARG_CONVERT3(G004368), FF_ARG_CONVERT0(G004369)));
  return res;
}

static LispRef ff_stub_eul_hostname4319 (Stack *reg_value_stack, LispRef *sreg_value_sp, LispRef *sreg_value_sb)
{
  LispRef res;

  FF_RES_CONVERT3(res,eul_hostname());
  return res;
}


/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module stream1 */
void initialize_module_stream1()
{
  if (is_initialized) return;
  initialize_module_telos();
  eul_fast_table_set(eul_modules,"stream1",(LispRef) stream1_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_4336, sym_4335, sym_4334, G004333, G004331, G004329, sym_4327, sym_4326, sym_4325, sym_4324, sym_4323, sym_4322, G004321;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 83 is_init: 0 index: 35 binding: top-level */
  static const void *G004320[] = {I(a9,41,00,00),B(stream1 ,14),I(1b,89,00,00),B(stream1 ,9),I(2a,24,00,00),B(stream1 ,9),I(82,02,1b,89),B(stream1 ,4),I(2a,24,00,00),B(stream1 ,9),I(83,02,1b,89),B(stream1 ,12),I(2a,24,00,00),B(stream1 ,9),I(84,02,1b,89),B(stream1 ,2),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,03),I(02,1b,89,00),B(stream1 ,7),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,04),I(02,1b,89,00),B(stream1 ,10),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,05),I(02,1b,89,00),B(stream1 ,8),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,06),I(02,1b,89,00),B(stream1 ,13),I(2a,24,00,00),B(stream1 ,9),I(26,00,00,00),I(00,00,00,07),I(02,1b,89,00),B(stream1 ,11),I(2a,24,00,00),B(stream1 ,8),I(24,00,00,00),B(stream1 ,13),I(14,24,00,00),B(stream1 ,12),I(1c,14,24,00),B(stream1 ,12),I(24,00,00,00),B(stream1 ,7),I(14,24,00,00),B(stream1 ,2),I(24,00,00,00),B(stream1 ,7),I(14,23,00,00),B(stream1 ,29),I(24,00,00,00),B(stream1 ,4),I(23,00,00,00),B(stream1 ,30),I(1f,05,23,00),B(stream1 ,31),I(1f,06,23,00),B(stream1 ,32),I(24,00,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,33),I(24,00,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,34),I(1f,0b,24,00),B(boot1 ,26),I(3c,0c,1b,89),B(stream1 ,5),I(45,0e,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 36 binding: hostname */
  static const void *G004328[] = {I(a9,41,00,00),B(stream1 ,28),I(45,00,00,00)};

  /* Byte-vector with size: 3 is_init: 0 index: 37 binding: strerror */
  static const void *G004330[] = {I(a9,41,00,00),B(stream1 ,25),I(45,00,00,00)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-stream1 */
  static const void *G004332[] = {I(87,25,00,00),B(stream1 ,1),I(24,00,00,00),B(telos ,1),I(3e,0b,24,00),B(telos ,0),I(3c,00,21,01),I(86,25,00,00),B(stream1 ,13),I(86,25,00,00),B(stream1 ,12),I(86,25,00,00),B(stream1 ,11),I(86,25,00,00),B(stream1 ,10),I(86,25,00,00),B(stream1 ,9),I(86,25,00,00),B(stream1 ,8),I(86,25,00,00),B(stream1 ,7),I(23,00,00,00),B(stream1 ,38),I(23,00,00,00),B(stream1 ,37),I(3b,00,25,00),B(stream1 ,6),I(86,25,00,00),B(stream1 ,5),I(86,25,00,00),B(stream1 ,4),I(23,00,00,00),B(stream1 ,39),I(23,00,00,00),B(stream1 ,36),I(3b,00,25,00),B(stream1 ,3),I(86,25,00,00),B(stream1 ,2),I(23,00,00,00),B(stream1 ,40),I(23,00,00,00),B(stream1 ,35),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_intern_symbol(sym_4322,"r");
  eul_intern_symbol(sym_4323,"w");
  eul_intern_symbol(sym_4324,"a");
  eul_intern_symbol(sym_4325,"r+");
  eul_intern_symbol(sym_4326,"w+");
  eul_intern_symbol(sym_4327,"a+");
  eul_allocate_bytevector( G004321,G004320);
  eul_allocate_bytevector( G004329,G004328);
  eul_allocate_bytevector( G004331,G004330);
  eul_intern_symbol(sym_4334,"strerror");
  eul_intern_symbol(sym_4335,"hostname");
  eul_intern_symbol(sym_4336,"top-level");
  eul_allocate_bytevector( G004333,G004332);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 14; i++)
      stream1_bindings[i] = eul_nil;
  }

  stream1_bindings[ 14] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_posix_codes4305;
  stream1_bindings[ 15] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_open4306;
  stream1_bindings[ 16] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_close4307;
  stream1_bindings[ 17] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_read4308;
  stream1_bindings[ 18] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_write4309;
  stream1_bindings[ 19] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_sprintf4310;
  stream1_bindings[ 20] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_sprintf_string4311;
  stream1_bindings[ 21] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_make_socket4312;
  stream1_bindings[ 22] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_socket_accept4313;
  stream1_bindings[ 23] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_make_connection4314;
  stream1_bindings[ 24] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_socket_strerror4315;
  stream1_bindings[ 25] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_strerror4316;
  stream1_bindings[ 26] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_ntok4317;
  stream1_bindings[ 27] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_read_into_buffer4318;
  stream1_bindings[ 28] = (LispRef) (LispRef (*) (Stack *, LispRef *, LispRef *)) ff_stub_eul_hostname4319;
  stream1_bindings[ 29] = sym_4322;
  stream1_bindings[ 30] = sym_4323;
  stream1_bindings[ 31] = sym_4324;
  stream1_bindings[ 32] = sym_4325;
  stream1_bindings[ 33] = sym_4326;
  stream1_bindings[ 34] = sym_4327;
  stream1_bindings[ 35] = G004321;
  stream1_bindings[ 36] = G004329;
  stream1_bindings[ 37] = G004331;
  stream1_bindings[ 1] = eul_nil;
  stream1_bindings[ 38] = sym_4334;
  stream1_bindings[ 39] = sym_4335;
  stream1_bindings[ 40] = sym_4336;
  eul_allocate_lambda( stream1_bindings[0], "initialize-stream1", 0, G004333);

  }
}


/* eof */
