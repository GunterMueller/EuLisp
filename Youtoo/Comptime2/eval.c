/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module eval
 **  Copyright: See file eval.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern void initialize_module_i_args();
extern void initialize_module_i_compile();
extern void initialize_module_i_modify();
extern void initialize_module_cg_interf();
extern void initialize_module_i_rep();
extern LispRef i_modify_bindings[];
extern LispRef i_all_bindings[];
extern LispRef i_args_bindings[];
extern LispRef dynamic_bindings[];
extern LispRef mop_meth_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef mop_class_bindings[];
extern LispRef stream_bindings[];
extern LispRef stream2_bindings[];
extern LispRef i_error_bindings[];
extern LispRef boot_bindings[];
extern LispRef boot1_bindings[];
extern LispRef i_rep_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef i_compile_bindings[];
extern LispRef cg_interf_bindings[];
extern LispRef i_param_bindings[];

/* Module bindings with size 16 */
LispRef eval_bindings[16];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module eval */
void initialize_module_eval()
{
  if (is_initialized) return;
  initialize_module_i_all();
  initialize_module_i_args();
  initialize_module_i_compile();
  initialize_module_i_modify();
  initialize_module_cg_interf();
  initialize_module_i_rep();
  eul_fast_table_set(eul_modules,"eval",(LispRef) eval_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_9379, sym_9378, G009377, sym_9375, G009374, G009372, G009370, sym_9368, G009365, G009362;

  /* Code vector and literal definitions */
  eul_allocate_static_string(str_9363, "Module ~a need not be recompiled.", 33);
  /* Byte-vector with size: 17 is_init: 0 index: 5 binding: anonymous */
  static const void *G009361[] = {I(aa,1b,89,00),B(i_param ,23),I(2a,1b,24,00),B(cg_interf ,7),I(3c,01,1b,44),I(0c,1c,24,00),B(i_compile ,10),I(3c,01,36,13),I(23,00,00,00),B(eval ,4),I(1d,24,00,00),B(i_notify ,3),I(3c,02,2a,1c),I(24,00,00,00),B(i_compile ,3),I(3d,01,02,45),I(02,00,00,00)};

  eul_allocate_static_string(str_9366, "", 0);
  eul_allocate_static_string(str_9367, "Start EuLysses ...", 18);
  /* Byte-vector with size: 30 is_init: 0 index: 9 binding: start-eulysses */
  static const void *G009364[] = {I(a9,23,00,00),B(eval ,6),I(24,00,00,00),B(i_notify ,3),I(3c,01,2a,23),B(eval ,7),I(24,00,00,00),B(i_notify ,4),I(3c,01,2a,24),B(i_param ,10),I(44,18,24,00),B(i_rep ,6),I(3c,00,2a,82),I(24,00,00,00),B(boot1 ,20),I(3d,01,00,36),I(33,24,00,00),B(cg_interf ,4),I(3c,00,2a,23),B(eval ,8),I(23,00,00,00),B(eval ,5),I(3b,01,24,00),B(i_param ,33),I(24,00,00,00),B(boot ,17),I(3c,02,2a,24),B(i_error ,2),I(3d,00,00,45),I(00,00,00,00)};

  /* Byte-vector with size: 8 is_init: 0 index: 10 binding: (method-G009320) */
  static const void *G009369[] = {I(ab,24,00,00),B(stream2 ,9),I(1d,27,0a,24),B(stream ,8),I(3c,03,2a,85),I(24,00,00,00),B(i_error ,2),I(3d,01,02,00)};

  /* Byte-vector with size: 1 is_init: 0 index: 11 binding: (method-G009320) */
  static const void *G009371[] = {I(ab,86,45,02)};

  /* Byte-vector with size: 94 is_init: 0 index: 13 binding: main */
  static const void *G009373[] = {I(aa,84,24,00),B(mop_class ,22),I(24,00,00,00),B(mop_class ,22),I(24,00,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,24,00),B(boot1 ,26),I(3c,00,23,00),B(eval ,8),I(1f,03,24,00),B(mop_class ,16),I(24,00,00,00),B(mop_class ,63),I(1f,05,1f,05),I(24,00,00,00),B(mop_gf ,16),I(3c,06,1b,8a),I(03,02,84,86),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,03),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(eval ,12),I(23,00,00,00),B(eval ,11),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,06),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,07),I(8a,03,02,84),I(24,00,00,00),B(i_error ,5),I(86,24,00,00),B(boot1 ,40),I(3c,03,24,00),B(boot1 ,26),I(3c,00,1f,0a),I(8a,04,02,1c),I(1c,24,00,00),B(boot ,8),I(3c,02,1f,04),I(1f,04,23,00),B(eval ,12),I(23,00,00,00),B(eval ,10),I(3b,02,1f,03),I(24,00,00,00),B(mop_meth ,3),I(3c,04,1f,0d),I(1c,24,00,00),B(mop_meth ,5),I(3c,02,1f,0e),I(24,00,00,00),B(dynamic ,5),I(3c,01,2a,24),B(i_param ,63),I(3c,00,2a,1f),I(12,24,00,00),B(i_args ,4),I(3c,01,2a,24),B(i_compile ,7),I(3c,00,2a,24),B(i_param ,42),I(44,04,86,36),I(10,24,00,00),B(stream2 ,34),I(89,00,00,00),B(stream2 ,9),I(2a,24,00,00),B(eval ,2),I(3c,00,83,24),B(dynamic ,6),I(3c,01,2a,24),B(stream2 ,34),I(24,00,00,00),B(stream ,16),I(3c,01,2a,24),B(stream2 ,9),I(24,00,00,00),B(stream ,16),I(3d,01,14,45),I(14,00,00,00)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-eval */
  static const void *G009376[] = {I(87,25,00,00),B(eval ,1),I(24,00,00,00),B(i_rep ,1),I(3e,0b,24,00),B(i_rep ,0),I(3c,00,21,01),I(24,00,00,00),B(cg_interf ,1),I(3e,0b,24,00),B(cg_interf ,0),I(3c,00,21,01),I(24,00,00,00),B(i_modify ,1),I(3e,0b,24,00),B(i_modify ,0),I(3c,00,21,01),I(24,00,00,00),B(i_compile ,1),I(3e,0b,24,00),B(i_compile ,0),I(3c,00,21,01),I(24,00,00,00),B(i_args ,1),I(3e,0b,24,00),B(i_args ,0),I(3c,00,21,01),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(eval ,14),I(23,00,00,00),B(eval ,13),I(3b,01,25,00),B(eval ,3),I(23,00,00,00),B(eval ,15),I(23,00,00,00),B(eval ,9),I(3b,00,25,00),B(eval ,2),I(86,ac,00,00)};


  /* Initializations */
  object_class(str_9363) = eul_static_string_class;
  eul_allocate_bytevector( G009362,G009361);
  object_class(str_9366) = eul_static_string_class;
  object_class(str_9367) = eul_static_string_class;
  eul_intern_symbol(sym_9368,"anonymous");
  eul_allocate_bytevector( G009365,G009364);
  eul_allocate_bytevector( G009370,G009369);
  eul_allocate_bytevector( G009372,G009371);
  eul_intern_symbol(sym_9375,"(method G009320)");
  eul_allocate_bytevector( G009374,G009373);
  eul_intern_symbol(sym_9378,"main");
  eul_intern_symbol(sym_9379,"start-eulysses");
  eul_allocate_bytevector( G009377,G009376);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 4; i++)
      eval_bindings[i] = eul_nil;
  }

  eval_bindings[ 4] = str_9363;
  eval_bindings[ 5] = G009362;
  eval_bindings[ 6] = str_9366;
  eval_bindings[ 7] = str_9367;
  eval_bindings[ 8] = sym_9368;
  eval_bindings[ 9] = G009365;
  eval_bindings[ 10] = G009370;
  eval_bindings[ 11] = G009372;
  eval_bindings[ 12] = sym_9375;
  eval_bindings[ 13] = G009374;
  eval_bindings[ 1] = eul_nil;
  eval_bindings[ 14] = sym_9378;
  eval_bindings[ 15] = sym_9379;
  eul_allocate_lambda( eval_bindings[0], "initialize-eval", 0, G009377);

  }
}


/* eof */
