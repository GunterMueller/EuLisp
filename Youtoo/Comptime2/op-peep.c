/** ----------------------------------------------------------------------- **
 **                 Generated by EuLisp System 'youtoo'
 ** ----------------------------------------------------------------------- **
 **  Title: C source file of EuLisp module op-peep
 **  Copyright: See file op-peep.em
 ** ----------------------------------------------------------------------- **/

#include "eulisp.h"


/* Imported modules */
extern void initialize_module_i_all();
extern LispRef i_all_bindings[];
extern LispRef compare_bindings[];
extern LispRef boot1_bindings[];
extern LispRef i_param_bindings[];
extern LispRef table_bindings[];
extern LispRef list_bindings[];
extern LispRef i_notify_bindings[];
extern LispRef boot_bindings[];
extern LispRef mop_gf_bindings[];
extern LispRef table1_bindings[];

/* Module bindings with size 26 */
LispRef op_peep_bindings[26];

/* Foreign functions */

/* Initialize module only once */
static int is_initialized = 0;

/* Initialize module op-peep */
void initialize_module_op_peep()
{
  if (is_initialized) return;
  initialize_module_i_all();
  eul_fast_table_set(eul_modules,"op_peep",(LispRef) op_peep_bindings);
  is_initialized = 1;
  {
  /* Declarations */
  LispRef sym_5661, sym_5660, sym_5659, sym_5658, sym_5657, sym_5656, G005655, G005653, G005651, G005649, G005647, G005645, G005643, G005641, sym_5639, G005638, G005635, G005633;

  /* Code vector and literal definitions */
  /* Byte-vector with size: 7 is_init: 0 index: 8 binding: top-level */
  static const void *G005632[] = {I(a9,24,00,00),B(table1 ,3),I(24,00,00,00),B(mop_gf ,2),I(3c,01,1b,89),B(op_peep ,6),I(45,01,00,00)};

  eul_allocate_static_string(str_5636, "[~a => ~a]", 10);
  /* Byte-vector with size: 40 is_init: 0 index: 10 binding: anonymous */
  static const void *G005634[] = {I(aa,1b,12,1b),I(44,04,86,36),I(94,1c,10,1b),I(10,1c,73,1d),I(11,1b,73,1f),I(03,47,00,00),I(24,00,00,00),B(op_peep ,7),I(3c,02,1b,44),I(19,1f,03,44),I(12,1b,10,1f),I(04,1c,24,00),B(boot ,5),I(3c,02,22,01),I(36,02,87,36),I(02,86,1b,44),I(51,1c,10,1f),I(03,1c,24,00),B(boot ,5),I(3c,02,1f,08),I(11,1b,11,1b),I(11,1b,10,1f),I(04,1c,23,00),B(op_peep ,9),I(1f,0e,1f,03),I(24,00,00,00),B(i_notify ,4),I(3c,03,2a,47),I(00,00,1c,24),B(list ,3),I(3c,02,1d,1c),I(24,00,00,00),B(boot ,8),I(3c,02,47,00),I(01,1c,90,2a),I(87,22,0a,36),I(0a,1f,08,11),I(47,00,02,3d),I(01,09,22,07),I(45,02,00,00)};

  /* Byte-vector with size: 17 is_init: 0 index: 12 binding: apply-rule */
  static const void *G005637[] = {I(ab,46,03,1c),I(48,00,00,1b),I(48,00,01,86),I(1b,48,00,02),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,10),I(3b,01,48,00),I(02,47,00,00),I(10,1b,10,24),B(op_peep ,6),I(1c,24,00,00),B(table ,7),I(3c,02,47,00),I(02,3d,01,05),I(45,05,00,00)};

  /* Byte-vector with size: 13 is_init: 0 index: 13 binding: anonymous */
  static const void *G005640[] = {I(aa,1b,11,12),I(1b,44,06,47),I(00,00,36,27),I(1c,11,1b,1f),I(03,24,00,00),B(op_peep ,2),I(3c,02,1b,44),I(0b,1f,03,47),I(00,01,3d,01),I(04,36,0a,1f),I(03,11,47,00),I(01,3d,01,04),I(22,02,45,02)};

  /* Byte-vector with size: 16 is_init: 0 index: 14 binding: peep-hole-optimize */
  static const void *G005642[] = {I(aa,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,13),I(3b,01,48,00),I(01,24,00,00),B(i_param ,50),I(12,1b,44,06),I(47,00,00,36),I(0c,86,47,00),I(00,0f,47,00),I(01,3d,01,03),I(45,03,00,00)};

  /* Byte-vector with size: 20 is_init: 0 index: 15 binding: anonymous */
  static const void *G005644[] = {I(ab,1c,12,1b),I(44,10,47,00),I(00,11,24,00),B(boot1 ,26),I(3d,01,03,36),I(38,1d,10,1d),I(10,1c,7c,1b),I(44,0f,1f,05),I(11,1f,05,11),I(47,00,01,3d),I(02,06,36,1f),I(1d,1d,24,00),B(compare ,9),I(3c,02,1b,44),I(0f,1f,06,11),I(1f,06,11,47),I(00,01,3d,02),I(07,36,02,86),I(22,01,22,03),I(45,03,00,00)};

  /* Byte-vector with size: 16 is_init: 0 index: 16 binding: match-pattern */
  static const void *G005646[] = {I(ab,46,02,1b),I(48,00,00,86),I(1b,48,00,01),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,15),I(3b,02,48,00),I(01,1d,10,47),I(00,00,10,50),I(1b,44,10,1f),I(03,11,47,00),I(00,11,47,00),I(01,3d,02,04),I(36,02,86,45),I(04,00,00,00)};

  /* Byte-vector with size: 19 is_init: 0 index: 17 binding: add-rule */
  static const void *G005648[] = {I(43,04,1f,03),I(72,24,00,00),B(op_peep ,6),I(1c,24,00,00),B(table ,7),I(3c,02,24,00),B(table ,7),I(24,00,00,00),B(boot1 ,42),I(3c,01,1f,06),I(1f,06,1f,06),I(1f,06,24,00),B(boot1 ,26),I(3c,04,1b,1f),I(03,0f,24,00),B(op_peep ,6),I(1f,05,1d,1f),I(05,3c,03,2a),I(1f,08,45,09)};

  /* Byte-vector with size: 22 is_init: 0 index: 18 binding: anonymous */
  static const void *G005650[] = {I(43,03,1d,12),I(1b,44,0f,1c),I(24,00,00,00),B(boot1 ,26),I(3d,01,04,36),I(42,1d,12,1b),I(44,04,86,36),I(38,1f,04,10),I(1f,04,10,24),B(op_peep ,4),I(3c,02,1b,44),I(25,1f,05,11),I(1f,05,11,1d),I(10,1f,06,1c),I(24,00,00,00),B(boot ,8),I(3c,02,1f,03),I(1f,03,1d,47),I(00,00,3d,03),I(0a,22,04,36),I(02,86,22,01),I(22,01,45,04)};

  /* Byte-vector with size: 10 is_init: 0 index: 19 binding: match-rule */
  static const void *G005652[] = {I(ab,46,01,86),I(1b,48,00,00),I(23,00,00,00),B(op_peep ,11),I(23,00,00,00),B(op_peep ,18),I(3b,03,48,00),I(00,1d,1d,86),I(47,00,00,3d),I(03,03,45,03)};

  /* Byte-vector with size: 45 is_init: 1 index: 0 binding: initialize-op-peep */
  static const void *G005654[] = {I(87,25,00,00),B(op_peep ,1),I(24,00,00,00),B(i_all ,1),I(3e,0b,24,00),B(i_all ,0),I(3c,00,21,01),I(23,00,00,00),B(op_peep ,20),I(23,00,00,00),B(op_peep ,19),I(3b,02,25,00),B(op_peep ,7),I(86,25,00,00),B(op_peep ,6),I(23,00,00,00),B(op_peep ,21),I(23,00,00,00),B(op_peep ,17),I(3b,04,25,00),B(op_peep ,5),I(23,00,00,00),B(op_peep ,22),I(23,00,00,00),B(op_peep ,16),I(3b,02,25,00),B(op_peep ,4),I(23,00,00,00),B(op_peep ,23),I(23,00,00,00),B(op_peep ,14),I(3b,01,25,00),B(op_peep ,3),I(23,00,00,00),B(op_peep ,24),I(23,00,00,00),B(op_peep ,12),I(3b,02,25,00),B(op_peep ,2),I(23,00,00,00),B(op_peep ,25),I(23,00,00,00),B(op_peep ,8),I(3b,00,3d,00),I(00,ac,00,00)};


  /* Initializations */
  eul_allocate_bytevector( G005633,G005632);
  object_class(str_5636) = eul_static_string_class;
  eul_allocate_bytevector( G005635,G005634);
  eul_intern_symbol(sym_5639,"anonymous");
  eul_allocate_bytevector( G005638,G005637);
  eul_allocate_bytevector( G005641,G005640);
  eul_allocate_bytevector( G005643,G005642);
  eul_allocate_bytevector( G005645,G005644);
  eul_allocate_bytevector( G005647,G005646);
  eul_allocate_bytevector( G005649,G005648);
  eul_allocate_bytevector( G005651,G005650);
  eul_allocate_bytevector( G005653,G005652);
  eul_intern_symbol(sym_5656,"match-rule");
  eul_intern_symbol(sym_5657,"add-rule");
  eul_intern_symbol(sym_5658,"match-pattern");
  eul_intern_symbol(sym_5659,"peep-hole-optimize");
  eul_intern_symbol(sym_5660,"apply-rule");
  eul_intern_symbol(sym_5661,"top-level");
  eul_allocate_bytevector( G005655,G005654);

  /* Set local bindings */
  {
    int i;
    for (i = 2; i < 8; i++)
      op_peep_bindings[i] = eul_nil;
  }

  op_peep_bindings[ 8] = G005633;
  op_peep_bindings[ 9] = str_5636;
  op_peep_bindings[ 10] = G005635;
  op_peep_bindings[ 11] = sym_5639;
  op_peep_bindings[ 12] = G005638;
  op_peep_bindings[ 13] = G005641;
  op_peep_bindings[ 14] = G005643;
  op_peep_bindings[ 15] = G005645;
  op_peep_bindings[ 16] = G005647;
  op_peep_bindings[ 17] = G005649;
  op_peep_bindings[ 18] = G005651;
  op_peep_bindings[ 19] = G005653;
  op_peep_bindings[ 1] = eul_nil;
  op_peep_bindings[ 20] = sym_5656;
  op_peep_bindings[ 21] = sym_5657;
  op_peep_bindings[ 22] = sym_5658;
  op_peep_bindings[ 23] = sym_5659;
  op_peep_bindings[ 24] = sym_5660;
  op_peep_bindings[ 25] = sym_5661;
  eul_allocate_lambda( op_peep_bindings[0], "initialize-op-peep", 0, G005655);

  }
}


/* eof */
